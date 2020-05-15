/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
# include <time.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include "lixa_errors.h"
#include "lixa_crash.h"
#include "lixa_trace.h"
#include "lixa_syslog.h"
#include "lixa_xid.h"
#include "server_thread_status.h"
#include "server_recovery.h"
#include "server_xa_branch.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_RECOVERY



int server_recovery(struct thread_status_s *ts,
                    size_t slot_id,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id,
                    struct server_client_status_s *cs)
{
    enum Exception { SERVER_RECOVERY_8_ERROR
                     , SERVER_RECOVERY_24_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_recovery\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (
                        ret_cod = server_recovery_8(
                            ts, slot_id, lmi, lmo, block_id, cs)))
                    THROW(SERVER_RECOVERY_8_ERROR);
                break;
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_recovery_24(
                                       ts, lmi, block_id, cs)))
                    THROW(SERVER_RECOVERY_24_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_RECOVERY_8_ERROR:
            case SERVER_RECOVERY_24_ERROR:
                break;
            case INVALID_STEP:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_recovery_8(struct thread_status_s *ts,
                      size_t slot_id,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id,
                      struct server_client_status_s *cs)
{
    enum Exception { PROTOCOL_ERROR,
                     JOB_SET_RAW_ERROR,
                     SERVER_RECOVERY_RESULT_ERROR,
                     THREAD_SWITCH,
                     RECOVERY_RESULT_EMPTY_ERROR,
                     GET_BLOCK_ERROR,
                     FSM_SEND_MESSAGE_AND_WAIT,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_recovery_8\n"));
    TRY {
        struct srvr_rcvr_tbl_rec_s query, result;
        lixa_job_t query_job, result_job;
        struct thread_status_switch_s *tss =
            &(ts->client_array[slot_id].switch_thread);

        if (LIXA_MSG_VERB_QRCVR != lmi->header.pvs.verb)
            THROW(PROTOCOL_ERROR);

        /* prepare the query object */
        if (LIXA_RC_OK != (ret_cod = lixa_job_set_raw(
                               &query_job,
                               (const char *) lmi->body.qrcvr_8.client.job)))
            THROW(JOB_SET_RAW_ERROR);
        query.job = &query_job;
        query.tsid = ts->id;
        result.job = &result_job; /* reserve room for job object */

        /* query the recovery table */
        ret_cod = srvr_rcvr_tbl_get_block(
            ts->recovery_table, &query, &result, FALSE);
        switch (ret_cod) {
            case LIXA_RC_OK:
                if (LIXA_RC_OK != (ret_cod = server_recovery_result(
                                       ts, &result, lmi, lmo, block_id)))
                    THROW(SERVER_RECOVERY_RESULT_ERROR);
                break;
            case LIXA_RC_BYPASSED_OPERATION:
                tss->id = result.tsid;
                LIXA_TRACE(("server_recovery_8: this client must be switched "
                            "to thread %d to perform recovery\n",
                            tss->id));
                THROW(THREAD_SWITCH);
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                if (LIXA_RC_OK != (ret_cod = server_recovery_empty_result(
                                       ts, &result, lmi, lmo)))
                    THROW(RECOVERY_RESULT_EMPTY_ERROR);
                break;
            default:
                THROW(GET_BLOCK_ERROR);
        } /* switch (rc) */

        /* prepare next protocol step */
        cs->last_verb_step.verb = lmo->header.pvs.verb;
        cs->last_verb_step.step = lmo->header.pvs.step;
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_send_message_and_wait(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_SEND_MESSAGE_AND_WAIT);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case JOB_SET_RAW_ERROR:
                break;
            case SERVER_RECOVERY_RESULT_ERROR:
                break;
            case THREAD_SWITCH:
                ret_cod = LIXA_RC_THREAD_SWITCH;
                break;
            case RECOVERY_RESULT_EMPTY_ERROR:
            case GET_BLOCK_ERROR:
            case FSM_SEND_MESSAGE_AND_WAIT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    
#ifdef _CRASH
    if (THREAD_SWITCH != excp) {
        LIXA_CRASH(LIXA_CRASH_POINT_SERVER_RECOVERY_8,
                   thread_status_get_crash_count(ts));
    }
#endif
    return ret_cod;
}



int server_recovery_24(struct thread_status_s *ts,
                       const struct lixa_msg_s *lmi,
                       uint32_t block_id,
                       struct server_client_status_s *cs)
{
    enum Exception {
        PAYLOAD_CHAIN_RELEASE,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        GETTIMEOFDAY_ERROR,
        INVALID_STATUS,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_recovery_24\n"));
    TRY {
        uint32_t recovering_block_id =
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.recovering_block_id;
        if (!lmi->body.qrcvr_24.recovery.failed) {
            struct payload_header_s *ph =
                &thread_status_get_record4update(ts, block_id)->data.pld.ph;
            /* avoid this transaction might be kept in recovery pending table
               if the client crashed */
            ph->state.finished = TRUE;
            LIXA_TRACE(("server_recovery_24: client completed the recovery "
                        "phase successfully; release block # "
                        UINT32_T_FORMAT
                        " and its chain\n", recovering_block_id));
            if (LIXA_RC_OK != (ret_cod = payload_chain_release(
                                   ts, recovering_block_id)))
                THROW(PAYLOAD_CHAIN_RELEASE);
        } else {
            struct payload_header_s *ph =
                &thread_status_get_record4update(
                    ts, recovering_block_id)->data.pld.ph;
            int i;

            LIXA_TRACE(("server_recovery_24: client did not complete the "
                        "recovery phase successfully; keeping block # "
                        UINT32_T_FORMAT
                        " and its chain\n", recovering_block_id));
            ph->recovery_failed = TRUE;
            ph->recovery_commit = lmi->body.qrcvr_24.recovery.commit;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(
                                   ts, recovering_block_id)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
            if (0 != gettimeofday(&ph->recovery_failed_time, NULL))
                THROW(GETTIMEOFDAY_ERROR);
            if (lmi->body.qrcvr_24.rsrmgrs->len != ph->n)
                THROW(INVALID_STATUS);
            for (i = 0; i < ph->n; ++i) {
                struct lixa_msg_body_qrcvr_24_rsrmgr_s *rsrmgr;
                rsrmgr = &g_array_index(lmi->body.qrcvr_24.rsrmgrs,
                                        struct lixa_msg_body_qrcvr_24_rsrmgr_s,
                                        i);
                thread_status_get_record4update(
                    ts, ph->block_array[
                        rsrmgr->rmid])->data.pld.rm.recovery_rc = rsrmgr->rc;
                if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(
                                       ts, ph->block_array[rsrmgr->rmid])))
                    THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
                LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD012W, ts->id,
                             recovering_block_id));
            }
        }
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PAYLOAD_CHAIN_RELEASE:
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR2:
            case FSM_WANT_MESSAGE:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_RECOVERY_24,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_recovery_result(struct thread_status_s *ts,
                           const struct srvr_rcvr_tbl_rec_s *record,
                           const struct lixa_msg_s *lmi,
                           struct lixa_msg_s *lmo,
                           uint32_t client_block_id)
{
    enum Exception {
        THREAD_STATUS_MARK_BLOCK_ERROR,
        XML_CHAR_STRDUP_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_recovery_result\n"));
    TRY {
        guint i;
        uint32_t block_id = record->block_id;
        const struct status_record_data_payload_s *pld =
            &(thread_status_get_record4read(ts, block_id)->data.pld);

        /* register the block is in recovery phase */
        thread_status_get_record4update(
            ts, client_block_id)->data.pld.ph.recovering_block_id = block_id;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(
                               ts, client_block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR);

        /* set basic answer information */
        lmo->header.pvs.verb = LIXA_MSG_VERB_QRCVR;
        lmo->body.qrcvr_16.answer.rc = LIXA_RC_OK;

        /* this duplication is not necessary, but it helps in keeping a clean
           code: no special behaviour in @ref lixa_msg_free */
        if (NULL == (lmo->body.qrcvr_16.client.job =
                     xmlCharStrdup(lixa_job_get_raw(&pld->ph.job))))
            THROW(XML_CHAR_STRDUP_ERROR);
        memcpy(&lmo->body.qrcvr_16.client.config_digest,
               &pld->ph.config_digest, sizeof(md5_digest_hex_t));

        /* check config digest: current and past */
        if (memcmp(lmi->body.qrcvr_8.client.config_digest,
                   lmo->body.qrcvr_16.client.config_digest,
                   sizeof(md5_digest_hex_t))) {
            lixa_ser_xid_t ser_xid = "";
            lixa_xid_serialize(&pld->ph.state.xid, ser_xid);

            lmo->body.qrcvr_16.answer.rc = LIXA_RC_LIXAC_CONF_CHANGED;
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD011W,
                         lmo->body.qrcvr_16.client.job, ser_xid));
            LIXA_TRACE(("server_recovery_result: job is '%s', past config "
                        "digest is '%s', current config digest is '%s'\n",
                        lmo->body.qrcvr_16.client.job,
                        lmo->body.qrcvr_16.client.config_digest,
                        lmi->body.qrcvr_8.client.config_digest));
        }

        /* check if the block is part of chain (multiple branches
           transaction) */
        LIXA_TRACE(("server_recovery_result: block_id=" UINT32_T_FORMAT "\n",
                    block_id));
        if (server_xa_branch_is_chained(ts, block_id)) {
            LIXA_TRACE(("server_recovery_result: block_id=" UINT32_T_FORMAT
                        " is part of a multiple branch transaction\n",
                        block_id));
            
        } /* if (server_xa_branch_is_chained(ts, block_id)) */
        
        lmo->body.qrcvr_16.client.last_verb_step.verb =
            pld->ph.last_verb_step[0].verb;
        lmo->body.qrcvr_16.client.last_verb_step.step =
            pld->ph.last_verb_step[0].step;

        lmo->body.qrcvr_16.client.state.finished = pld->ph.state.finished;
        lmo->body.qrcvr_16.client.state.txstate = pld->ph.state.txstate;
        lmo->body.qrcvr_16.client.state.will_commit =
            pld->ph.state.will_commit;
        lmo->body.qrcvr_16.client.state.will_rollback =
            pld->ph.state.will_rollback;
        lmo->body.qrcvr_16.client.state.global_recovery =
            pld->ph.state.global_recovery;
        memcpy(&lmo->body.qrcvr_16.client.state.xid, &pld->ph.state.xid,
               sizeof(XID));

        lmo->body.qrcvr_16.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_qrcvr_16_rsrmgr_s),
            pld->ph.n);
        for (i = 0; i < pld->ph.n; ++i) {
            struct lixa_msg_body_qrcvr_16_rsrmgr_s rsrmgr;
            const struct payload_rsrmgr_s *rm =
                &(thread_status_get_record4read(
                      ts,
                      thread_status_get_record4read(
                          ts, block_id)->data.pld.ph.block_array[i]
                                                )->data.pld.rm);
            rsrmgr.rmid = rm->rmid;
            rsrmgr.next_verb = rm->state.next_verb;
            rsrmgr.r_state = rm->state.xa_r_state;
            rsrmgr.s_state = rm->state.xa_s_state;
            rsrmgr.td_state = rm->state.xa_td_state;
            g_array_append_val(lmo->body.qrcvr_16.rsrmgrs, rsrmgr);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case THREAD_STATUS_MARK_BLOCK_ERROR:
                break;
            case XML_CHAR_STRDUP_ERROR:
                ret_cod = LIXA_RC_XML_CHAR_STRDUP_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery_result/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_recovery_empty_result(struct thread_status_s *ts,
                                 const struct srvr_rcvr_tbl_rec_s *record,
                                 const struct lixa_msg_s *lmi,
                                 struct lixa_msg_s *lmo)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_recovery_empty_result\n"));
    TRY {
        lmo->header.pvs.verb = LIXA_MSG_VERB_QRCVR;

        lmo->body.qrcvr_16.answer.rc = LIXA_RC_OBJ_NOT_FOUND;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery_empty_result/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


