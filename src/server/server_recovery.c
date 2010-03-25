/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <config.h>



#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_syslog.h>
#include <server_recovery.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_RECOVERY



int server_recovery(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id)
{
    enum Exception { PROTOCOL_ERROR
                     , JOB_SET_RAW_ERROR
                     , SERVER_RECOVERY_RESULT_ERROR
                     , GET_BLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_recovery\n"));
    TRY {
        struct srvr_rcvr_tbl_rec_s query, result;
        lixa_job_t query_job, result_job;

        if (LIXA_MSG_VERB_QRCVR != lmi->header.pvs.verb)
            THROW(PROTOCOL_ERROR);

        /* prepare the query object */
        if (LIXA_RC_OK != (ret_cod = lixa_job_set_raw(
                               &query_job,
                               (const char *)lmi->body.qrcvr_8.client.job)))
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
                                       ts, &result, lmi, lmo)))
                    THROW(SERVER_RECOVERY_RESULT_ERROR);
                break;
            case LIXA_RC_BYPASSED_OPERATION:
                /* @@@ implement thread transfer */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* @@@ answer "no available transactions" */
                break;
            default:
                THROW(GET_BLOCK_ERROR);
        } /* switch (rc) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case JOB_SET_RAW_ERROR:
                break;
            case SERVER_RECOVERY_RESULT_ERROR:
            case GET_BLOCK_ERROR:
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



int server_recovery_result(struct thread_status_s *ts,
                           const struct srvr_rcvr_tbl_rec_s *record,
                           const struct lixa_msg_s *lmi,
                           struct lixa_msg_s *lmo)
{
    enum Exception { XML_CHAR_STRDUP_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_recovery_result\n"));
    TRY {
        guint i;
        uint32_t block_id = record->block_id;
        struct status_record_data_payload_s *pld =
            &(ts->curr_status[block_id].sr.data.pld);

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
            char *ser_xid = xid_serialize(&pld->ph.state.xid);
            
            lmo->body.qrcvr_16.answer.rc = LIXA_RC_LIXAC_CONF_CHANGED;
            syslog(LOG_WARNING, LIXA_SYSLOG_LXD011W,
                   lmo->body.qrcvr_16.client.job, ser_xid ? ser_xid : "");
            LIXA_TRACE(("server_recovery_result: job is '%s', past config "
                        "digest is '%s', current config digest is '%s'\n",
                        lmo->body.qrcvr_16.client.job,
                        lmo->body.qrcvr_16.client.config_digest,
                        lmi->body.qrcvr_8.client.config_digest));
            if (ser_xid) free(ser_xid);
        }
        
        lmo->body.qrcvr_16.client.last_verb_step.verb =
            pld->ph.last_verb_step[0].verb;
        lmo->body.qrcvr_16.client.last_verb_step.step =
            pld->ph.last_verb_step[0].step;

        lmo->body.qrcvr_16.client.state.finished = pld->ph.state.finished;
        lmo->body.qrcvr_16.client.state.txstate = pld->ph.state.txstate;
        lmo->body.qrcvr_16.client.state.will_commit = pld->ph.state.will_commit;
        lmo->body.qrcvr_16.client.state.will_rollback =
            pld->ph.state.will_rollback;
        memcpy(&lmo->body.qrcvr_16.client.state.xid, &pld->ph.state.xid,
               sizeof(XID));

        lmo->body.qrcvr_16.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_qrcvr_16_rsrmgr_s),
            pld->ph.n);
        for (i=0; i<pld->ph.n; ++i) {
            struct lixa_msg_body_qrcvr_16_rsrmgr_s rsrmgr;
            status_record_t *sr = ts->curr_status +
                ts->curr_status[block_id].sr.data.pld.ph.block_array[i]; 
            rsrmgr.rmid = sr->sr.data.pld.rm.rmid;
            rsrmgr.next_verb = sr->sr.data.pld.rm.state.next_verb;
            rsrmgr.r_state = sr->sr.data.pld.rm.state.xa_r_state;
            rsrmgr.s_state = sr->sr.data.pld.rm.state.xa_s_state;
            rsrmgr.t_state = sr->sr.data.pld.rm.state.xa_t_state;
            g_array_append_val(lmo->body.qrcvr_16.rsrmgrs, rsrmgr);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
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

