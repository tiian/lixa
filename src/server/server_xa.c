/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include "lixa_errors.h"
#include "lixa_crash.h"
#include "lixa_utils.h"
#include "lixa_syslog.h"
#include "server_thread_status.h"
#include "server_fsm.h"
#include "server_xa.h"
#include "server_xa_branch.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_XA



int server_ax_reg(struct thread_status_s *ts,
                  size_t slot_id,
                  const struct lixa_msg_s *lmi,
                  uint32_t block_id)
{
    enum Exception {
        INVALID_STEP,
        INVALID_BLOCK_ID,
        RMID_OUT_OF_RANGE,
        THREAD_STATUS_MARK_BLOCK_ERROR,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_ax_reg\n"));
    TRY {
        uint32_t slot;
        const struct lixa_msg_body_reg_8_ax_reg_exec_s *ax_reg_exec =
            &lmi->body.reg_8.ax_reg_exec;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);
        struct status_record_data_payload_s *pld = NULL;

        /* check message step */
        if (8 != lmi->header.pvs.step)
            THROW(INVALID_STEP);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check rmid */
        if (ax_reg_exec->rmid < 0 ||
            ax_reg_exec->rmid >=
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(RMID_OUT_OF_RANGE);

        /* reset finished state */
        thread_status_get_record4update(ts, block_id
                                        )->data.pld.ph.state.finished = FALSE;
        slot = thread_status_get_record4read(ts, block_id
                                             )->data.pld.ph.block_array[
                                                 ax_reg_exec->rmid];
        /* mark the slot as updated */
        pld = &(thread_status_get_record4update(ts, slot)->data.pld);
        pld->rm.state.xa_td_state = ax_reg_exec->td_state;
        pld->rm.state.xa_s_state = ax_reg_exec->s_state;
        pld->rm.ax_reg_flags = ax_reg_exec->flags;
        pld->rm.ax_reg_rc = ax_reg_exec->rc;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR);        
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case RMID_OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR:
            case FSM_WANT_MESSAGE:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_ax_reg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));    
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_AX_REG,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_ax_unreg(struct thread_status_s *ts,
                    size_t slot_id,
                    const struct lixa_msg_s *lmi,
                    uint32_t block_id)
{
    enum Exception {
        INVALID_STEP,
        INVALID_BLOCK_ID,
        RMID_OUT_OF_RANGE,
        THREAD_STATUS_MARK_BLOCK_ERROR,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_ax_unreg\n"));
    TRY {
        uint32_t slot;
        const struct lixa_msg_body_unreg_8_ax_unreg_exec_s *ax_unreg_exec =
            &lmi->body.unreg_8.ax_unreg_exec;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);
        struct status_record_data_payload_s *pld = NULL;

        /* check message step */
        if (8 != lmi->header.pvs.step)
            THROW(INVALID_STEP);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check rmid */
        if (ax_unreg_exec->rmid < 0 ||
            ax_unreg_exec->rmid >=
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(RMID_OUT_OF_RANGE);

        slot = thread_status_get_record4read(ts, block_id
                                             )->data.pld.ph.block_array[
                                                 ax_unreg_exec->rmid];
        /* mark the slot as updated */
        pld = &(thread_status_get_record4update(ts, slot)->data.pld);
        pld->rm.state.xa_td_state = ax_unreg_exec->td_state;
        pld->rm.ax_unreg_flags = ax_unreg_exec->flags;
        pld->rm.ax_unreg_rc = ax_unreg_exec->rc;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR);
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case RMID_OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR:
            case FSM_WANT_MESSAGE:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_ax_unreg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_AX_UNREG,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_close(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    uint32_t block_id)
{
    enum Exception { BRANCH_UNCHAIN
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_close\n"));
    TRY {
        /* in the event of multiple branch, unchain this one */
        if (server_xa_branch_is_chained(ts, block_id)) {
            if (LIXA_RC_OK != (ret_cod =
                               server_xa_branch_unchain(ts, block_id)))
                THROW(BRANCH_UNCHAIN);
        } /* if (server_xa_branch_is_chained(ts, block_id)) */
        /* nothing to do, but a simple trace... ?! */
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BRANCH_UNCHAIN:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_commit(struct thread_status_s *ts,
                     size_t slot_id,
                     const struct lixa_msg_s *lmi,
                     uint32_t block_id)
{
    enum Exception { SERVER_XA_COMMIT_8_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_commit\n"));
    TRY {
        if (8 != lmi->header.pvs.step) {
            THROW(INVALID_STEP);
        } else if (LIXA_RC_OK != (ret_cod = server_xa_commit_8(
                                      ts, slot_id, lmi, block_id)))
            THROW(SERVER_XA_COMMIT_8_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_COMMIT_8_ERROR:
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
    LIXA_TRACE(("server_xa_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_commit_8(struct thread_status_s *ts,
                       size_t slot_id,
                       const struct lixa_msg_s *lmi,
                       uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        XID_SERIALIZE_ERROR,
        TRANS_TABLE_REMOVE_ERROR,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct server_trans_tbl_qry_s sttq;
    sttq.gtrid = NULL;

    LIXA_TRACE(("server_xa_commit_8\n"));
    TRY {
        uint32_t i;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.commit_8.xa_commit_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store commit/rollback intent after commit phase */
        thread_status_get_record4update(ts, block_id
                                        )->data.pld.ph.state.finished =
            lmi->body.commit_8.conthr.finished;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        /* remove XID from the global transaction table */
        sttq.gtrid = lixa_xid_get_gtrid_ascii(
            &(thread_status_get_record4read(ts, block_id
                                            )->data.pld.ph.state.xid));
        if (!lixa_xid_serialize(
                &(thread_status_get_record4read(ts, block_id
                                                )->data.pld.ph.state.xid),
                sttq.xid))
            THROW(XID_SERIALIZE_ERROR);
        sttq.tsid = ts->id;
        if (LIXA_RC_OK != (ret_cod = server_trans_tbl_remove(
                               ts->trans_table, &sttq)))
            THROW(TRANS_TABLE_REMOVE_ERROR);
        
        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.commit_8.xa_commit_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_commit_8_xa_commit_execs_s *xa_commit_execs;
            uint32_t slot;
            xa_commit_execs = &g_array_index(
                lmi->body.commit_8.xa_commit_execs,
                struct lixa_msg_body_commit_8_xa_commit_execs_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[xa_commit_execs->rmid];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.xa_r_state = xa_commit_execs->r_state;
            pld->rm.state.xa_s_state = xa_commit_execs->s_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            pld->rm.xa_commit_flags = xa_commit_execs->flags;
            pld->rm.xa_commit_rc = xa_commit_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
                break;
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case TRANS_TABLE_REMOVE_ERROR:
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
    /* release gtrid */
    if (NULL != sttq.gtrid) {
        free(sttq.gtrid);
        sttq.gtrid = NULL;
    }
    LIXA_TRACE(("server_xa_commit_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_COMMIT_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_end(struct thread_status_s *ts,
                  size_t slot_id,
                  const struct lixa_msg_s *lmi,
                  struct lixa_msg_s *lmo,
                  uint32_t block_id)
{
    enum Exception { SERVER_XA_END_8_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_end\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_end_8(
                            ts, slot_id, lmi, lmo, block_id)))
                    THROW(SERVER_XA_END_8_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (lmi->header.pvs.step) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_END_8_ERROR:
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
    LIXA_TRACE(("server_xa_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_end_8(struct thread_status_s *ts,
                    size_t slot_id,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        THREAD_STATUS_MARK_BLOCK_ERROR3,
        FSM_SEND_MESSAGE_AND_WAIT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_end_8\n"));
    TRY {
        uint32_t i;
        int multiple_branches = FALSE;
        int multiple_branches_rollback = FALSE;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.end_8.xa_end_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* check if the block is a branch inside a multiple branches
           transaction */
        if (server_xa_branch_is_chained(ts, block_id)) {
            LIXA_TRACE(("server_xa_end_8: this branch is part of a "
                        "multiple branches transaction\n"));
            multiple_branches = TRUE;
        }
        /* store commit/rollback intent */
        if (lmi->body.end_8.conthr.commit) {
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.state.will_commit = TRUE;
        } else
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.state.will_rollback = TRUE;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        /* the status file must be synchronized */
        status_sync_ask_sync(&ts->status_sync);

        /* store next_verb for all the resource managers */
        for (i=0; i<thread_status_get_record4read(ts, block_id)->data.pld.ph.n;
             ++i) {
            struct status_record_data_payload_s *pld = NULL;
            uint32_t slot;
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[i];
            LIXA_TRACE(("server_xa_end_8: updating next_verb for resource "
                        "manager # " UINT32_T_FORMAT " (" UINT32_T_FORMAT ") "
                        "\n", i, slot));
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.next_verb = LIXA_MSG_VERB_END;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */

        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.end_8.xa_end_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_end_8_xa_end_execs_s *xa_end_execs;
            uint32_t slot;
            xa_end_execs = &g_array_index(
                lmi->body.end_8.xa_end_execs,
                struct lixa_msg_body_end_8_xa_end_execs_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[xa_end_execs->rmid];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.xa_s_state = xa_end_execs->s_state;
            pld->rm.state.xa_td_state = xa_end_execs->td_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            pld->rm.xa_end_flags = xa_end_execs->flags;
            pld->rm.xa_end_rc = xa_end_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR3);
            /* check return code and flags in the event of XTA multiple branch
               distributed transaction */
            if (multiple_branches &&
                (TMFAIL & xa_end_execs->flags || XA_OK != xa_end_execs->rc)) {
                LIXA_TRACE(("server_xa_end_8: resource manager with rmid="
                            UINT32_T_FORMAT " is triggering rollback for "
                            "a multiple branches transaction "
                            "(flags=0x%x,rc=%d)\n", i, xa_end_execs->flags,
                            xa_end_execs->rc));
                multiple_branches_rollback = TRUE;
            }
        } /* for (i=0; ... */

        /* send KO to all the waiting branches */
        if (multiple_branches_rollback)
            cs->branch_join = CLIENT_BRANCH_JOIN_KO;
        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
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
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
            case THREAD_STATUS_MARK_BLOCK_ERROR2:
            case THREAD_STATUS_MARK_BLOCK_ERROR3:
            case FSM_SEND_MESSAGE_AND_WAIT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_end_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_END_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_forget(struct thread_status_s *ts,
                     size_t slot_id,
                     const struct lixa_msg_s *lmi,
                     uint32_t block_id)
{
    enum Exception { SERVER_XA_FORGET_8_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_forget\n"));
    TRY {
        if (8 != lmi->header.pvs.step) {
            THROW(INVALID_STEP);
        } else if (LIXA_RC_OK != (ret_cod = server_xa_forget_8(
                                      ts, slot_id, lmi, block_id)))
            THROW(SERVER_XA_FORGET_8_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_FORGET_8_ERROR:
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
    LIXA_TRACE(("server_xa_forget/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_forget_8(struct thread_status_s *ts,
                       size_t slot_id,
                       const struct lixa_msg_s *lmi,
                       uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_forget_8\n"));
    TRY {
        uint32_t i;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.forget_8.xa_forget_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store transaction finished status */
        thread_status_get_record4update(
            ts, block_id)->data.pld.ph.state.finished =
            lmi->body.forget_8.conthr.finished;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);

        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.forget_8.xa_forget_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_forget_8_xa_forget_execs_s *xa_forget_execs;
            uint32_t slot;
            xa_forget_execs = &g_array_index(
                lmi->body.forget_8.xa_forget_execs,
                struct lixa_msg_body_forget_8_xa_forget_execs_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[xa_forget_execs->rmid];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.xa_s_state = xa_forget_execs->s_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            pld->rm.xa_forget_flags = xa_forget_execs->flags;
            pld->rm.xa_forget_rc = xa_forget_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
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
    LIXA_TRACE(("server_xa_forget_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_FORGET_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_open(struct thread_status_s *ts,
                   size_t slot_id,
                   const struct lixa_msg_s *lmi,
                   struct lixa_msg_s *lmo,
                   uint32_t block_id)
{
    enum Exception { SERVER_XA_OPEN_8_ERROR
                     , SERVER_XA_OPEN_24_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_open\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_open_8(
                            ts, slot_id, lmi, lmo, block_id)))
                    THROW(SERVER_XA_OPEN_8_ERROR);
                break;
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_xa_open_24(
                                       ts, slot_id, lmi, block_id)))
                    THROW(SERVER_XA_OPEN_24_ERROR);
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_OPEN_8_ERROR:
            case SERVER_XA_OPEN_24_ERROR:
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
    LIXA_TRACE(("server_xa_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_open_8(struct thread_status_s *ts,
                     size_t slot_id,
                     const struct lixa_msg_s *lmi,
                     struct lixa_msg_s *lmo,
                     uint32_t block_id)
{
    enum Exception { RSRMGRS_ARRAY_NULL
                     , MAINTENANCE_MODE
                     , TOO_MANY_RSRMGRS
                     , PAYLOAD_CHAIN_ALLOCATE_ERROR
                     , FSM_SEND_MESSAGE_AND_WAIT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_open_8\n"));
    TRY {
        uint32_t i;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* replace session id */
        LIXA_TRACE(("server_xa_open_8: server generated sessid='%s' ...\n",
                    lixa_session_get_sid(&(cs->session))));
        memcpy(&(cs->session), &(lmi->body.open_8.client.session),
               sizeof(lixa_session_t));
        LIXA_TRACE(("server_xa_open_8: force sessid='%s' using client value\n",
                    lixa_session_get_sid(&(cs->session))));
        
        /* check the resource manager array is OK */
        if (NULL == lmi->body.open_8.rsrmgrs)
            THROW(RSRMGRS_ARRAY_NULL);

        if (ts->mmode && !lmi->body.open_8.client.maint) {
            /* server in maintenance mode, client not in maintenance mode */
            LIXA_TRACE(("server_xa_open_8: connected client is not operating "
                        "in maintenance mode while server was started in "
                        "maintenance mode; closing client connection...\n"));
            /* prepare output message */
            lmo->header.pvs.verb = lmi->header.pvs.verb;
            /* prepare next protocol step */
            cs->last_verb_step.verb = lmo->header.pvs.verb;
            cs->last_verb_step.step = lmo->header.pvs.step;
            THROW(MAINTENANCE_MODE);
        }

        if (lmi->body.open_8.rsrmgrs->len > CHAIN_MAX_SIZE) {
            LIXA_TRACE(("server_xa_open_8: message arrived from client "
                        "would use %u (max is %u) child blocks\n",
                        lmi->body.open_8.rsrmgrs->len,
                        CHAIN_MAX_SIZE));
            THROW(TOO_MANY_RSRMGRS);
        }

        if (LIXA_RC_OK != (ret_cod = payload_chain_allocate(
                               ts, block_id,
                               lmi->body.open_8.rsrmgrs->len))) THROW(
                                   PAYLOAD_CHAIN_ALLOCATE_ERROR);

        /* retrieve header information */
        strncpy(thread_status_get_record4update(
                    ts, block_id)->data.pld.ph.config_digest,
                lmi->body.open_8.client.config_digest,
                sizeof(md5_digest_hex_t));
        lixa_job_set_raw(&(thread_status_get_record4update(
                               ts, block_id)->data.pld.ph.job),
                         (char *) lmi->body.open_8.client.job);
        for (i = 0; i < lmi->body.open_8.rsrmgrs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr;
            uint32_t slot;
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[i];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            rsrmgr = &g_array_index(lmi->body.open_8.rsrmgrs,
                                    struct lixa_msg_body_open_8_rsrmgr_s,
                                    i);
            pld->rm.rmid = rsrmgr->rmid;
            strncpy(pld->rm.name, (char *) rsrmgr->name,
                    PAYLOAD_RSRMGR_NAME_MAX);
            common_status_rsrmgr_init(&pld->rm.state, rsrmgr->dynamic);
            pld->rm.name[PAYLOAD_RSRMGR_NAME_MAX - 1] = '\0';
            strncpy(pld->rm.xa_name, (char *) rsrmgr->xa_name, RMNAMESZ);
            pld->rm.xa_name[RMNAMESZ - 1] = '\0';
            pld->rm.state.next_verb = LIXA_MSG_VERB_OPEN;
        }

        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
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
            case RSRMGRS_ARRAY_NULL:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case MAINTENANCE_MODE:
                ret_cod = LIXA_RC_MAINTENANCE_MODE;
                break;
            case TOO_MANY_RSRMGRS:
                ret_cod = LIXA_RC_TOO_MANY_RSRMGRS;
                break;
            case PAYLOAD_CHAIN_ALLOCATE_ERROR:
            case FSM_SEND_MESSAGE_AND_WAIT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_open_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));    
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_OPEN_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_open_24(struct thread_status_s *ts,
                      size_t slot_id,
                      const struct lixa_msg_s *lmi,
                      uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        INVALID_RMID,
        THREAD_STATUS_MARK_BLOCK_ERROR,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_open_24\n"));
    TRY {
        uint32_t i;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.open_24.xa_open_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* retrieve and save control thread status */
        thread_status_get_record4update(
            ts, block_id)->data.pld.ph.state.txstate =
            lmi->body.open_24.conthr.txstate;
        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.open_24.xa_open_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_open_24_xa_open_execs_s *xa_open_execs;
            uint32_t slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[i];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            xa_open_execs = &g_array_index(
                lmi->body.open_24.xa_open_execs,
                struct lixa_msg_body_open_24_xa_open_execs_s, i);
            /* check rmid: this check should be useless... */
            if (pld->rm.rmid != xa_open_execs->rmid)
                THROW(INVALID_RMID);
            /* update the block */
            pld->rm.state.xa_r_state = xa_open_execs->r_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            strncpy(pld->rm.xa_open_info, (char *) xa_open_execs->xa_info,
                    MAXINFOSIZE);
            pld->rm.xa_open_info[MAXINFOSIZE - 1] = '\0';
            pld->rm.xa_open_flags = xa_open_execs->flags;
            pld->rm.xa_open_rc = xa_open_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR);
        } /* for (i=0; ... */
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case INVALID_RMID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR:
            case FSM_WANT_MESSAGE:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_open_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_OPEN_24,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_prepare(struct thread_status_s *ts,
                      size_t slot_id,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id)
{
    enum Exception { SERVER_XA_PREPARE_8_ERROR
                     , SERVER_XA_PREPARE_24_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_prepare\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (ret_cod = server_xa_prepare_8(
                                       ts, slot_id, lmi, lmo, block_id)))
                    THROW(SERVER_XA_PREPARE_8_ERROR);
                break;
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_xa_prepare_24(
                                       ts, slot_id, lmi, lmo, block_id)))
                    THROW(SERVER_XA_PREPARE_24_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (lmi->header.pvs.step) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_PREPARE_8_ERROR:
            case SERVER_XA_PREPARE_24_ERROR:
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
    LIXA_TRACE(("server_xa_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_prepare_8(struct thread_status_s *ts,
                        size_t slot_id,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        BRANCH_LIST,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_WOULD_BLOCK,
        FSM_WANT_WAKE_UP,
        PREPARE_BRANCHES,
        WOULD_BLOCK,
        PREPARE_DELAYED,
        FSM_SEND_MESSAGE_AND_WAIT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    uint32_t *branch_array = NULL;

    LIXA_TRACE(("server_xa_prepare_8\n"));
    TRY {
        uint32_t i;
        uint32_t branch_array_size = 0;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);
        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check the number of children blocks matches with the arrived
           update resource manager records */
        if (lmi->body.prepare_8.xa_prepare_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* check if the block is a branch inside a multiple branches
           transaction */
        if (server_xa_branch_is_chained(ts, block_id)) {
            LIXA_TRACE(("server_xa_prepare_8: this branch is part of a "
                        "multiple branches transaction\n"));
            /* retrieve the list of all the branches in this global
               transaction */
            if (LIXA_RC_OK != (ret_cod = server_xa_branch_list(
                                   ts, block_id, &branch_array_size,
                                   &branch_array)))
                THROW(BRANCH_LIST);
        }
        /* store commit/rollback intent after prepare phase */
        if (lmi->body.prepare_8.conthr.commit) {
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.state.will_commit = TRUE;
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.state.will_rollback = FALSE;
        } else {
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.state.will_commit = FALSE;
            thread_status_get_record4update(
                ts, block_id)->data.pld.ph.state.will_rollback = TRUE;
        }
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.prepare_8.xa_prepare_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_prepare_8_xa_prepare_execs_s
                *xa_prepare_execs;
            uint32_t slot;
            xa_prepare_execs = &g_array_index(
                lmi->body.prepare_8.xa_prepare_execs,
                struct lixa_msg_body_prepare_8_xa_prepare_execs_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[xa_prepare_execs->rmid];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.xa_s_state = xa_prepare_execs->s_state;
            pld->rm.state.xa_td_state = xa_prepare_execs->td_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            pld->rm.xa_prepare_flags = xa_prepare_execs->flags;
            pld->rm.xa_prepare_rc = xa_prepare_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */
        /* check all the branches if this is a multiple branches transaction */
        if (0 < branch_array_size) {
            ret_cod = server_xa_branch_prepare(
                ts, block_id, branch_array_size, branch_array,
                cs->branch_join);
            switch (ret_cod) {
                case LIXA_RC_OK:
                    cs->branch_join = CLIENT_BRANCH_JOIN_OK;
                    break;
                case LIXA_RC_MULTIBRANCH_PREPARE_FAILED:
                    cs->branch_join = CLIENT_BRANCH_JOIN_KO;
                    break;
                case LIXA_RC_OPERATION_POSTPONED:
                    if (lmi->body.prepare_8.conthr.non_block) {
                        if (LIXA_RC_OK != (
                                ret_cod = server_fsm_would_block(
                                    &cs->fsm, lixa_session_get_sid(
                                        &cs->session))))
                            THROW(FSM_WOULD_BLOCK);
                    } else {
                        if (LIXA_RC_OK != (
                                ret_cod = server_fsm_want_wake_up(
                                    &cs->fsm, lixa_session_get_sid(
                                        &cs->session))))
                            THROW(FSM_WANT_WAKE_UP);
                    }
                    break;
                default:
                    THROW(PREPARE_BRANCHES);
            } /* switch (ret_cod) */
        } else
            ret_cod = LIXA_RC_OK;
            
        /* send an answer message only if OK or WOULD_BLOCK */
        if (!server_fsm_is_waiting_wake_up(&cs->fsm)) {
            /* prepare output message */
            lmo->header.pvs.verb = lmi->header.pvs.verb;
            /* prepare next protocol step */
            cs->last_verb_step.verb = lmo->header.pvs.verb;
            cs->last_verb_step.step = lmo->header.pvs.step;
            /* the status file must be synchronized */
            status_sync_ask_sync(&ts->status_sync);
            if (!server_fsm_is_waiting_unblock(&cs->fsm))
                /* update the Finite State Machine */
                if (LIXA_RC_OK != (ret_cod = server_fsm_send_message_and_wait(
                                       &cs->fsm, lixa_session_get_sid(
                                           &cs->session))))
                    THROW(FSM_SEND_MESSAGE_AND_WAIT);
        } /* if (!server_fsm_is_waiting_wake_up(&cs->fsm)) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case BRANCH_LIST:
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
            case THREAD_STATUS_MARK_BLOCK_ERROR2:
            case FSM_WOULD_BLOCK:
            case FSM_WANT_WAKE_UP:
            case PREPARE_BRANCHES:
                break;
            case WOULD_BLOCK:
                ret_cod = LIXA_RC_WOULD_BLOCK;
                break;
            case PREPARE_DELAYED:
                ret_cod = LIXA_RC_OPERATION_POSTPONED;
                break;
            case FSM_SEND_MESSAGE_AND_WAIT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* clean memory */
    if (NULL != branch_array) {
        free(branch_array);
        branch_array = NULL;
    }
    LIXA_TRACE(("server_xa_prepare_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();    
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_PREPARE_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_prepare_24(struct thread_status_s *ts,
                         size_t slot_id,
                         const struct lixa_msg_s *lmi,
                         struct lixa_msg_s *lmo,
                         uint32_t block_id)
{
    enum Exception { INVALID_BLOCK_ID
                     , BRANCH_LIST
                     , PROTOCOL_ERROR
                     , FSM_WANT_WAKE_UP
                     , PREPARE_BRANCHES
                     , FSM_SEND_MESSAGE_AND_WAIT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    uint32_t *branch_array = NULL;
    
    LIXA_TRACE(("server_xa_prepare_24\n"));
    TRY {
        uint32_t branch_array_size = 0;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);
        
        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check if the block is a branch inside a multiple branches
           transaction */
        if (server_xa_branch_is_chained(ts, block_id)) {
            LIXA_TRACE(("server_xa_prepare_24: this branch is part of a "
                        "multiple branches transaction\n"));
            /* retrieve the list of all the branches in this global
               transaction */
            if (LIXA_RC_OK != (ret_cod = server_xa_branch_list(
                                   ts, block_id, &branch_array_size,
                                   &branch_array)))
                THROW(BRANCH_LIST);
        } else {
            /* this is an error only if branch_join has not been setup
               previously */
            if (CLIENT_BRANCH_JOIN_NULL == cs->branch_join) {
                LIXA_TRACE(("server_xa_prepare_24: this message can't arrive "
                            "from a client that's not part of a "
                            "multiple branches transaction\n"));
                THROW(PROTOCOL_ERROR);
            } /* if (CLIENT_BRANCH_JOIN_NULL != */
        } /* if (server_xa_branch_is_chained(ts, block_id)) */
        
        /* check all the branches if this is a multiple branches transaction */
        if (0 < branch_array_size) {
            ret_cod = server_xa_branch_prepare(
                ts, block_id, branch_array_size, branch_array,
                cs->branch_join);
            switch (ret_cod) {
                case LIXA_RC_OK:
                    cs->branch_join = CLIENT_BRANCH_JOIN_OK;
                    break;
                case LIXA_RC_MULTIBRANCH_PREPARE_FAILED:
                    cs->branch_join = CLIENT_BRANCH_JOIN_KO;
                    break;
                case LIXA_RC_OPERATION_POSTPONED:
                    if (CLIENT_BRANCH_JOIN_NULL == cs->branch_join) {
                        /* going to sleep and wait for a wake-up only
                           if some other branch has not already rised a
                           KO or OK condition */
                        if (LIXA_RC_OK != (ret_cod = server_fsm_want_wake_up(
                                               &cs->fsm, lixa_session_get_sid(
                                                   &cs->session))))
                            THROW(FSM_WANT_WAKE_UP);
                    } /* if (CLIENT_BRANCH_JOIN_KO != cs->branch_join) */
                    break;
                default:
                    THROW(PREPARE_BRANCHES);
            } /* switch (ret_cod) */
        } else
            ret_cod = LIXA_RC_OK;
            
        /* send an answer message only if not postponed */
        if (!server_fsm_is_waiting_wake_up(&cs->fsm)) {
            /* prepare output message */
            lmo->header.pvs.verb = lmi->header.pvs.verb;
            /* prepare next protocol step */
            cs->last_verb_step.verb = lmo->header.pvs.verb;
            cs->last_verb_step.step = lmo->header.pvs.step;
            /* update the Finite State Machine */
            if (LIXA_RC_OK != (ret_cod = server_fsm_send_message_and_wait(
                                   &cs->fsm, lixa_session_get_sid(
                                       &cs->session))))
                THROW(FSM_SEND_MESSAGE_AND_WAIT);
        } /* if (!server_fsm_is_waiting_wake_up(&cs->fsm)) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case BRANCH_LIST:
                break;
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case FSM_WANT_WAKE_UP:
            case PREPARE_BRANCHES:
            case FSM_SEND_MESSAGE_AND_WAIT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* clean memory */
    if (NULL != branch_array) {
        free(branch_array);
        branch_array = NULL;
    }
    LIXA_TRACE(("server_xa_prepare_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_PREPARE_24,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_rollback(struct thread_status_s *ts,
                       size_t slot_id,
                       const struct lixa_msg_s *lmi,
                       uint32_t block_id)
{
    enum Exception { SERVER_XA_ROLLBACK_8_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_rollback\n"));
    TRY {
        if (8 != lmi->header.pvs.step) {
            THROW(INVALID_STEP);
        } else if (LIXA_RC_OK != (ret_cod = server_xa_rollback_8(
                                      ts, slot_id, lmi, block_id)))
            THROW(SERVER_XA_ROLLBACK_8_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_ROLLBACK_8_ERROR:
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
    LIXA_TRACE(("server_xa_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_rollback_8(struct thread_status_s *ts,
                         size_t slot_id,
                         const struct lixa_msg_s *lmi,
                         uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        XID_SERIALIZE_ERROR,
        TRANS_TABLE_REMOVE_ERROR,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct server_trans_tbl_qry_s sttq;
    sttq.gtrid = NULL;

    LIXA_TRACE(("server_xa_rollback_8\n"));
    TRY {
        uint32_t i;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.rollback_8.xa_rollback_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store rollback intent */
        thread_status_get_record4update(
            ts, block_id)->data.pld.ph.state.finished =
            lmi->body.rollback_8.conthr.finished;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        /* remove XID from the global transaction table */
        sttq.gtrid = lixa_xid_get_gtrid_ascii(
            &(thread_status_get_record4read(
                  ts, block_id)->data.pld.ph.state.xid));
        if (!lixa_xid_serialize(
                &(thread_status_get_record4read(
                      ts, block_id)->data.pld.ph.state.xid),
                sttq.xid))
            THROW(XID_SERIALIZE_ERROR);
        sttq.tsid = ts->id;
        if (LIXA_RC_OK != (ret_cod = server_trans_tbl_remove(
                               ts->trans_table, &sttq)))
            THROW(TRANS_TABLE_REMOVE_ERROR);

        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.rollback_8.xa_rollback_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_rollback_8_xa_rollback_execs_s
                *xa_rollback_execs;
            uint32_t slot;
            xa_rollback_execs = &g_array_index(
                lmi->body.rollback_8.xa_rollback_execs,
                struct lixa_msg_body_rollback_8_xa_rollback_execs_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[
                    xa_rollback_execs->rmid];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.xa_r_state = xa_rollback_execs->r_state;
            pld->rm.state.xa_s_state = xa_rollback_execs->s_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            pld->rm.xa_rollback_flags = xa_rollback_execs->flags;
            pld->rm.xa_rollback_rc = xa_rollback_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case TRANS_TABLE_REMOVE_ERROR:
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
                break;
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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
    /* release gtrid */
    if (NULL != sttq.gtrid) {
        free(sttq.gtrid);
        sttq.gtrid = NULL;
    }
    LIXA_TRACE(("server_xa_rollback_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));    
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_ROLLBACK_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_start(struct thread_status_s *ts,
                    size_t slot_id,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id)
{
    enum Exception { SERVER_XA_START_8_ERROR,
                     SERVER_XA_START_24_ERROR,
                     INVALID_STEP,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_start\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (ret_cod = server_xa_start_8(
                                       ts, slot_id, lmi, lmo, block_id)))
                    THROW(SERVER_XA_START_8_ERROR);
                break;
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_xa_start_24(
                                       ts, slot_id, lmi, block_id)))
                    THROW(SERVER_XA_START_24_ERROR);
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_START_8_ERROR:
            case SERVER_XA_START_24_ERROR:
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
    LIXA_TRACE(("server_xa_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_xa_start_8(struct thread_status_s *ts,
                      size_t slot_id,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id)
{
    enum Exception {
        XID_GET_GTRID_ERROR, 
        XID_SERIALIZE_ERROR,
        G_ARRAY_NEW_ERROR,
        TRANS_TBL_QUERY_XID_ERROR,
        BRANCHES_ON_MULTIPLE_THREADS,
        THREAD_SWITCH,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        SERVER_RECORD_BRANCH_CHAIN,
        TRANS_TABLE_INSERT_ERROR,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_SEND_MESSAGE_AND_WAIT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int warning = LIXA_RC_OK;
    server_trans_tbl_qry_arr_t *query_result = NULL;
    struct server_trans_tbl_qry_s sttq;
    sttq.gtrid = NULL;
    
    LIXA_TRACE(("server_xa_start_8\n"));
    TRY {
        uint32_t i;
        int chain_branch = FALSE;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* prepare transaction table record */
        memset(&sttq, 0, sizeof(sttq));
        if (NULL == (sttq.gtrid = lixa_xid_get_gtrid_ascii(
                         &(lmi->body.start_8.conthr.xid))))
            THROW(XID_GET_GTRID_ERROR);
        if (!lixa_xid_serialize(&(lmi->body.start_8.conthr.xid), sttq.xid))
            THROW(XID_SERIALIZE_ERROR);
        sttq.tsid = ts->id;
        sttq.block_id = block_id;
        /* check if the xa_start is related to a subordinate branch */
        if (lmi->body.start_8.conthr.sub_branch) {
            guint i, tsid = 0;
            LIXA_TRACE(("server_xa_start_8: the client is asking a new "
                        "subordinate branch\n"));
            /* preparying query_result array for next step */
            if (NULL == (query_result = server_trans_tbl_qry_arr_new()))
                THROW(G_ARRAY_NEW_ERROR);
            /* looking for other branches of the same global transaction */
            ret_cod = server_trans_tbl_query_xid(ts->trans_table, &sttq,
                                                 query_result, FALSE);
            if (LIXA_RC_OBJ_NOT_FOUND == ret_cod) {
                /* this could happen if a subordinate branch is late and
                   the superior branch has already committed; a specific
                   return code must be passed to the client */
                LIXA_TRACE(("server_xa_start_8: client is asking to "
                            "branch an existing global transaction, but "
                            "no other branches have been found for "
                            "xid='%s'\n", sttq.xid));
                LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD030N, sttq.xid));
                /* set a warning condition */
                warning = LIXA_RC_NO_SUPERIOR_BRANCH;
            } else if (LIXA_RC_OK != ret_cod) {
                THROW(TRANS_TBL_QUERY_XID_ERROR);
            } else {
                struct server_trans_tbl_qry_s *result = NULL;
                /* check result: only one thread must contain it*/
                for (i=0; i<query_result->len; ++i) {
                    result = &g_array_index(query_result,
                                            struct server_trans_tbl_qry_s, i);
                    LIXA_TRACE(("server_xa_start_8: xid='%s' found in "
                                "thread %u\n", result->xid, result->tsid));
                    if (0 == tsid) {
                        /* first entry */
                        tsid = result->tsid;
                    } else if (tsid != result->tsid) {
                        LIXA_TRACE(("server_xa_start_8: the same global "
                                    "transaction %s has been found in two "
                                    "state threads: %u and %u\n",
                                    result->xid, tsid, result->tsid));
                        LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD029W,
                                     result->xid, tsid, result->tsid));
                        THROW(BRANCHES_ON_MULTIPLE_THREADS);
                    } /* if (tsid != result->tsid) */
                } /* for (i=0; i<query_result->len; ++i) */
                /* at this point one or more branches of the same global
                   transaction have been found in thread tsid */
                if (tsid != ts->id) {
                    struct thread_status_switch_s *tss =
                        &(ts->client_array[slot_id].switch_thread);
                    /* move to the proper thread, 
                       set destination switch thread */
                    tss->id = tsid;
                    LIXA_TRACE(("server_xa_start_8: tsid=%u, ts->id=%u, "
                                "tss->id=%u\n", tsid, ts->id, tss->id));
                    LIXA_TRACE(("server_xa_start_8: client is working on "
                                "branch %s that must be managed by state "
                                "thread id %u (this thread id is %u)\n",
                                sttq.xid, tss->id, ts->id));
                    LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD031I,
                                 result->xid, tss->id, ts->id));
                    THROW(THREAD_SWITCH);
                } else {
                    /* the branch must be chained with the other ones */
                    chain_branch = TRUE;
                } /* if (tsid != ts->id) */
            } /* check query result */
        } /* if (lmi->body.start_8.conthr.sub_branch) */
        
        /* store XID in the header block */
        thread_status_get_record4update(
            ts, block_id)->data.pld.ph.state.xid = 
            lmi->body.start_8.conthr.xid;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        /* chain this record to other branches */
        if (chain_branch) {
            if (LIXA_RC_OK != (ret_cod = server_xa_branch_chain(
                                   ts, block_id, query_result)))
                THROW(SERVER_RECORD_BRANCH_CHAIN);
        } /* if (chain_branch) */
        /* store XID in the global transaction table */
        if (LIXA_RC_OK != (ret_cod = server_trans_tbl_insert(
                               ts->trans_table, &sttq)))
            THROW(TRANS_TABLE_INSERT_ERROR);
        /* reset finished state */
        thread_status_get_record4update(
            ts, block_id)->data.pld.ph.state.finished = FALSE;
        
        /* store next_verb for not dynamically registering resource managers */
        for (i = 0; i < lmi->body.start_8.rsrmgrs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_start_8_rsrmgr_s *rsrmgrs;
            uint32_t slot;
            rsrmgrs = &g_array_index(
                lmi->body.start_8.rsrmgrs,
                struct lixa_msg_body_start_8_rsrmgr_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[rsrmgrs->rmid];

            LIXA_TRACE(("server_xa_start_8: updating next_verb for resource "
                        "manager # " UINT32_T_FORMAT "\n", rsrmgrs->rmid));
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.next_verb = LIXA_MSG_VERB_START;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */

        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        cs->last_verb_step.verb = lmo->header.pvs.verb;
        cs->last_verb_step.step = lmo->header.pvs.step;
        /* 2018-05-21 Ch.F.
         * This status file synchronization seems to be useless: no test case
         * requires it.
         * This comment will stay here for a long time: before re-activating it
         * a clear need must be analyzed

         status_sync_ask_sync(&ts->status_sync);
        */
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_send_message_and_wait(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_SEND_MESSAGE_AND_WAIT);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XID_GET_GTRID_ERROR:
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case G_ARRAY_NEW_ERROR:
                ret_cod = LIXA_RC_G_ARRAY_NEW_ERROR;
                break;
            case TRANS_TBL_QUERY_XID_ERROR:
                break;
            case BRANCHES_ON_MULTIPLE_THREADS:
                ret_cod = LIXA_RC_BRANCHES_ON_MULTIPLE_THREADS;
                break;
            case THREAD_SWITCH:
                ret_cod = LIXA_RC_THREAD_SWITCH;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
            case SERVER_RECORD_BRANCH_CHAIN:
            case TRANS_TABLE_INSERT_ERROR:
            case THREAD_STATUS_MARK_BLOCK_ERROR2:
            case FSM_SEND_MESSAGE_AND_WAIT:
                break;
            case NONE:
                ret_cod = warning;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* memory recovery to avoid memory leak */
    if (NULL != sttq.gtrid)
        free(sttq.gtrid);
    if (NULL != query_result)
        server_trans_tbl_qry_arr_delete(query_result);
    LIXA_TRACE(("server_xa_start_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));    
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_START_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_start_24(struct thread_status_s *ts,
                       size_t slot_id,
                       const struct lixa_msg_s *lmi,
                       uint32_t block_id)
{
    enum Exception {
        INVALID_BLOCK_ID,
        NUMBER_OF_RSRMGRS_MISMATCH,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        FSM_WANT_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_xa_start_24\n"));
    TRY {
        uint32_t i;
        struct server_client_status_s *cs = &(ts->client_array[slot_id]);

        /* check block_id is a valid block */
        if (thread_status_get_record4read(ts, block_id)->data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.start_24.xa_start_execs->len >
            thread_status_get_record4read(ts, block_id)->data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* retrieve and save control thread status */
        thread_status_get_record4update(
            ts, block_id)->data.pld.ph.state.txstate =
            lmi->body.start_24.conthr.txstate;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        /* store data in the children blocks... */
        for (i = 0; i < lmi->body.start_24.xa_start_execs->len; ++i) {
            struct status_record_data_payload_s *pld = NULL;
            struct lixa_msg_body_start_24_xa_start_execs_s *xa_start_execs;
            uint32_t slot;
            xa_start_execs = &g_array_index(
                lmi->body.start_24.xa_start_execs,
                struct lixa_msg_body_start_24_xa_start_execs_s, i);
            slot = thread_status_get_record4read(
                ts, block_id)->data.pld.ph.block_array[xa_start_execs->rmid];
            pld = &(thread_status_get_record4update(ts, slot)->data.pld);
            /* update the block */
            pld->rm.state.xa_td_state = xa_start_execs->td_state;
            pld->rm.state.xa_s_state = xa_start_execs->s_state;
            pld->rm.state.next_verb = LIXA_MSG_VERB_NULL;
            pld->rm.xa_start_flags = xa_start_execs->flags;
            pld->rm.xa_start_rc = xa_start_execs->rc;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, slot)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        } /* for (i=0; ... */
        /* update the Finite State Machine */
        if (LIXA_RC_OK != (ret_cod = server_fsm_want_message(
                               &cs->fsm, lixa_session_get_sid(
                                   &cs->session))))
            THROW(FSM_WANT_MESSAGE);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
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
    LIXA_TRACE(("server_xa_start_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_START_24,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



