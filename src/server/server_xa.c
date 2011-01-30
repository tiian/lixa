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



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_errors.h>
#include <lixa_crash.h>
#include <server_xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_XA



int server_ax_reg(struct thread_status_s *ts,
                  const struct lixa_msg_s *lmi,
                  uint32_t block_id)
{
    enum Exception { INVALID_STEP
                     , INVALID_BLOCK_ID
                     , RMID_OUT_OF_RANGE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_ax_reg\n"));
    TRY {
        uint32_t slot;
        const struct lixa_msg_body_reg_8_ax_reg_exec_s *ax_reg_exec =
            &lmi->body.reg_8.ax_reg_exec;
        status_record_t *sr;

        /* check message step */
        if (8 != lmi->header.pvs.step)
            THROW(INVALID_STEP);
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check rmid */
        if (ax_reg_exec->rmid < 0 ||
            ax_reg_exec->rmid >= ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(RMID_OUT_OF_RANGE);
        
        slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
            ax_reg_exec->rmid];
        /* update the block */
        status_record_update(ts->curr_status + slot, slot,
                             ts->updated_records);
        sr = ts->curr_status + slot;
        sr->sr.data.pld.rm.state.xa_td_state = ax_reg_exec->td_state;
        sr->sr.data.pld.rm.state.xa_s_state = ax_reg_exec->s_state;
        sr->sr.data.pld.rm.ax_reg_flags = ax_reg_exec->flags;
        sr->sr.data.pld.rm.ax_reg_rc = ax_reg_exec->rc;
        
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_ax_reg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_AX_REG,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_ax_unreg(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    uint32_t block_id)
{
    enum Exception { INVALID_STEP
                     , INVALID_BLOCK_ID
                     , RMID_OUT_OF_RANGE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_ax_unreg\n"));
    TRY {
        uint32_t slot;
        const struct lixa_msg_body_unreg_8_ax_unreg_exec_s *ax_unreg_exec =
            &lmi->body.unreg_8.ax_unreg_exec;
        status_record_t *sr;

        /* check message step */
        if (8 != lmi->header.pvs.step)
            THROW(INVALID_STEP);
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check rmid */
        if (ax_unreg_exec->rmid < 0 ||
            ax_unreg_exec->rmid >= ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(RMID_OUT_OF_RANGE);
        
        slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
            ax_unreg_exec->rmid];
        /* update the block */
        status_record_update(ts->curr_status + slot, slot,
                             ts->updated_records);
        sr = ts->curr_status + slot;
        sr->sr.data.pld.rm.state.xa_td_state = ax_unreg_exec->td_state;
        sr->sr.data.pld.rm.ax_unreg_flags = ax_unreg_exec->flags;
        sr->sr.data.pld.rm.ax_unreg_rc = ax_unreg_exec->rc;
        
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_ax_unreg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_AX_UNREG,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_close(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    uint32_t block_id)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_close\n"));
    TRY {
        /* nothing to do, but a simple trace... ?! */
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
    LIXA_TRACE(("server_xa_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_xa_commit(struct thread_status_s *ts,
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
                                      ts, lmi, block_id)))
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
    return ret_cod;
}



int server_xa_commit_8(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        uint32_t block_id)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_commit_8\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.commit_8.xa_commit_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store commit/rollback intent after commit phase */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        ts->curr_status[block_id].sr.data.pld.ph.state.finished =
            lmi->body.commit_8.conthr.finished;
        
        /* store data in the children blocks... */
        for (i=0; i<lmi->body.commit_8.xa_commit_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_commit_8_xa_commit_execs_s *xa_commit_execs;
            uint32_t slot;
            xa_commit_execs = &g_array_index(
                lmi->body.commit_8.xa_commit_execs,
                struct lixa_msg_body_commit_8_xa_commit_execs_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                xa_commit_execs->rmid];
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_r_state = xa_commit_execs->r_state;
            sr->sr.data.pld.rm.state.xa_s_state = xa_commit_execs->s_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            sr->sr.data.pld.rm.xa_commit_flags = xa_commit_execs->flags;
            sr->sr.data.pld.rm.xa_commit_rc = xa_commit_execs->rc;
        } /* for (i=0; ... */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_commit_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));

    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_COMMIT_8,
               thread_status_get_crash_count(ts));    
    return ret_cod;
}



int server_xa_end(struct thread_status_s *ts,
                  const struct lixa_msg_s *lmi,
                  struct lixa_msg_s *lmo,
                  uint32_t block_id,
                  struct lixa_msg_verb_step_s *last_verb_step)
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
                            ts, lmi, lmo, block_id, last_verb_step)))
                    THROW(SERVER_XA_END_8_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
        }
        
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
    return ret_cod;
}



int server_xa_end_8(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id,
                    struct lixa_msg_verb_step_s *last_verb_step)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_end_8\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.end_8.xa_end_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        
        /* store commit/rollback intent */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        ts->asked_sync = FALSE;
        if (lmi->body.end_8.conthr.commit) {
            ts->curr_status[block_id].sr.data.pld.ph.state.will_commit = TRUE;
            /* the status file must be synchronized */
            ts->asked_sync = TRUE;
        } else
            ts->curr_status[block_id].sr.data.pld.ph.state.will_rollback =
                TRUE;
        
        /* store next_verb for all the resource managers */
        for (i=0; i<ts->curr_status[block_id].sr.data.pld.ph.n; ++i) {
            status_record_t *sr;
            uint32_t slot;
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[i];
            LIXA_TRACE(("server_xa_end_8: updating next_verb for resource "
                        "manager # " UINT32_T_FORMAT "\n", slot));
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_END;
        } /* for (i=0; ... */

        /* store data in the children blocks... */
        for (i=0; i<lmi->body.end_8.xa_end_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_end_8_xa_end_execs_s *xa_end_execs;
            uint32_t slot;
            xa_end_execs = &g_array_index(
                lmi->body.end_8.xa_end_execs,
                struct lixa_msg_body_end_8_xa_end_execs_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                xa_end_execs->rmid];
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_s_state = xa_end_execs->s_state;
            sr->sr.data.pld.rm.state.xa_td_state = xa_end_execs->td_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            sr->sr.data.pld.rm.xa_end_flags = xa_end_execs->flags;
            sr->sr.data.pld.rm.xa_end_rc = xa_end_execs->rc;
        } /* for (i=0; ... */
        
        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        last_verb_step->verb = lmo->header.pvs.verb;
        last_verb_step->step = lmo->header.pvs.step;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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

    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_END_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_forget(struct thread_status_s *ts,
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
                                      ts, lmi, block_id)))
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
    return ret_cod;
}



int server_xa_forget_8(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        uint32_t block_id)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_forget_8\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.forget_8.xa_forget_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store transaction finished status */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        ts->curr_status[block_id].sr.data.pld.ph.state.finished =
            lmi->body.forget_8.conthr.finished;
        
        /* store data in the children blocks... */
        for (i=0; i<lmi->body.forget_8.xa_forget_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_forget_8_xa_forget_execs_s *xa_forget_execs;
            uint32_t slot;
            xa_forget_execs = &g_array_index(
                lmi->body.forget_8.xa_forget_execs,
                struct lixa_msg_body_forget_8_xa_forget_execs_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                xa_forget_execs->rmid];
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_s_state = xa_forget_execs->s_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            sr->sr.data.pld.rm.xa_forget_flags = xa_forget_execs->flags;
            sr->sr.data.pld.rm.xa_forget_rc = xa_forget_execs->rc;
        } /* for (i=0; ... */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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

    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_FORGET_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_open(struct thread_status_s *ts,
                   const struct lixa_msg_s *lmi,
                   struct lixa_msg_s *lmo,
                   uint32_t block_id,
                   struct lixa_msg_verb_step_s *last_verb_step)
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
                            ts, lmi, lmo, block_id, last_verb_step)))
                    THROW(SERVER_XA_OPEN_8_ERROR);
                break;
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_xa_open_24(
                                       ts, lmi, block_id)))
                    THROW(SERVER_XA_OPEN_24_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
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
    return ret_cod;
}



int server_xa_open_8(struct thread_status_s *ts,
                     const struct lixa_msg_s *lmi,
                     struct lixa_msg_s *lmo,
                     uint32_t block_id,
                     struct lixa_msg_verb_step_s *last_verb_step)
{
    enum Exception { RSRMGRS_ARRAY_NULL
                     , MAINTENANCE_MODE
                     , TOO_MANY_RSRMGRS
                     , PAYLOAD_CHAIN_ALLOCATE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_open_8\n"));
    TRY {
        uint32_t i;
        
#ifdef LIXA_DEBUG
        /* check the resource manager array is OK */
        if (NULL == lmi->body.open_8.rsrmgrs)
            THROW(RSRMGRS_ARRAY_NULL);
#endif /* LIXA_DEBUG */

        if (ts->mmode && !lmi->body.open_8.client.maint) {
            /* server in maintenance mode, client not in maintenance mode */
            LIXA_TRACE(("server_xa_open_8: connected client is not operating "
                        "in maintenance mode while server was started in "
                        "maintenance mode; closing client connection...\n"));
            /* prepare output message */
            lmo->header.pvs.verb = lmi->header.pvs.verb;
            /* prepare next protocol step */
            last_verb_step->verb = lmo->header.pvs.verb;
            last_verb_step->step = lmo->header.pvs.step;
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
                               lmi->body.open_8.rsrmgrs->len)))
            THROW(PAYLOAD_CHAIN_ALLOCATE_ERROR);

        /* retrieve header information */
        strncpy(ts->curr_status[block_id].sr.data.pld.ph.config_digest,
                lmi->body.open_8.client.config_digest,
                sizeof(md5_digest_hex_t));
        lixa_job_set_raw(&(ts->curr_status[block_id].sr.data.pld.ph.job),
                             (char *)lmi->body.open_8.client.job);
        
        for (i=0; i<lmi->body.open_8.rsrmgrs->len; ++i) {
            status_record_t *sr;
                struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr;
                sr = ts->curr_status +
                    ts->curr_status[block_id].sr.data.pld.ph.block_array[i];
                rsrmgr = &g_array_index(lmi->body.open_8.rsrmgrs,
                                        struct lixa_msg_body_open_8_rsrmgr_s,
                                        i);
                sr->sr.data.pld.rm.rmid = rsrmgr->rmid;
                strncpy(sr->sr.data.pld.rm.name, (char *)rsrmgr->name,
                        PAYLOAD_RSRMGR_NAME_MAX);
                common_status_rsrmgr_init(&sr->sr.data.pld.rm.state,
                                          rsrmgr->dynamic);
                sr->sr.data.pld.rm.name[PAYLOAD_RSRMGR_NAME_MAX - 1] = '\0';
                strncpy(sr->sr.data.pld.rm.xa_name, (char *)rsrmgr->xa_name,
                        RMNAMESZ);
                sr->sr.data.pld.rm.xa_name[RMNAMESZ - 1] = '\0';
                sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_OPEN;
        }
        
        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        last_verb_step->verb = lmo->header.pvs.verb;
        last_verb_step->step = lmo->header.pvs.step;

        THROW(NONE);
    } CATCH {
        switch (excp) {
#ifdef LIXA_DEBUG
            case RSRMGRS_ARRAY_NULL:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
#endif /* LIXA_DEBUG */
            case MAINTENANCE_MODE:
                ret_cod = LIXA_RC_MAINTENANCE_MODE;
                break;
            case TOO_MANY_RSRMGRS:
                ret_cod = LIXA_RC_TOO_MANY_RSRMGRS;
                break;
            case PAYLOAD_CHAIN_ALLOCATE_ERROR:
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
    
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_OPEN_8,
               thread_status_get_crash_count(ts));
    return ret_cod;
}



int server_xa_open_24(struct thread_status_s *ts,
                      const struct lixa_msg_s *lmi,
                      uint32_t block_id)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , INVALID_RMID
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_open_24\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.open_24.xa_open_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* retrieve and save control thread status */
        ts->curr_status[block_id].sr.data.pld.ph.state.txstate =
            lmi->body.open_24.conthr.txstate;
        /* store data in the children blocks... */
        for (i=0; i<lmi->body.open_24.xa_open_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_open_24_xa_open_execs_s *xa_open_execs;
            uint32_t slot =
                ts->curr_status[block_id].sr.data.pld.ph.block_array[i];
            sr = ts->curr_status + slot;
            xa_open_execs = &g_array_index(
                lmi->body.open_24.xa_open_execs,
                struct lixa_msg_body_open_24_xa_open_execs_s, i);
            /* check rmid: this check should be useless... */
            if (sr->sr.data.pld.rm.rmid != xa_open_execs->rmid)
                THROW(INVALID_RMID);
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_r_state = xa_open_execs->r_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            strncpy(sr->sr.data.pld.rm.xa_open_info,
                    (char *)xa_open_execs->xa_info, MAXINFOSIZE);
            sr->sr.data.pld.rm.xa_open_info[MAXINFOSIZE - 1] = '\0';
            sr->sr.data.pld.rm.xa_open_flags = xa_open_execs->flags;
            sr->sr.data.pld.rm.xa_open_rc = xa_open_execs->rc;
        } /* for (i=0; ... */
        
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_open_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    
    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_OPEN_24,
               thread_status_get_crash_count(ts));    
    return ret_cod;
}



int server_xa_prepare(struct thread_status_s *ts,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id,
                      struct lixa_msg_verb_step_s *last_verb_step)
{
    enum Exception { SERVER_XA_PREPARE_8_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_prepare\n"));
    TRY {
        if (8 != lmi->header.pvs.step) {
            THROW(INVALID_STEP);
        } else if (LIXA_RC_OK != (ret_cod = server_xa_prepare_8(
                                      ts, lmi, lmo, block_id, last_verb_step)))
            THROW(SERVER_XA_PREPARE_8_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_PREPARE_8_ERROR:
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
    return ret_cod;
}



int server_xa_prepare_8(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id,
                        struct lixa_msg_verb_step_s *last_verb_step)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_prepare_8\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.prepare_8.xa_prepare_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store commit/rollback intent after prepare phase */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        if (lmi->body.prepare_8.conthr.commit) {
            ts->curr_status[block_id].sr.data.pld.ph.state.will_commit = TRUE;
            ts->curr_status[block_id].sr.data.pld.ph.state.will_rollback =
                FALSE;
        } else {
            ts->curr_status[block_id].sr.data.pld.ph.state.will_commit = FALSE;
            ts->curr_status[block_id].sr.data.pld.ph.state.will_rollback =
                TRUE;
        }
        
        /* store data in the children blocks... */
        for (i=0; i<lmi->body.prepare_8.xa_prepare_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_prepare_8_xa_prepare_execs_s
                *xa_prepare_execs;
            uint32_t slot;
            xa_prepare_execs = &g_array_index(
                lmi->body.prepare_8.xa_prepare_execs,
                struct lixa_msg_body_prepare_8_xa_prepare_execs_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                xa_prepare_execs->rmid];
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_s_state = xa_prepare_execs->s_state;
            sr->sr.data.pld.rm.state.xa_td_state = xa_prepare_execs->td_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            sr->sr.data.pld.rm.xa_prepare_flags = xa_prepare_execs->flags;
            sr->sr.data.pld.rm.xa_prepare_rc = xa_prepare_execs->rc;
        } /* for (i=0; ... */
        
        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        last_verb_step->verb = lmo->header.pvs.verb;
        last_verb_step->step = lmo->header.pvs.step;

        /* the status file must be synchronized */
        ts->asked_sync = TRUE;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_prepare_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));

    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_PREPARE_8,
               thread_status_get_crash_count(ts));    
    return ret_cod;
}



int server_xa_rollback(struct thread_status_s *ts,
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
                                      ts, lmi, block_id)))
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
    return ret_cod;
}



int server_xa_rollback_8(struct thread_status_s *ts,
                         const struct lixa_msg_s *lmi,
                         uint32_t block_id)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_rollback_8\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.rollback_8.xa_rollback_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* store rollback intent */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        ts->curr_status[block_id].sr.data.pld.ph.state.finished =
            lmi->body.rollback_8.conthr.finished;
        
        /* store data in the children blocks... */
        for (i=0; i<lmi->body.rollback_8.xa_rollback_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_rollback_8_xa_rollback_execs_s
                *xa_rollback_execs;
            uint32_t slot;
            xa_rollback_execs = &g_array_index(
                lmi->body.rollback_8.xa_rollback_execs,
                struct lixa_msg_body_rollback_8_xa_rollback_execs_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                xa_rollback_execs->rmid];
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_r_state = xa_rollback_execs->r_state;
            sr->sr.data.pld.rm.state.xa_s_state = xa_rollback_execs->s_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            sr->sr.data.pld.rm.xa_rollback_flags = xa_rollback_execs->flags;
            sr->sr.data.pld.rm.xa_rollback_rc = xa_rollback_execs->rc;
        } /* for (i=0; ... */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_rollback_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_xa_start(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id,
                    struct lixa_msg_verb_step_s *last_verb_step)
{
    enum Exception { SERVER_XA_START_8_ERROR
                     , SERVER_XA_START_24_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_start\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_start_8(
                            ts, lmi, lmo, block_id, last_verb_step)))
                    THROW(SERVER_XA_START_8_ERROR);
                break;
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_xa_start_24(
                                       ts, lmi, block_id)))
                    THROW(SERVER_XA_START_24_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
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
    return ret_cod;
}



int server_xa_start_8(struct thread_status_s *ts,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id,
                      struct lixa_msg_verb_step_s *last_verb_step)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_start_8\n"));
    TRY {
        uint32_t i;
        
        /* store XID in the header block */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        ts->curr_status[block_id].sr.data.pld.ph.state.xid =
            lmi->body.start_8.conthr.xid;
        
        /* store next_verb for not dynamically registering resource managers */
        for (i=0; i<lmi->body.start_8.rsrmgrs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_start_8_rsrmgr_s *rsrmgrs;
            uint32_t slot;
            rsrmgrs = &g_array_index(
                lmi->body.start_8.rsrmgrs,
                struct lixa_msg_body_start_8_rsrmgr_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                rsrmgrs->rmid];
            LIXA_TRACE(("server_xa_start_8: updating next_verb for resource "
                        "manager # " UINT32_T_FORMAT "\n", rsrmgrs->rmid));
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_START;
        } /* for (i=0; ... */

        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        last_verb_step->verb = lmo->header.pvs.verb;
        last_verb_step->step = lmo->header.pvs.step;

        /* the status file must be synchronized */
        ts->asked_sync = TRUE;
        
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
    LIXA_TRACE(("server_xa_start_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));

    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_START_8,
               thread_status_get_crash_count(ts));    
    return ret_cod;
}



int server_xa_start_24(struct thread_status_s *ts,
                       const struct lixa_msg_s *lmi,
                       uint32_t block_id)
{
    enum Exception { INVALID_BLOCK_ID
                     , NUMBER_OF_RSRMGRS_MISMATCH
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_start_24\n"));
    TRY {
        uint32_t i;
        
        /* check block_id is a valid block */
        if (ts->curr_status[block_id].sr.data.pld.type !=
            DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_ID);
        /* check children blocks match with the arrived update */
        if (lmi->body.start_24.xa_start_execs->len >
            ts->curr_status[block_id].sr.data.pld.ph.n)
            THROW(NUMBER_OF_RSRMGRS_MISMATCH);
        /* retrieve and save control thread status */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        ts->curr_status[block_id].sr.data.pld.ph.state.txstate =
            lmi->body.start_24.conthr.txstate;
        /* store data in the children blocks... */
        for (i=0; i<lmi->body.start_24.xa_start_execs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_start_24_xa_start_execs_s *xa_start_execs;
            uint32_t slot;
            xa_start_execs = &g_array_index(
                lmi->body.start_24.xa_start_execs,
                struct lixa_msg_body_start_24_xa_start_execs_s, i);
            slot = ts->curr_status[block_id].sr.data.pld.ph.block_array[
                xa_start_execs->rmid];
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.xa_td_state = xa_start_execs->td_state;
            sr->sr.data.pld.rm.state.xa_s_state = xa_start_execs->s_state;
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_NULL;
            sr->sr.data.pld.rm.xa_start_flags = xa_start_execs->flags;
            sr->sr.data.pld.rm.xa_start_rc = xa_start_execs->rc;
        } /* for (i=0; ... */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_BLOCK_ID:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NUMBER_OF_RSRMGRS_MISMATCH:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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

    LIXA_CRASH(LIXA_CRASH_POINT_SERVER_XA_START_24,
               thread_status_get_crash_count(ts));    
    return ret_cod;
}



