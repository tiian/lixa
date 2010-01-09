/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_errors.h>
#include <server_xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_XA



int server_xa_close(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    uint32_t block_id)
{
    enum Exception { PAYLOAD_CHAIN_RELEASE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_close\n"));
    TRY {
        /* release all allocated blocks */
        if (LIXA_RC_OK != (
                ret_cod = payload_chain_release(
                    ts, ts->client_array[block_id].pers_status_slot_id)))
            THROW(PAYLOAD_CHAIN_RELEASE);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PAYLOAD_CHAIN_RELEASE:
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
    return ret_cod;
}



int server_xa_open(struct thread_status_s *ts,
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
                if (LIXA_RC_OK != (ret_cod = server_xa_open_8(
                                       ts, lmi, lmo, block_id)))
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
                     uint32_t block_id)
{
    enum Exception { RSRMGRS_ARRAY_NULL
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
        if (lmi->body.open_8.rsrmgrs->len > CHAIN_MAX_SIZE) {
            LIXA_TRACE(("server_xa_open_8: message arrived from client "
                        "would use %u (max is %u)\n",
                        lmi->body.open_8.rsrmgrs->len,
                        CHAIN_MAX_SIZE));
            THROW(TOO_MANY_RSRMGRS);
        }

        if (LIXA_RC_OK != (ret_cod = payload_chain_allocate(
                               ts, block_id, lmi->body.open_8.rsrmgrs->len)))
            THROW(PAYLOAD_CHAIN_ALLOCATE_ERROR);

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
            common_status_rsrmgr_init(&sr->sr.data.pld.rm.state);
            sr->sr.data.pld.rm.name[PAYLOAD_RSRMGR_NAME_MAX - 1] = '\0';
            strncpy(sr->sr.data.pld.rm.xa_name, (char *)rsrmgr->xa_name,
                    RMNAMESZ);
            sr->sr.data.pld.rm.xa_name[RMNAMESZ - 1] = '\0';
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_OPEN;
        }

        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        ts->client_array[block_id].last_verb_step.verb = lmo->header.pvs.verb;
        ts->client_array[block_id].last_verb_step.step = lmo->header.pvs.step;

        THROW(NONE);
    } CATCH {
        switch (excp) {
#ifdef LIXA_DEBUG
            case RSRMGRS_ARRAY_NULL:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
#endif /* LIXA_DEBUG */
            case TOO_MANY_RSRMGRS:
                ret_cod = LIXA_RC_TOO_MANY_RSRMGRS;
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
            lmi->body.open_24.conthr.state;
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
            sr->sr.data.pld.rm.state.xa_r_state = xa_open_execs->state;
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
    return ret_cod;
}



int server_xa_start(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id)
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
                if (LIXA_RC_OK != (ret_cod = server_xa_start_8(
                                       ts, lmi, lmo, block_id)))
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
                      uint32_t block_id)
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
                        "manager # " UINT32_T_FORMAT "\n", slot));
            sr = ts->curr_status + slot;
            /* update the block */
            status_record_update(ts->curr_status + slot, slot,
                                 ts->updated_records);
            sr->sr.data.pld.rm.state.next_verb = LIXA_MSG_VERB_START;
        } /* for (i=0; ... */

        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        ts->client_array[block_id].last_verb_step.verb = lmo->header.pvs.verb;
        ts->client_array[block_id].last_verb_step.step = lmo->header.pvs.step;

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
        ts->curr_status[block_id].sr.data.pld.ph.state.txstate =
            lmi->body.start_24.conthr.state;
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
            sr->sr.data.pld.rm.state.xa_t_state = xa_start_execs->state;
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
    return ret_cod;
}



int server_xa_end(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id)
{
    enum Exception { SERVER_XA_END_8_ERROR
                     , SERVER_XA_END_24_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_end\n"));
    TRY {
        switch (lmi->header.pvs.step) {
            case 8:
                if (LIXA_RC_OK != (ret_cod = server_xa_end_8(
                                       ts, lmi, lmo, block_id)))
                    THROW(SERVER_XA_END_8_ERROR);
                break;
                /*
            case 24:
                if (LIXA_RC_OK != (ret_cod = server_xa_end_24(
                                       ts, lmi, block_id)))
                    THROW(SERVER_XA_END_24_ERROR);
                break;
                */
            default:
                THROW(INVALID_STEP);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_END_8_ERROR:
            case SERVER_XA_END_24_ERROR:
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
                    uint32_t block_id)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_end_8\n"));
    TRY {
        uint32_t i;
        
        /* store commit/rollback intent */
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        if (lmi->body.end_8.conthr.commit)
            ts->curr_status[block_id].sr.data.pld.ph.state.will_commit = TRUE;
        else
            ts->curr_status[block_id].sr.data.pld.ph.state.will_rollback = TRUE;
        
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

        /* prepare output message */
        lmo->header.pvs.verb = lmi->header.pvs.verb;
        /* prepare next protocol step */
        ts->client_array[block_id].last_verb_step.verb = lmo->header.pvs.verb;
        ts->client_array[block_id].last_verb_step.step = lmo->header.pvs.step;

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
    LIXA_TRACE(("server_xa_end_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



