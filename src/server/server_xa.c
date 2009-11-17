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



int server_xa_open(struct thread_status_s *ts,
                   const struct lixa_msg_s *lm,
                   uint32_t block_id)
{
    enum Exception { SERVER_XA_OPEN_1_ERROR
                     , INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_open\n"));
    TRY {
        switch (lm->header.step) {
            case 1:
                if (LIXA_RC_OK != (ret_cod = server_xa_open_1(
                                       ts, lm, block_id)))
                    THROW(SERVER_XA_OPEN_1_ERROR);
                break;
            default:
                THROW(INVALID_STEP);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERVER_XA_OPEN_1_ERROR:
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



int server_xa_open_1(struct thread_status_s *ts,
                     const struct lixa_msg_s *lm,
                     uint32_t block_id)
{
    enum Exception { RSRMGRS_ARRAY_NULL
                     , TOO_MANY_RSRMGRS
                     , PAYLOAD_CHAIN_ALLOCATE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_open_1\n"));
    TRY {
        uint32_t i;
        
#ifndef NDEBUG
        /* check the resource manager array is OK */
        if (NULL == lm->body.open_1.rsrmgrs)
            THROW(RSRMGRS_ARRAY_NULL);
#endif /* NDEBUG */
        if (lm->body.open_1.rsrmgrs->len > CHAIN_MAX_SIZE) {
            LIXA_TRACE(("server_xa_open_1: message arrived from client "
                        "would use %u (max is %u)\n",
                        lm->body.open_1.rsrmgrs->len,
                        CHAIN_MAX_SIZE));
            THROW(TOO_MANY_RSRMGRS);
        }

        if (LIXA_RC_OK != (ret_cod = payload_chain_allocate(
                               ts, block_id, lm->body.open_1.rsrmgrs->len)))
            THROW(PAYLOAD_CHAIN_ALLOCATE_ERROR);

        for (i=0; i<lm->body.open_1.rsrmgrs->len; ++i) {
            status_record_t *sr;
            struct lixa_msg_body_open_1_rsrmgr_s *rsrmgr;
            sr = ts->curr_status +
                ts->curr_status[block_id].sr.data.pld.ph.block_array[i];
            rsrmgr = &g_array_index(lm->body.open_1.rsrmgrs,
                                    struct lixa_msg_body_open_1_rsrmgr_s,
                                    i);
            sr->sr.data.pld.rm.rmid = rsrmgr->rmid;
            strncpy(sr->sr.data.pld.rm.name, (char *)rsrmgr->name,
                    PAYLOAD_RSRMGR_NAME_MAX);
            sr->sr.data.pld.rm.name[PAYLOAD_RSRMGR_NAME_MAX - 1] = '\0';
        }
        /* @@@ go on with implementation... reply to client the records
         are allocated */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
#ifndef NDEBUG
            case RSRMGRS_ARRAY_NULL:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
#endif /* NDEBUG */
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
    LIXA_TRACE(("server_xa_open_1/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
