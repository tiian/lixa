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



#include <lixa_errors.h>
#include <lixa_xml_msg_serialize.h>
#include <server_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_REPLY



int server_reply_default(struct thread_status_s *ts, size_t slot_id,
                         struct lixa_msg_s *lmo)
{
    enum Exception { SERIALIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_reply_default\n"));
    TRY {
        if (LIXA_RC_OK != (
                ret_cod = lixa_msg_serialize(
                    lmo,
                    ts->client_array[slot_id].output_buffer,
                    LIXA_MSG_XML_BUFFER_SIZE,
                    &ts->client_array[slot_id].output_buffer_size))) {
            LIXA_TRACE(("server_reply_open: error while "
                        "serializing reply message to client\n"));
            /* release the buffer to avoid transmission */
            free(ts->client_array[slot_id].output_buffer);
            ts->client_array[slot_id].output_buffer = NULL;
            THROW(SERIALIZE_ERROR);
        }
        if (LIXA_RC_OK == ret_cod) {
            LIXA_TRACE(("server_reply_open: reply message is "
                        "|%*.*s|\n",
                        ts->client_array[slot_id].output_buffer_size,
                        ts->client_array[slot_id].output_buffer_size,
                        ts->client_array[slot_id].output_buffer));
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERIALIZE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_reply_default/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_reply_open(struct thread_status_s *ts, size_t slot_id,
                      struct lixa_msg_s *lmo, int rc)
{
    enum Exception { REPLY_DEFAULT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_reply_open\n"));
    TRY {
        lmo->body.open_16.answer.rc = rc;
        if (LIXA_RC_OK != (ret_cod = server_reply_default(ts, slot_id, lmo)))
            THROW(REPLY_DEFAULT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REPLY_DEFAULT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_reply_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_reply_start(struct thread_status_s *ts, size_t slot_id,
                       struct lixa_msg_s *lmo, int rc)
{
    enum Exception { REPLY_DEFAULT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_reply_start\n"));
    TRY {
        lmo->body.start_16.answer.rc = rc;
        if (LIXA_RC_OK != (ret_cod = server_reply_default(ts, slot_id, lmo)))
            THROW(REPLY_DEFAULT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REPLY_DEFAULT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_reply_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_reply_end(struct thread_status_s *ts, size_t slot_id,
                     struct lixa_msg_s *lmo, int rc)
{
    enum Exception { REPLY_DEFAULT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_reply_end\n"));
    TRY {
        lmo->body.end_16.answer.rc = rc;
        if (LIXA_RC_OK != (ret_cod = server_reply_default(ts, slot_id, lmo)))
            THROW(REPLY_DEFAULT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REPLY_DEFAULT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_reply_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



