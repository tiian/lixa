/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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
            LIXA_TRACE(("server_reply_default: error while "
                        "serializing reply message to client\n"));
            /* release the buffer to avoid transmission */
            free(ts->client_array[slot_id].output_buffer);
            ts->client_array[slot_id].output_buffer = NULL;
            THROW(SERIALIZE_ERROR);
        }
        if (LIXA_RC_OK == ret_cod) {
            LIXA_TRACE(("server_reply_default: reply message is "
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



int server_reply_prepare(struct thread_status_s *ts, size_t slot_id,
                         struct lixa_msg_s *lmo, int rc)
{
    enum Exception { REPLY_DEFAULT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_reply_prepare\n"));
    TRY {
        lmo->body.prepare_16.answer.rc = rc;
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
    LIXA_TRACE(("server_reply_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_reply_qrcvr(struct thread_status_s *ts, size_t slot_id,
                       struct lixa_msg_s *lmo)
{
    enum Exception { SERIALIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_reply_qrcvr\n"));
    TRY {
        if (LIXA_RC_OK != (
                ret_cod = lixa_msg_serialize(
                    lmo,
                    ts->client_array[slot_id].output_buffer,
                    LIXA_MSG_XML_BUFFER_SIZE,
                    &ts->client_array[slot_id].output_buffer_size))) {
            LIXA_TRACE(("server_reply_qrcvr: error while "
                        "serializing reply message to client\n"));
            /* release the buffer to avoid transmission */
            free(ts->client_array[slot_id].output_buffer);
            ts->client_array[slot_id].output_buffer = NULL;
            THROW(SERIALIZE_ERROR);
        }
        if (LIXA_RC_OK == ret_cod) {
            LIXA_TRACE(("server_reply_qrcvr: reply message is "
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
    LIXA_TRACE(("server_reply_qrcvr/excp=%d/"
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



