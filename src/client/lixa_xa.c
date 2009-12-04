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
#include <string.h>
#endif



#include <lixa_trace.h>
#include <lixa_errors.h>
#include <lixa_xa.h>
#include <lixa_xml_msg.h>
#include <client_status.h>
#include <tx.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



int lixa_xa_open(client_status_t *cs, int *txrc)
{
    enum Exception { MSG_SERIALIZE_ERROR1
                     , SEND_ERROR1
                     , RECV_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , MSG_SERIALIZE_ERROR2
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , SEND_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_open\n"));
    TRY {
        struct lixa_msg_s msg;
        int fd;
        size_t buffer_size = 0;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_OPEN;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.open_8.client.profile = (xmlChar *)global_ccc.profile;
        msg.body.open_8.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_open_8_rsrmgr_s),
            global_ccc.actconf.rsrmgrs->len);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_open_8_rsrmgr_s record;
            record.rmid = i;
            record.name = act_rsrmgr->generic->name;
            record.xa_name = (xmlChar *)act_rsrmgr->xa_switch->name;
            g_array_append_val(msg.body.open_8.rsrmgrs, record);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer)-1, &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR1);
        buffer[buffer_size] = '\0';

        /* this object contains a lot of references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.open_8.rsrmgrs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR1);
        
        if (0 > (read_bytes = recv(fd, buffer, buffer_size, 0)))
            THROW(RECV_ERROR);
        LIXA_TRACE(("lixa_xa_open: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif
        /* check the answer from the server */
        if (LIXA_RC_OK != (ret_cod = msg.body.open_16.answer.rc))
            THROW(ERROR_FROM_SERVER);

        /* prepare the next message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_OPEN;
        msg.header.pvs.step = 24;
        msg.body.open_24.xa_open_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_open_24_xa_open_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_open function */
        *txrc = TX_OK;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_open_24_xa_open_execs_s record;
            long xa_open_flags = TMNOFLAGS;
            int rc;

            record.xa_info = (xmlChar *)act_rsrmgr->generic->xa_open_info;
            record.rmid = i;
            record.flags = xa_open_flags;
            record.rc = rc = act_rsrmgr->xa_switch->xa_open_entry(
                (char *)record.xa_info, record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_open: xa_open_entry('%s', %d, %ld) = %d\n",
                        (char *)record.xa_info, record.rmid, record.flags,
                        record.rc));
            g_array_append_val(msg.body.open_24.xa_open_execs, record);

            /* compute TX_*; the implemented behaviour is the most restrictive;
               see RFNF 2907143 https://sourceforge.net/tracker/?func=detail&aid=2907143&group_id=257602&atid=1231748 */
            switch (record.rc) {
                case XA_OK:
                    break;
                case XAER_RMERR:
                    if (*txrc == TX_OK)
                        *txrc = TX_ERROR;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    break;
                case XAER_ASYNC:
                    THROW(ASYNC_NOT_IMPLEMENTED);
                    break;
                default:
                    THROW(UNEXPECTED_XA_RC);
            }
        } /* for (i=0; ...) */
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR2);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.open_24.xa_open_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes to the server for step 24\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR2);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR1:
                break;
            case SEND_ERROR1:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case RECV_ERROR:
                ret_cod = LIXA_RC_RECV_ERROR;
                break;
            case MSG_DESERIALIZE_ERROR:
                break;
            case ERROR_FROM_SERVER:
                ret_cod += LIXA_RC_ERROR_FROM_SERVER_OFFSET;
                break;
            case MSG_SERIALIZE_ERROR2:
                break;
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;                
            case SEND_ERROR2:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_close(client_status_t *cs, int *txrc)
{
    enum Exception { MSG_SERIALIZE_ERROR
                     , SEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_close\n"));
    TRY {
        struct lixa_msg_s msg;
        int fd;
        size_t buffer_size = 0;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_CLOSE;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.close_8.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_close_8_rsrmgr_s),
            global_ccc.actconf.rsrmgrs->len);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct lixa_msg_body_close_8_rsrmgr_s record;
            record.rmid = i;
            g_array_append_val(msg.body.close_8.rsrmgrs, record);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        /* this object contains a lot of references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.close_8.rsrmgrs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_close: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR:
                *txrc = TX_FAIL;
                break;
            case SEND_ERROR:
                *txrc = TX_ERROR;
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case NONE:
                *txrc = TX_OK;
                ret_cod = LIXA_RC_OK;
                break;
            default:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


