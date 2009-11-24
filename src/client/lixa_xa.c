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



#include <lixa_trace.h>
#include <lixa_errors.h>
#include <lixa_xml_msg.h>
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



int lixa_xa_open(client_status_t *cs)
{
    enum Exception { MSG_SERIALIZE_ERROR
                     , SEND_ERROR
                     , RECV_ERROR
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
        msg.header.verb = LIXA_MSG_VERB_OPEN;
        msg.header.step = 1;

        msg.body.open_1.client.profile = (xmlChar *)global_ccc.profile;
        msg.body.open_1.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_open_1_rsrmgr_s),
            global_ccc.actconf.rsrmgrs->len);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_open_1_rsrmgr_s record;
            record.rmid = i;
            record.name = act_rsrmgr->generic->name;
            g_array_append_val(msg.body.open_1.rsrmgrs, record);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes to the server\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);
        
        if (0 > (read_bytes = recv(fd, buffer, buffer_size, 0)))
            THROW(RECV_ERROR);
        LIXA_TRACE(("lixa_xa_open: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        /* @@@ a lot of stuff:
           4. loop on all the resource managers
              4a. xa_open the resource manager
              4b. send the result (asynchronously) to the server
           5. return a value to the caller
        */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR:
                break;
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case RECV_ERROR:
                ret_cod = LIXA_RC_RECV_ERROR;
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
