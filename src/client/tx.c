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



#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include <tx.h>
#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg.h>
#include <client_conn.h>
#include <client_config.h>
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



int tx_open(void)
{
    enum Exception { CLIENT_CONFIG_ERROR
                     , CLIENT_CONNECT_ERROR
                     , COLL_GET_CS_ERROR
                     , BUFFER_TOO_SMALL
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("tx_open\n"));
    TRY {
        int fd;
        client_status_t cs;
        char xml_buffer[XML_BUFFER_SIZE];
        
        if (LIXA_RC_OK != (ret_cod = client_config(&global_ccc)))
            THROW(CLIENT_CONFIG_ERROR);
        if (LIXA_RC_OK != (ret_cod = client_connect(&global_csc, &global_ccc)))
            THROW(CLIENT_CONNECT_ERROR);

        /* retrive the socket */
        if (LIXA_RC_OK != (ret_cod = client_status_coll_get_cs(
                               &global_csc, &cs)))
            THROW(COLL_GET_CS_ERROR);
        fd = client_status_get_sockfd(&cs);

        /* @@@ the real logic must be put here */
        
        if (sizeof(xml_buffer) <= snprintf(
                xml_buffer, XML_BUFFER_SIZE, XML_MSG_EX_OPEN1, 
                global_ccc.profile)) {
            LIXA_TRACE(("tx_open: xml_buffer to small. INTERNAL ERROR\n"));
            THROW(BUFFER_TOO_SMALL);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_CONFIG_ERROR:
                ret_cod = TX_FAIL;
                break;
            case CLIENT_CONNECT_ERROR:
                ret_cod = TX_ERROR;
                break;
            case COLL_GET_CS_ERROR:
                break;
            case BUFFER_TOO_SMALL:
                ret_cod = TX_FAIL;
                break;
            case NONE:
                ret_cod = TX_OK;
                break;
            default:
                ret_cod = TX_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("tx_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    assert(ret_cod == TX_OK);
    return ret_cod;
}



int tx_close(void)
{
    enum Exception { CLIENT_DISCONNECT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("tx_close\n"));
    TRY {
        if (LIXA_RC_OK != (ret_cod = client_disconnect(&global_csc)))
            THROW(CLIENT_DISCONNECT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_DISCONNECT_ERROR:
                ret_cod = TX_ERROR;
                break;
            case NONE:
                ret_cod = TX_OK;
                break;
            default:
                ret_cod = TX_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("tx_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    assert(ret_cod == TX_OK);
    return ret_cod;
}

