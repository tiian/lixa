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



#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif



#include <tx.h>
#include <lixa_errors.h>
#include <lixa_config.h>
#include <lixa_trace.h>
#include <client_conn.h>
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_CONN



int client_connect(client_status_coll_t *csc,
                   client_config_coll_t *ccc)
{
    enum Exception { GET_TRNMGR_ERROR
                     , SOCKET_ERROR
                     , CONNECT_ERROR
                     , CLIENT_STATUS_COLL_GET_CS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int out_socket;
    struct trnmgr_config_s *tc;
    
    LIXA_TRACE(("client_connect\n"));
    TRY {
        client_status_t *cs;

        /* search connection parameters */
        if (NULL == (tc = client_config_get_trnmgr(ccc)))
            THROW(GET_TRNMGR_ERROR);

        /* create new socket */
        if (LIXA_NULL_FD == (out_socket = socket(tc->domain, SOCK_STREAM, 0)))
            THROW(SOCKET_ERROR);

        LIXA_TRACE(("client_connect: connecting socket %d to server '%s' port "
                    IN_PORT_T_FORMAT "\n", out_socket, tc->address, tc->port));
        if (0 != connect(out_socket, (struct sockaddr *)&ccc->serv_addr,
                         sizeof(struct sockaddr_in)))
            THROW(CONNECT_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_status_coll_get_cs(csc, &cs)))
            THROW(CLIENT_STATUS_COLL_GET_CS_ERROR);
        client_status_set_sockfd(cs, out_socket);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_TRNMGR_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case SOCKET_ERROR:
                break;
            case CONNECT_ERROR:
                ret_cod = LIXA_RC_CONNECT_ERROR;
                break;
            case CLIENT_STATUS_COLL_GET_CS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_connect/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_disconnect(client_status_coll_t *csc)
{
    enum Exception { COLL_GET_CS_ERROR
                     , SHUTDOWN_WR_ERROR
                     , RECV_ERROR
                     , CLOSE_ERROR
                     , COLL_DEL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_disconnect\n"));
    TRY {
        int fd;
        client_status_t *cs;
        char dummy_buffer[1000];

        /* retrieve the socket */
        if (LIXA_RC_OK != (ret_cod = client_status_coll_get_cs(csc, &cs)))
            THROW(COLL_GET_CS_ERROR);
        fd = client_status_get_sockfd(cs);
        
        /* close write half socket */
        LIXA_TRACE(("client_disconnect: shutdown write, fd = %d\n", fd));
        if (0 != shutdown(fd, SHUT_WR))
            THROW(SHUTDOWN_WR_ERROR);
        
        /* wait server reply */
        if (0 != (recv(fd, dummy_buffer, sizeof(dummy_buffer), 0)))
            THROW(RECV_ERROR);
        
        LIXA_TRACE(("client_disconnect: close socket, fd = %d\n", fd));
        if (0 != close(fd))
            THROW(CLOSE_ERROR);
        
        if (LIXA_RC_OK != (ret_cod = client_status_coll_del(csc)))
            THROW(COLL_DEL_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_ERROR:
                break;
            case SHUTDOWN_WR_ERROR:
                ret_cod = LIXA_RC_SHUTDOWN_ERROR;
                break;
            case RECV_ERROR:
                ret_cod = LIXA_RC_RECV_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case COLL_DEL_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_disconnect/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

