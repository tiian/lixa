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
#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_config.h>
#include <server_listener.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_LISTENER



int server_listener(const struct server_config_s *sc,
                    struct listener_status_array_s *lsa)
{
    enum Exception { MALLOC_ERROR
                     , INVALID_ADDRESS_ERROR
                     , SOCKET_ERROR
                     , SETSOCKOPT_ERROR
                     , BIND_ERROR
                     , LISTEN_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    int i = 0;
    int n = sc->listeners.n;
    int sock_opt = 1;
    in_addr_t address;
    
    LIXA_TRACE(("server_listener\n"));
    TRY {
        /* allocate status structure */
        if (NULL == (lsa->array = (struct listener_status_s *)malloc(
                         sizeof(struct listener_status_s) * n)))
            THROW(MALLOC_ERROR);
        lsa->n = n;

        for (i=0; i<n; ++i) {
            /* reset listeners status */
            lsa->array[i].fd = 0;
            memset(&(lsa->array[i].servaddr), 0, sizeof(struct sockaddr_in));
            lsa->array[i].servaddr.sin_family = sc->listeners.array[i].domain;
            
            /* resolving address */
            LIXA_TRACE(("server_listener: resolving address '%s' for "
                        "listener # %d\n", sc->listeners.array[i].address, i));
            if ((in_addr_t)-1 == (address = inet_addr(
                                      sc->listeners.array[i].address))) {
                struct hostent *hptr;
                
                LIXA_TRACE(("server_listener: address '%s' is not a valid "
                            "dotted-decimal string for listener # %d; "
                            "trying to resolve as a network name...\n",
                            sc->listeners.array[i].address, i));
                if (NULL == (hptr = gethostbyname(
                                 sc->listeners.array[i].address))) {
                    LIXA_TRACE(("server_listener: address '%s' is not a valid "
                                "network name too, this configuration can "
                                "not be executed\n",
                                sc->listeners.array[i].address, i));
                    THROW(INVALID_ADDRESS_ERROR);
                } else {
                    LIXA_TRACE(("server_listener: OK, '%s' can be resolved "
                                "by this host\n",
                                sc->listeners.array[i].address));
                    lsa->array[i].servaddr.sin_addr.s_addr =
                        *((in_addr_t *)hptr->h_addr_list[0]);
                }
            } else {
                lsa->array[i].servaddr.sin_addr.s_addr = address;
            }
            /* set port */
            lsa->array[i].servaddr.sin_port = htons(
                sc->listeners.array[i].port);
            
            /* create socket */
            LIXA_TRACE(("server_listener: creating socket for listener # %d\n",
                        i));
            if (0 > (lsa->array[i].fd = socket(
                         sc->listeners.array[i].domain, SOCK_STREAM, 0)))
                THROW(SOCKET_ERROR);
            LIXA_TRACE(("server_listener: socket for listener %d is %d\n",
                        i, lsa->array[i].fd));
            /* set reuse option */
            LIXA_TRACE(("server_listener: setting SO_REUSE option for "
                        "listener # %d\n", i));
            if (0 != setsockopt(lsa->array[i].fd, SOL_SOCKET, SO_REUSEADDR,
                                (void *)(&sock_opt), sizeof(sock_opt)))
                THROW(SETSOCKOPT_ERROR);

            /* bind the socket */
            if (0 != bind(lsa->array[i].fd,
                          (struct sockaddr *) &(lsa->array[i].servaddr),
                          sizeof(struct sockaddr_in)))
                THROW(BIND_ERROR);

            /* finally, listen to this new socket! */
            if (0 != listen(lsa->array[i].fd, SOMAXCONN))
                THROW(LISTEN_ERROR);
        } /* for (i=0; i<n; ++i) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case INVALID_ADDRESS_ERROR:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case SOCKET_ERROR:
                ret_cod = LIXA_RC_SOCKET_ERROR;
                break;
            case SETSOCKOPT_ERROR:
                ret_cod = LIXA_RC_SETSOCKOPT_ERROR;
                break;
            case BIND_ERROR:
                ret_cod = LIXA_RC_BIND_ERROR;
                break;
            case LISTEN_ERROR:
                ret_cod = LIXA_RC_LISTEN_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_listener/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

