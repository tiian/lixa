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
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_CONFIG



int server_listener(const struct server_config_s *sc,
                    struct listener_status_array_s *lsa)
{
    enum Exception { MALLOC_ERROR
                     , SOCKET_ERROR
                     , SETSOCKOPT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    int i = 0;
    int n = sc->listeners.n;
    int sock_opt = 1;
    
    LIXA_TRACE(("server_listener\n"));
    TRY {
        /* allocate status structure */
        if (NULL == (lsa->array = (struct listener_status_s *)malloc(
                         sizeof(struct listener_status_s) * n)))
            THROW(MALLOC_ERROR);

        for (i=0; i<n; ++i) {
            /* reset listeners status */
            lsa->array[i].fd = 0;
            /* create socket */
            LIXA_TRACE(("server_listener: creating socket for listener # %d\n",
                        i));
            if (0 > (lsa->array[i].fd = socket(
                         sc->listeners.array[i].domain, SOCK_STREAM, 0)))
                THROW(SOCKET_ERROR);
            /* set reuse option */
            LIXA_TRACE(("server_listener: setting SO_REUSE option for "
                        "listener # %d\n", i));
            if (0 != setsockopt(lsa->array[i].fd, SOL_SOCKET, SO_REUSEADDR,
                                (void *)(&sock_opt), sizeof(sock_opt)))
                THROW(SETSOCKOPT_ERROR);

            /* @@@ */
        } /* for (i=0; i<n; ++i) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case SOCKET_ERROR:
                ret_cod = LIXA_RC_SOCKET_ERROR;
                break;
            case SETSOCKOPT_ERROR:
                ret_cod = LIXA_RC_SETSOCKOPT_ERROR;
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

