/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
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



#include "tx.h"
#include "lixa_errors.h"
#include "lixa_config.h"
#include "lixa_trace.h"
#include "lixa_utils.h"
#include "lixa_syslog.h"
#include "client_conn.h"
#include "client_status.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_CONN



int client_connect(client_status_t *cs,
                   client_config_coll_t *ccc)
{
    enum Exception {
        GET_STTSRV_ERROR,
        SOCKET_ERROR,
        CONNECT_ERROR,
        SETSOCKOPT_ERROR,
        SESSION_INIT_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int out_socket;
    struct sttsrv_config_s *tc;
    
    LIXA_TRACE(("client_connect\n"));
    TRY {
        int sock_opt = 1;

        /* search connection parameters */
        if (NULL == (tc = client_config_get_sttsrv(ccc)))
            THROW(GET_STTSRV_ERROR);

        /* create new socket */
        if (LIXA_NULL_FD == (out_socket = socket(tc->domain, SOCK_STREAM, 0)))
            THROW(SOCKET_ERROR);

        LIXA_TRACE(("client_connect: connecting socket %d to server '%s' port "
                    IN_PORT_T_FORMAT "\n", out_socket, tc->address, tc->port));
        if (0 != connect(out_socket, (struct sockaddr *)&ccc->serv_addr,
                         sizeof(struct sockaddr_in))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXC002E, tc->address, tc->port));
            THROW(CONNECT_ERROR);
        }
        /* disable Nagle's algorithm to reduce latency */
        if (0 != setsockopt(out_socket, IPPROTO_TCP, TCP_NODELAY,
                            (void *)(&sock_opt), sizeof(sock_opt)))
            THROW(SETSOCKOPT_ERROR);
        /* initialize session id */
        if (LIXA_RC_OK != (ret_cod = lixa_session_init(
                               &cs->session, out_socket, TRUE)))
            THROW(SESSION_INIT_ERROR);
        
        LIXA_TRACE(("client_connect: cs=%p, sid='%s'\n", cs,
                    lixa_session_get_sid(&cs->session)));
        LIXA_CRASH(LIXA_CRASH_POINT_CLIENT_CONNECT_1,
                   client_status_get_crash_count(cs));
        
        client_status_set_sockfd(cs, out_socket);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_STTSRV_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case SOCKET_ERROR:
                break;
            case CONNECT_ERROR:
                ret_cod = LIXA_RC_CONNECT_ERROR;
                break;
            case SETSOCKOPT_ERROR:
                ret_cod = LIXA_RC_SETSOCKOPT_ERROR;
                break;
            case SESSION_INIT_ERROR:
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_disconnect_thread(client_status_coll_t *csc)
{
    enum Exception {
        COLL_GET_CS_ERROR,
        CLIENT_DISCONNECT,
        COLL_DEL_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_disconnect_thread\n"));
    TRY {
        client_status_t *cs;

        /* get the status associated to the current thread */
        if (LIXA_RC_OK != (ret_cod = client_status_coll_get_cs(csc, &cs)))
            THROW(COLL_GET_CS_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_disconnect(cs)))
            THROW(CLIENT_DISCONNECT);
        
        if (client_status_is_failed(cs)) {
            LIXA_TRACE(("client_disconnect_thread: client_status_is_failed()"
                        "==TRUE, bypassing status deletion\n"));
        } else if (LIXA_RC_OK != (ret_cod = client_status_coll_del(csc)))
            THROW(COLL_DEL_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_ERROR:
            case CLIENT_DISCONNECT:
            case COLL_DEL_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_disconnect_thread/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_disconnect(client_status_t *cs)
{
    enum Exception {
        SHUTDOWN_WR_ERROR,
        CLOSE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_disconnect\n"));
    TRY {
        int fd;
        char dummy_buffer[1000];
        struct pollfd fds[1];
        int ready_fd;

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);
        
        /* close write half socket */
        LIXA_TRACE(("client_disconnect: shutdown write, fd = %d\n", fd));
        if (0 != shutdown(fd, SHUT_WR))
            THROW(SHUTDOWN_WR_ERROR);

        /* to avoid a possible indefinite wait, poll is used */
        fds[0].fd = fd;
        fds[0].events = POLLIN | POLLHUP | POLLERR;
        fds[0].revents = 0;
        ready_fd = poll(fds, 1, LIXA_CLIENT_CONNECTION_TIMEOUT_DEFAULT);
        if (1 == ready_fd) {
            LIXA_TRACE(("client_disconnect: poll returned 0x%x for fd %d\n",
                        fds[0].revents, fds[0].fd));
            if ((POLLHUP | POLLERR) & fds[0].revents) {
                LIXA_TRACE(("client_disconnect: POLLHUP/POLLERR...\n"));
            } else if (POLLIN & fds[0].revents)
                recv(fd, dummy_buffer, sizeof(dummy_buffer), 0);
        } else if (0 == ready_fd) {
            LIXA_TRACE(("client_disconnect: poll time out (%d ms) exceeded\n",
                        LIXA_CLIENT_CONNECTION_TIMEOUT_DEFAULT));
        }
        LIXA_TRACE(("client_disconnect: close socket, fd = %d\n", fd));
        if (0 != close(fd))
            THROW(CLOSE_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SHUTDOWN_WR_ERROR:
                ret_cod = LIXA_RC_SHUTDOWN_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
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
    LIXA_TRACE_STACK();
    return ret_cod;
}
