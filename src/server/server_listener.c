/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_config.h>
#include <server_listener.h>
#include <server_messages.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_LISTENER



int server_listener(const struct server_config_s *sc,
                    struct listener_status_array_s *lsa,
                    struct thread_status_array_s *tsa)
{
    enum Exception { MALLOC_ERROR
                     , INVALID_ADDRESS_ERROR
                     , SOCKET_ERROR
                     , SETSOCKOPT_ERROR
                     , BIND_ERROR
                     , LISTEN_ERROR
                     , LISTENER_LOOP_ERROR
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

        if (LIXA_RC_OK != (ret_cod = server_listener_loop(sc, lsa, tsa)))
            THROW(LISTENER_LOOP_ERROR);
        
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
            case LISTENER_LOOP_ERROR:
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



int server_listener_loop(const struct server_config_s *sc,
                         struct listener_status_array_s *lsa,
                         struct thread_status_array_s *tsa)
{
    enum Exception { MALLOC_ERROR
                     , POLL_ERROR
                     , NETWORK_EVENT_ERROR
                     , ACCEPT_ERROR
                     , FIND_MANAGER_ERROR
                     , WRITE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_listener_loop\n"));
    TRY {
        /* prepare pool array */
        nfds_t n = (nfds_t)lsa->n + 1;
        nfds_t i;
        int ready_fd, found_fd;
        struct thread_status_s *ts = &(tsa->array[0]);

        if (NULL == (ts->poll_array = malloc(
                         n * sizeof(struct pollfd))))
            THROW(MALLOC_ERROR);
        ts->poll_size = n;
        
        for (i = 0; i < n; ++i) {
            if (!i)
                /* control pipe */
                ts->poll_array[i].fd = ts->tpa->array[ts->id].pipefd[0];
            else
                /* listening socket */
                ts->poll_array[i].fd = lsa->array[i - 1].fd;
            ts->poll_array[i].events = POLLIN;
        } /* for (i = 0; i < n; ++i) */

        while (TRUE) {
            LIXA_TRACE(("server_listener_loop: entering poll...\n"));
            if (0 >= (ready_fd = poll(ts->poll_array, ts->poll_size, -1)))
                THROW(POLL_ERROR);
            LIXA_TRACE(("server_listener_loop: ready file descriptors = %d\n",
                        ready_fd));
            /* look for ready file descriptors */
            found_fd = 0;
            for (i = 0; i < n; ++i) {
                struct sockaddr_in cliaddr;
                socklen_t clilen;
                int conn_fd;
                
                LIXA_TRACE(("server_listener_loop: slot = " NFDS_T_FORMAT
                            ", fd = %d, POLLIN = %d, POLLERR = %d, "
                            "POLLHUP = %d, POLLNVAL = %d\n",
                            i, ts->poll_array[i].fd,
                            ts->poll_array[i].revents & POLLIN,
                            ts->poll_array[i].revents & POLLERR,
                            ts->poll_array[i].revents & POLLHUP,
                            ts->poll_array[i].revents & POLLNVAL));

                if (ts->poll_array[i].revents &
                    (POLLERR | POLLHUP | POLLNVAL)) {
                    LIXA_TRACE(("server_listener_loop: unespected "
                                "network event on slot " NFDS_T_FORMAT
                                ", fd = %d\n", i, ts->poll_array[i].fd));
                    /* @@@ this piece of code must be improved */
                    THROW(NETWORK_EVENT_ERROR);
                }
                
                if (ts->poll_array[i].revents & POLLIN) {
                    int manager_id = 0;
                    struct srv_msg_s msg;
                    
                    clilen = sizeof(cliaddr);
                    if (0 > (conn_fd = accept(ts->poll_array[i].fd,
                                              (struct sockaddr *)&cliaddr,
                                              &clilen)))
                        THROW(ACCEPT_ERROR);
                    LIXA_TRACE(("server_listener_loop: accepted new incoming "
                                "connection from address '%s', port "
                                IN_PORT_T_FORMAT ", fd = %d\n",
                                inet_ntoa(cliaddr.sin_addr),
                                ntohs(cliaddr.sin_port), conn_fd));
                    /* choose a manager for this client */
                    if (LIXA_RC_OK != (ret_cod = server_listener_find_manager(
                                           tsa, &manager_id)))
                        THROW(FIND_MANAGER_ERROR);

                    /* prepare the message to signal the manager */
                    msg.type = SRV_MSG_TYPE_NEW_CLIENT;
                    msg.body.nc.fd = conn_fd;

                    if (sizeof(msg) != write(
                            ts->tpa->array[manager_id].pipefd[1], &msg,
                            sizeof(msg)))
                        THROW(WRITE_ERROR);
                    
                    found_fd++;
                }

                /* @@@ */
                
                if (ready_fd == found_fd)
                    break; /* the loop is now useless */
            } /* for (i = 0; i < n; ++i) */
        } /* while (TRUE) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case POLL_ERROR:
                ret_cod = LIXA_RC_POLL_ERROR;
                break;
            case NETWORK_EVENT_ERROR:
                ret_cod = LIXA_RC_NETWORK_EVENT_ERROR;
                break;
            case ACCEPT_ERROR:
                ret_cod = LIXA_RC_ACCEPT_ERROR;
                break;
            case FIND_MANAGER_ERROR:
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_listener_loop/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_listener_find_manager(const struct thread_status_array_s *tsa,
                                 int *manager_id)
{
    enum Exception { OUT_OF_RANGE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_listener_find_manager\n"));
    TRY {
        int i;
        size_t current_min;
        int    current_pos;

        if (tsa->n <= 1)
            THROW(OUT_OF_RANGE);
        
        current_pos = 1;
        current_min = tsa->array[current_pos].active_clients;

        /* first slot is skipped because contains listener thread */
        for (i = 1; i < tsa->n; ++i) {
            LIXA_TRACE(("server_listener_find_manager: id = %i, "
                        "active_clients = " SIZE_T_FORMAT "\n",
                        i, tsa->array[i].active_clients));
            if (tsa->array[i].active_clients < current_min ||
                (tsa->array[i].active_clients == current_min &&
                 !(rand()%(tsa->n - 1)))) {
                current_pos = i;
                current_min = tsa->array[current_pos].active_clients;
            }
        }
        LIXA_TRACE(("server_listener_find_manager: choosed id = %i, "
                    "active_clients = " SIZE_T_FORMAT "\n",
                    current_pos, current_min));
        *manager_id = current_pos;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                LIXA_TRACE(("server_listener_find_manager: thread status "
                            "array has less than 2 elements, no manager "
                            "can be choosed\n"));
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_listener_find_manager/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
