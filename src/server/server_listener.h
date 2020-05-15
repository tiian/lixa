/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_LISTENER_H
# define SERVER_LISTENER_H



#include <config.h>



#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif



#include <server_status.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_LISTENER



/**
 * It contains the status of a listener
 */
struct listener_status_s {
    /**
     * file descriptor associated to the socket used to listen
     */
    int                  fd;
    /**
     * Broken down address
     */
    struct sockaddr_in   servaddr;
};



/**
 * It contains the status of all listeners
 */
struct listener_status_array_s {
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct listener_status_s *array;
    /**
     * This pointer is a reference to a structure allocated in a different
     * place: the pointed structure is common to listeners and managers
     */
    struct thread_status_array_s  *tsa;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Start the listener(s) configured for the server in configuration file
     * @param sc IN server configuration structure
     * @param lsa OUT status of all the listeners
     * @param tsa IN/OUT status of listener thread
     * @return a standardized return code
     */
    int server_listener(const struct server_config_s *sc,
                        struct listener_status_array_s *lsa,
                        struct thread_status_array_s *tsa);



    /**
     * Cicle on poll to receive incoming clients and dispatching to managers
     * @param sc IN server configuration structure
     * @param lsa IN/OUT status of all the listeners
     * @param tsa IN/OUT thread status of the listener
     * @return a standardized return code     
     */
    int server_listener_loop(const struct server_config_s *sc,
                             struct listener_status_array_s *lsa,
                             struct thread_status_array_s *tsa);



    /**
     * Find out the first available manager
     * @param tsa IN listener thread status
     * @param manager_id OUT id of the manager will receive the client
     * @return a standardized return code
     */
    int server_listener_find_manager(const struct thread_status_array_s *tsa,
                                     int *manager_id);



    /**
     * Install the LIXA signal handler
     * @return a standardized reason code
     */
    int server_listener_signal(void);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_LISTENER_H */
