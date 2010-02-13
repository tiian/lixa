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
#ifndef SERVER_LISTENER_H
#define SERVER_LISTENER_H



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
