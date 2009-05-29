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
#ifndef SERVER_MANAGER_H
# define SERVER_MANAGER_H



#include <config.h>



#include <server_config.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_MANAGER



#define READ_BUFFER_SIZE 4096



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Start the managers configured for this server
     * @param sc IN server configuration structure
     * @param tpa IN threads' communication pipes
     * @param tsa OUT status of all threads
     * @return a standardized return code
     */
    int server_manager(struct server_config_s *sc,
                       struct thread_pipe_array_s *tpa,
                       struct thread_status_array_s *tsa);



    /**
     * This is the initial function of every new manager thread
     * @param void_ts IN status of this thread
     */
    void *server_manager_thread(void *void_ts);



    /**
     * This method manages POLLIN event on file descriptors when it happens
     * due to internal control actions
     * @param ts IN/OUT thread status
     * @param fd IN file descriptor raised the POLLIN event
     * @return a standardized return code
     */
    int server_manager_pollin_ctrl(struct thread_status_s *ts, int fd);

    
    
    /**
     * This method manages POLLIN event on file descriptors when it happens
     * due to client actions
     * @param ts IN/OUT thread status
     * @param slot_id IN the slot associated to the file descriptor raised
     *                   the POLLIN event
     * @return a standardized return code
     */
    int server_manager_pollin_data(struct thread_status_s *ts, size_t slot_id);



    /**
     * Free unused tail slot in thread status structure
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @return a standardized return code
     */
    int server_manager_free_slots(struct thread_status_s *ts, size_t slot_id);

    
    
    /**
     * Process XML message coming from client
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @param buf IN buffer read from socket
     * @param read_bytes IN number of bytes read from socket
     * @return a standardized return code
     */
    int server_manager_XML_proc(struct thread_status_s *ts, size_t slot_id,
                                const char *buf, ssize_t read_bytes);

    
    
    /**
     * Prepare the data structure used for calling poll system function
     * @param ts IN thread status structure
     * @param new_fd IN the new file descriptor must be polled
     * @param place OUT the place inside poll_array and client_array assigned
     *                  to the new session
     * @return a standardized return code
     */
    int server_manager_add_poll(struct thread_status_s *ts,
                                int new_fd, nfds_t *place);

    

    /**
     * Do all stuff necessary to initialize the status for the new incoming
     * client
     * @param ts IN thread status structure
     * @param fd IN the new file descriptor must be polled
     * @param place IN the place inside client_array will be used to store
     *                 the reference to mapped memory status
     * @return a standardized return code     
     */
    int server_manager_new_client(struct thread_status_s *ts, int fd,
                                  nfds_t place);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_MANAGER_H */
