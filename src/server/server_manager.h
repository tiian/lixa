/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_MANAGER_H
# define SERVER_MANAGER_H



#include <config.h>



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include "server_config.h"
#include "server_messages.h"
#include "server_thread_status.h"
#include "srvr_rcvr_tbl.h"
#include "server_trans_tbl.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_MANAGER



#define READ_BUFFER_SIZE 4096


/**
 * This mutex is used to avoid two or more threads try to perform state file
 * synchronization in parallel: this is to avoid disk enqueing (see the
 * logic inside @ref server_manager_thread function)
 */
extern GMutex state_file_synchronization;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    /**
     * Start the managers configured for this server
     * @param[in] sc server configuration structure
     * @param[in] tpa threads' communication pipes
     * @param[out] tsa status of all threads
     * @param[in,out] srt server recovery table
     * @param stt server transaction table
     * @param[in] tsds dump specifications
     * @param[in] tsrs recovery specifications
     * @param[in] mmode maintenance mode: only privileged clients can connect
     * @return a standardized return code
     */
    int server_manager(struct server_config_s *sc,
                       struct thread_pipe_array_s *tpa,
                       struct thread_status_array_s *tsa,
                       srvr_rcvr_tbl_t *srt,
                       server_trans_tbl_t *stt,
                       const struct ts_dump_spec_s *tsds,
                       const struct ts_recovery_spec_s *tsrs, int mmode);

    

    /**
     * Initialize the pipes necessary to thread intercommunication
     * @param[in,out] tpa object reference
     * @return a standardized return code
     */
    int server_pipes_init(struct thread_pipe_array_s *tpa);

    

    /**
     * This is the initial function of every new manager thread
     * @param void_ts IN status of this thread
     */
    void *server_manager_thread(void *void_ts);


    
    /**
     * Clean-up the server environment: <br>
     * - reset element in thread pipe array (to avoid another thread
     *   communication) <br>
     * - close file descriptors (all: sockets and control pipe) <br>
     * - send notification message to all the other threads <br>
     * @param ts IN thread status reference
     */
    void server_manager_thread_cleanup(struct thread_status_s *ts);

    

    /**
     * This method manages POLLIN event on file descriptors when it happens
     * due to internal control actions
     * @param[in,out] ts thread status
     * @param[in] fd file descriptor raised the POLLIN event
     * @return a standardized return code
     */
    int server_manager_pollin_ctrl(struct thread_status_s *ts, int fd);


    
    /**
     * This method manages POLLIN event on file descriptors when it happens
     * due to client actions
     * @param[in,out] ts thread status
     * @param[in] slot_id the slot associated to the file descriptor raised
     *                   the poll (POLLIN) event
     * @return a standardized return code
     */
    int server_manager_pollin_data(struct thread_status_s *ts, size_t slot_id);

    

    /**
     * Drop client connection
     * @param[in,out] ts thread status
     * @param[in] slot_id the slot associated to the file descriptor raised
     *                   the poll event
     * @return a standardized return code
     */
    int server_manager_drop_client(struct thread_status_s *ts, size_t slot_id);


    
    /**
     * Switch the connected client to a different thread, phase 1:
     * the source thread send a message with the switch information to the
     * destination thread and freeze the connected client in "control only"
     * mode
     * @param[in,out] ts thread status
     * @param[in] slot_id the slot associated to the file descriptor connected
     *                to the client
     * @return a standardized return code
     */
    int server_manager_switch_1(struct thread_status_s *ts,
                                size_t slot_id);

    

    /**
     * Switch the connected client to a different thread, phase 2:
     * the destination thread receives a message with the switch information
     * @param[in,out] ts thread status
     * @param[in] msg the request message sent by source thread
     * @return a standardized return code
     */
    int server_manager_switch_2(struct thread_status_s *ts,
                                const struct srv_msg_s *msg);


    
    /**
     * Switch the connected client to a different thread, phase 3:
     * the source thread receives the answer from the destination thread
     * @param[in,out] ts thread status
     * @param[in] msg the request message sent by source thread
     * @return a standardized return code
     */
    int server_manager_switch_3(struct thread_status_s *ts,
                                const struct srv_msg_s *msg);


    
    /**
     * Shutdown the current server
     * @param[in,out] ts thread status
     * @param[in] msg the shutdown message (sent by the signal handler routine)
     * @return a standardized return code
     */
    int server_manager_shutdown(struct thread_status_s *ts,
                                const struct srv_msg_s *msg);


    
    /**
     * This method manages POLLOUT event on file descriptors when a session
     * previously asked to send back data to its client
     * @param[in,out] ts thread status
     * @param[in] slot_id the slot associated to the file descriptor raised
     *                   the POLLOUT event
     * @return a standardized return code
     */
    int server_manager_pollout(struct thread_status_s *ts, size_t slot_id);


    
    /**
     * Free unused tail slot in thread status structure
     * @param[in,out] ts thread status structure
     * @param[in] slot_id id of the slot must be freed
     * @return a standardized return code
     */
    int server_manager_free_slots(struct thread_status_s *ts, size_t slot_id);

    

    /**
     * Process the input message arrived from the client and prepare the output
     * message if necessary
     * @param[in,out] ts thread status structure
     * @param[in] slot_id the slot associated to the file descriptor raised the
     *                   POLLIN event
     * @param[in,out] buf buffer read from socket
     * @param[in] read_bytes number of bytes read from socket
     * @return a standardized return code
     */
    int server_manager_msg_proc(struct thread_status_s *ts,
                                size_t slot_id, char *buf, ssize_t read_bytes);


    
    /**
     * Process the input message arrived from the client
     * @param[in,out] ts thread status structure
     * @param[in] slot_id the slot associated to the file descriptor raised the
     *                   POLLIN event
     * @param[in,out] buf buffer read from socket
     * @param[in] read_bytes number of bytes read from socket
     * @param[out] lmo message should be returned to the client
     * @return a standardized return code
     */
    int server_manager_inmsg_proc(struct thread_status_s *ts, size_t slot_id,
                                  char *buf, ssize_t read_bytes,
                                  struct lixa_msg_s *lmo);


    
    /**
     * Prepare the output message must be sent to the client
     * @param[in,out] ts thread status structure
     * @param[in] slot_id the slot associated to the file descriptor raised the
     *            POLLIN event (current session client)
     * @param[in] list_size number of clients that must be notified by the
     *            message
     * @param[in] list of the clients that must be notified
     * @param[in] rc return code of the previous operations must be returned
     *              to the client
     * @return a standardized return code
     */
    int server_manager_outmsg_prep(struct thread_status_s *ts,
                                   size_t slot_id, size_t list_size,
                                   const size_t *list, int rc);

    

    /**
     * Check if the connected thread should deal with recovery pending
     * transactions
     * @param[in] ts thread status structure
     * @param[in] lmi deserialized message sent by client
     * @param[out] recovery_pending boolean: TRUE if there are recovery
     *                         pending transactions
     * @return a reason code
     */
    int server_manager_check_recovery(struct thread_status_s *ts,
                                      const struct lixa_msg_s *lmi,
                                      int *recovery_pending);


    
    /**
     * Prepare the data structure used for calling poll system function
     * @param[in] ts thread status structure
     * @param[in] new_fd the new file descriptor must be polled
     * @param[out] place the place inside poll_array and client_array assigned
     *                  to the new session
     * @return a standardized return code
     */
    int server_manager_add_poll(struct thread_status_s *ts,
                                int new_fd, nfds_t *place);


    
    /**
     * Fix the status of poll_array: sessions must send back data to client
     * will be input disabled; sessions don't have to send back data to client
     * will be output disabled
     * @param[in,out] ts thread status structure
     * @return a standardized return code
     */
    int server_manager_fix_poll(struct thread_status_s *ts);

    

    /**
     * Do all stuff necessary to initialize the status for the new incoming
     * client
     * @param[in,out] ts thread status structure
     * @param[in] fd the new file descriptor must be polled
     * @param[in] place the place inside client_array will be used to store
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
