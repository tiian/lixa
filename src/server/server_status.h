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
#ifndef SERVER_STATUS_H
#define SERVER_STATUS_H



#include <config.h>



#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Initial size of the status file
 */
#define STATUS_FILE_INIT_SIZE 10
/**
 * Percentual increment change of the status file
 */
#define STATUS_FILE_DELTA_SIZE 10
/**
 * Magic number of the lixa status files
 */
#define STATUS_FILE_MAGIC_NUMBER 0x6110
/**
 * Level (versione) of the lixa status files
 */
#define STATUS_FILE_LEVEL 0x0001



/**
 * It contains the configuration common to any thread
 */
struct thread_pipe_s {
    /**
     * This pipe is used to receive commands from other threads
     * as specified by POSIX
     * http://www.opengroup.org/onlinepubs/009695399/toc.htm
     * pipefd[1] must be used for writing
     * pipefd[0] must be used for reading
     */
    int pipefd[2];
};



/**
 * It contains the common configuration of all activated threads
 * thread 0 is the listener thread, threads 1..n are the serving threads
 */
struct thread_pipe_array_s {
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct thread_pipe_s *array;
};



/**
 * It's the struct used to keep the status of a client
 * @@@ is this useful or can it be removed?
 */
struct server_client_status_s {
    int foo;
};



/**
 * This is the control record type and is used for first record only
 */
struct status_record_ctrl_s {
    /**
     * It's the signature of the first 4 bytes and can be used
     * by "file" utility to discover a file is a lixa status file
     */
    uint32_t magic_number;
    /**
     * Used to distinguish status files produced and used by different
     * versions of the software
     */
    uint32_t level;
    /**
     * First record of the used blocks chain (0 means the chain is empty)
     */
    uint32_t first_used_block;
    /**
     * First record of the free blocks chain (0 means the chain is empty)
     */
    uint32_t first_free_block;
    /**
     * Status file name; this is a reference to the string allocated in
     * configuration struct. This reference must be updated every time the
     * server boots up
     */
    const char *status_file;
};



/**
 * This struct is used to separate scaffolding from payload in data records;
 * all the payload data are kept by this structure
 */
struct status_record_data_payload_s {
    int foo;
};



/**
 * This is the data record type and is used for all but first records
 */
struct status_record_data_s {
    /**
     * Index of the first record in the chain (0 means the record is the last
     * in the chain)
     */
    uint32_t next_block;
    /**
     * It contains the interesting data, the persistent status of the client
     */
    struct status_record_data_payload_s payload;
};



/**
 * This union is used to store and retrieve a record from the status file
 */
union status_record_u {
    /**
     * This type of record applies to first record only: it's the control
     * block
     */
    struct status_record_ctrl_s ctrl;
    /**
     * This type of record applies to all but first record: it's the data block
     */
    struct status_record_data_s data;
};



/**
 * It's the struct used as argument to every created thread
 */
struct thread_status_s {
    /**
     * This id is not the system thread id, but an internal identificator
     * in the server scope (0 = listeners thread, 1..N = manager threads)
     */
    int                            id;
    /**
     * This is the system identificator of the thread
     */
    pthread_t                      tid;
    /**
     * This pointer is used a reference to the array of pipes must be used
     * for communication
     */
    struct thread_pipe_array_s    *tpa;
    /**
     * Size of polling array. Note: some slots may be unused because array
     * reclamation does not happen every time a client disconnects
     */
    nfds_t                         poll_size;
    /**
     * Elements used for polling file descriptors. Note: some slots may be
     * unused because array reclamation does not happen every time a client
     * disconnects
     */
    struct pollfd                 *poll_array;
    /**
     * Number of clients active for this manager
     * Note: MUST BE active_clients < poll_size because poll array contains
     *       at least che control pipe
     */
    size_t                         active_clients;
    /**
     * Array of active and not active clients
     */
    struct server_client_status_s *client_array;
    /**
     * Memory mapped file accessed as an array
     */
    union status_record_u         *status;
    /**
     * Exception reported by the thread (after exit)
     */
    int                            excp;
    /**
     * Return code reported by the thread (after exit)
     */
    int                            ret_cod;
    /**
     * Errno reported by the thread (after exit)
     */
    int                            last_errno;
};



/**
 * It contains the argument passed to all the thread
 */
struct thread_status_array_s {
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct thread_status_s *array;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Load status records from status file
     * @param sr OUT the pointer of the mapped file
     * @param status_file IN the name of the status file to be loaded
     * @return a standardized return code
     */
    int status_record_load(union status_record_u **sr,
                           const char *status_file);



    /**
     * Insert a new element in the used slot list
     * @param sr IN/OUT record status mapped file; the pointer may change if
     *                  status file resizing happens
     * @param slot OUT the index of the found free slot
     * @return a standardized return code
     */
    int status_record_insert(union status_record_u **sr,
                             uint32_t *slot);



    /**
     * Remove an element from the used slot list
     * @param sr IN/OUT record status mapped file; the pointer may change if
     *                  status file resizing happens
     * @param slot IN the index of the slot must be released
     * @return a standardized return code
     *
     */
    int status_record_delete(union status_record_u **sr,
                             uint32_t slot);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_STATUS_H */
