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
 * It's the struct used as argument to every created thread
 */
struct thread_status_s {
    /**
     * This id is not the system thread id, but an internal identificator
     * in the server scope (0 = listeners thread, 1..N = manager threads)
     */
    int id;
    /**
     * This is the system identificator of the thread
     */
    pthread_t tid;
    /**
     * This pointer is used a reference to the array of pipes must be used
     * for communication
     */
    struct thread_pipe_array_s *tpa;
    /**
     * Number of clients + 1 (control pipe) connected to this manager
     */
    nfds_t          poll_size;
    /**
     * Elements used for polling file descriptors
     */
    struct pollfd  *poll_array;
    /**
     * Exception reported by the thread (after exit)
     */
    int excp;
    /**
     * Return code reported by the thread (after exit)
     */
    int ret_cod;
    /**
     * Errno reported by the thread (after exit)
     */
    int last_errno;
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
