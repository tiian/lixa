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
#ifndef SERVER_REPLY_H
# define SERVER_REPLY_H



#include <config.h>



#include <lixa_trace.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_REPLY



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Send the default output message to the client
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @param lmo IN message will be returned to the client
     * @return a standardized return code
     */
    int server_reply_default(struct thread_status_s *ts, size_t slot_id,
                             struct lixa_msg_s *lmo);


    
    /**
     * Send the output message related to "end" verb to the client
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @param lmo IN message will be returned to the client
     * @param rc IN return code of the previous operations must be returned
     *              to the client
     * @return a standardized return code
     */
    int server_reply_end(struct thread_status_s *ts, size_t slot_id,
                         struct lixa_msg_s *lmo, int rc);


    
    /**
     * Send the output message related to "open" verb to the client
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @param lmo IN message will be returned to the client
     * @param rc IN return code of the previous operations must be returned
     *              to the client
     * @return a standardized return code
     */
    int server_reply_open(struct thread_status_s *ts, size_t slot_id,
                          struct lixa_msg_s *lmo, int rc);


    
    /**
     * Send the output message related to "prepare" verb to the client
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @param lmo IN message will be returned to the client
     * @param rc IN return code of the previous operations must be returned
     *              to the client
     * @return a standardized return code
     */
    int server_reply_prepare(struct thread_status_s *ts, size_t slot_id,
                             struct lixa_msg_s *lmo, int rc);


    
    /**
     * Send the output message related to "start" verb to the client
     * @param ts IN/OUT thread status structure
     * @param slot_id IN id of the slot must be freed
     * @param lmo IN message will be returned to the client
     * @param rc IN return code of the previous operations must be returned
     *              to the client
     * @return a standardized return code
     */
    int server_reply_start(struct thread_status_s *ts, size_t slot_id,
                           struct lixa_msg_s *lmo, int rc);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_REPLY_H */
