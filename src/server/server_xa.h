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
#ifndef SERVER_XA_H
# define SERVER_XA_H



#include <config.h>



#include <lixa_trace.h>
#include <lixa_xml_msg.h>
#include <server_status.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_XA



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Executes the logic related to "xa_close" on the server side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_close(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        uint32_t block_id);

    

    /**
     * Executes the logic related to "xa_open" on the server side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_open(struct thread_status_s *ts,
                       const struct lixa_msg_s *lmi,
                       struct lixa_msg_s *lmo,
                       uint32_t block_id);

    

    /**
     * Executes the logic related to the first step of "xa_open" on the server
     * side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_open_8(struct thread_status_s *ts,
                         const struct lixa_msg_s *lmi,
                         struct lixa_msg_s *lmo,
                         uint32_t block_id);

    

    /**
     * Executes the logic related to the third step of "xa_open" on the server
     * side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_open_24(struct thread_status_s *ts,
                          const struct lixa_msg_s *lmi,
                          uint32_t block_id);

    

    /**
     * Executes the logic related to "xa_start" on the server side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_start(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id);

    

    /**
     * Executes the logic related to the first step of "xa_start" on the
     * server side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_start_8(struct thread_status_s *ts,
                          const struct lixa_msg_s *lmi,
                          struct lixa_msg_s *lmo,
                          uint32_t block_id);

    

    /**
     * Executes the logic related to the third step of "xa_start" on the server
     * side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_start_24(struct thread_status_s *ts,
                           const struct lixa_msg_s *lmi,
                           uint32_t block_id);

    

    /**
     * Executes the logic related to "xa_end" on the server side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_end(struct thread_status_s *ts,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id);

    

    /**
     * Executes the logic related to the first step of "xa_end" on the
     * server side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_end_8(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id);

    

    /**
     * Executes the logic related to the third step of "xa_end" on the server
     * side
     * @param ts IN/OUT a reference to the current thread status
     * @param lmi IN a reference to the message received from the client
     * @param block_id IN position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_end_24(struct thread_status_s *ts,
                         const struct lixa_msg_s *lmi,
                         uint32_t block_id);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_XA_H */
