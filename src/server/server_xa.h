/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_XA_H
# define SERVER_XA_H



#include "config.h"



#include "lixa_trace.h"
#include "lixa_xml_msg.h"
#include "server_status.h"
#include "server_trans_tbl.h"
#include "lixa_xid.h"



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
     * Executes the logic related to "ax_reg" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block that stores the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_ax_reg(struct thread_status_s *ts,
                      const struct lixa_msg_s *lmi,
                      uint32_t block_id);


    
    /**
     * Executes the logic related to "ax_unreg" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block that stores the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_ax_unreg(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_close" on the server side
     * @param[in] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block that stores the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_close(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_commit" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block that stores the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_commit(struct thread_status_s *ts,
                         const struct lixa_msg_s *lmi,
                         uint32_t block_id);

    

    /**
     * Executes the logic related to "xa_commit" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block that stores the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_commit_8(struct thread_status_s *ts,
                           const struct lixa_msg_s *lmi,
                           uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_end" on the server side
     * @param[in] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_xa_end(struct thread_status_s *ts,
                      const struct lixa_msg_s *lmi,
                      struct lixa_msg_s *lmo,
                      uint32_t block_id,
                      struct server_client_status_s *cs);

    

    /**
     * Executes the logic related to the first step of "xa_end" on the
     * server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_xa_end_8(struct thread_status_s *ts,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id,
                        struct server_client_status_s *cs);


    
    /**
     * Executes the logic related to "xa_forget" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_forget(struct thread_status_s *ts,
                         const struct lixa_msg_s *lmi,
                         uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_forget" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_forget_8(struct thread_status_s *ts,
                           const struct lixa_msg_s *lmi,
                           uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_open" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] last_verb_step last verb and step values (prepared for
     *                       reply function)
     * @return a reason code
     */
    int server_xa_open(struct thread_status_s *ts,
                       const struct lixa_msg_s *lmi,
                       struct lixa_msg_s *lmo,
                       uint32_t block_id,
                       struct lixa_msg_verb_step_s *last_verb_step);


    
    /**
     * Executes the logic related to the first step of "xa_open" on the server
     * side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] last_verb_step last verb and step values (prepared for
     *                       reply function)
     * @return a reason code
     */
    int server_xa_open_8(struct thread_status_s *ts,
                         const struct lixa_msg_s *lmi,
                         struct lixa_msg_s *lmo,
                         uint32_t block_id,
                         struct lixa_msg_verb_step_s *last_verb_step);


    
    /**
     * Executes the logic related to the third step of "xa_open" on the server
     * side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_open_24(struct thread_status_s *ts,
                          const struct lixa_msg_s *lmi,
                          uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_prepare" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_xa_prepare(struct thread_status_s *ts,
                          const struct lixa_msg_s *lmi,
                          struct lixa_msg_s *lmo,
                          uint32_t block_id,
                          struct server_client_status_s *cs);



    /**
     * Executes the logic related to the first step of "xa_prepare" on the
     * server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_xa_prepare_8(struct thread_status_s *ts,
                            const struct lixa_msg_s *lmi,
                            struct lixa_msg_s *lmo,
                            uint32_t block_id,
                            struct server_client_status_s *cs);


    
    /**
     * Executes the logic related to the third step of "xa_prepare" on the
     * server side. It's used to suspend client activity until all branches
     * have prepared their own resource managers.
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_xa_prepare_24(struct thread_status_s *ts,
                             const struct lixa_msg_s *lmi,
                             struct lixa_msg_s *lmo,
                             uint32_t block_id,
                             struct server_client_status_s *cs);


    
    /**
     * Executes the logic related to "xa_rollback" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_rollback(struct thread_status_s *ts,
                           const struct lixa_msg_s *lmi,
                           uint32_t block_id);

    

    /**
     * Executes the logic related to "xa_rollback" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_rollback_8(struct thread_status_s *ts,
                             const struct lixa_msg_s *lmi,
                             uint32_t block_id);


    
    /**
     * Executes the logic related to "xa_start" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] slot_id client identification, position in the polling array
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] last_verb_step last verb and step values (prepared for
     *                       reply function)
     * @return a reason code
     */
    int server_xa_start(struct thread_status_s *ts,
                        size_t slot_id,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id,
                        struct lixa_msg_verb_step_s *last_verb_step);


    
    /**
     * Executes the logic related to the first step of "xa_start" on the
     * server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] slot_id client identification, position in the polling array
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] last_verb_step last verb and step values (prepared for
     *                       reply function)
     * @return a reason code
     */
    int server_xa_start_8(struct thread_status_s *ts,
                          size_t slot_id,
                          const struct lixa_msg_s *lmi,
                          struct lixa_msg_s *lmo,
                          uint32_t block_id,
                          struct lixa_msg_verb_step_s *last_verb_step);


    
    /**
     * Executes the logic related to the third step of "xa_start" on the server
     * side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @return a reason code
     */
    int server_xa_start_24(struct thread_status_s *ts,
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
