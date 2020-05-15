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
#ifndef SERVER_RECOVERY_H
# define SERVER_RECOVERY_H



#include "config.h"



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_xml_msg.h"
#include "server_status.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_RECOVERY



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Executes the logic related to "qrcvr" on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] slot_id position inside volatile status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_recovery(struct thread_status_s *ts,
                        size_t slot_id,
                        const struct lixa_msg_s *lmi,
                        struct lixa_msg_s *lmo,
                        uint32_t block_id,
                        struct server_client_status_s *cs);

    

    /**
     * Executes the logic related to "qrcvr" (step 8) on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] slot_id position inside volatile status
     * @param[in] lmi reference to the message received from the client
     * @param[out] lmo reference to the message must be sent to the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_recovery_8(struct thread_status_s *ts,
                          size_t slot_id,
                          const struct lixa_msg_s *lmi,
                          struct lixa_msg_s *lmo,
                          uint32_t block_id,
                          struct server_client_status_s *cs);

    

    /**
     * Executes the logic related to "qrcvr" (step 24) on the server side
     * @param[in,out] ts reference to the current thread status
     * @param[in] lmi reference to the message received from the client
     * @param[in] block_id position of the block is storing the status of the
     *                    client is calling the server
     * @param[out] cs client status record
     * @return a reason code
     */
    int server_recovery_24(struct thread_status_s *ts,
                           const struct lixa_msg_s *lmi,
                           uint32_t block_id,
                           struct server_client_status_s *cs);

    

    /**
     * Prepare the message with recovery information must be returned to the
     * client queried the server
     * @param ts IN/OUT a reference to the current thread status
     * @param record IN a reference to the record extracted from the
     *               recovery table
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @param client_block_id IN position of the block is storing the status
     *                        of the client is calling the server
     * @return a standardized reason code
     */
    int server_recovery_result(struct thread_status_s *ts,
                               const struct srvr_rcvr_tbl_rec_s *record,
                               const struct lixa_msg_s *lmi,
                               struct lixa_msg_s *lmo,
                               uint32_t client_block_id);



    /**
     * Prepare the message with no recovery information (no transactions)
     * @param ts IN/OUT a reference to the current thread status
     * @param record IN a reference to the record extracted from the
     *               recovery table
     * @param lmi IN a reference to the message received from the client
     * @param lmo OUT a reference to the message must be sent to the client
     * @return a standardized reason code
     */
    int server_recovery_empty_result(struct thread_status_s *ts,
                                     const struct srvr_rcvr_tbl_rec_s *record,
                                     const struct lixa_msg_s *lmi,
                                     struct lixa_msg_s *lmo);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_RECOVERY_H */
