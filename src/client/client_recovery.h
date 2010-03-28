/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef CLIENT_RECOVERY_H
# define CLIENT_RECOVERY_H



#include <config.h>



#include <client_status.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_RECOVERY



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Perform the revovery action: the server (lixad) is
     * polled to retrieve the transactions need to be recovered
     * @param cs IN reference to the status of the calling client
     * @param client IN reference to the client piece of info sent with
     *               @ref LIXA_MSG_VERB_OPEN message
     * @return a reason code     
     */
    int client_recovery(client_status_t *cs,
                        const struct lixa_msg_body_open_8_client_s *client);
    


    /**
     * Analyze the data received from the server
     * @param cs IN reference to the status of the calling client
     * @param rpl IN information received from the server
     * @param commit OUT boolean value: TRUE = perform commit, FALSE = perform
     *               rollback
     * @return a reason code
     */
    int client_recovery_analyze(const client_status_t *cs,
                                struct lixa_msg_s *rpl,
                                int *commit);


    
    /**
     * Commit the transaction received from the server
     * @param cs IN reference to the status of the calling client
     * @param rpl IN information received from the server
     * @return a reason code
     */
    int client_recovery_commit(const client_status_t *cs,
                               struct lixa_msg_s *rpl);


    
    /**
     * Rollback the transaction received from the server
     * @param cs IN reference to the status of the calling client
     * @param rpl IN information received from the server
     * @return a reason code
     */
    int client_recovery_rollback(const client_status_t *cs,
                                 struct lixa_msg_s *rpl);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* CLIENT_RECOVERY_H */
