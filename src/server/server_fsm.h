/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_FSM_H
# define SERVER_FSM_H



#include "config.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_FSM



/**
 * Finite State Machine class for the client sessions managed by the state
 * server
 */
typedef struct server_fsm_s {
    /**
     * Current state of the FSM
     */
    enum { /** The session want to receive the first message from the client */
        FSM_WANT_FIRST_MESSAGE,
        /** The first message from the client has been arrived */
        FSM_FIRST_MESSAGE_ARRIVED,
        /** The session have a message to send to the client and then it
            wants to get another one */
        FSM_HAVE_MESSAGE_AND_WANT,
        /** The session want to receive a message (not the first) from the
            client */
        FSM_WANT_MESSAGE,
        /** A message (not the first) from the client has been arrived */
        FSM_MESSAGE_ARRIVED,
        /** The session must be switched (migrated) to a different thread */
        FSM_MUST_SWITCH_THREAD,
        /** The session has to sleep and wants a wake-up message from another
            client */
        FSM_WANT_WAKE_UP,
        /** The session would block, but the client specified non blocking
            behaviour */
        FSM_WOULD_BLOCK
    } state;
} server_fsm_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a Finite State Machine and set it to the first state
     * @param[out] fsm is the object that must be initialized
     * @param[in] sid session id for debugging purposes only
     */
    void server_fsm_init(server_fsm_t *fsm, const char *sid);



    /**
     * A new message has been arrived and the FSM must be updated
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_message_arrived(server_fsm_t *fsm, const char *sid);
    

    
    /**
     * A message must be sent to the client and another one is wanted
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_send_message_and_wait(server_fsm_t *fsm, const char *sid);
    

    
    /**
     * A message has been sent to the client and the FSM must be updated
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_message_sent(server_fsm_t *fsm, const char *sid);
    

    
    /**
     * A new message is wanted from the client
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_want_message(server_fsm_t *fsm, const char *sid);
    


    /**
     * The client must be migrated to another thread
     * @param[in,out] fsm is the object that must be initialized
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_switch_thread(server_fsm_t *fsm, const char *sid);



    /**
     * A wake-up message is wanted from the client
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_want_wake_up(server_fsm_t *fsm, const char *sid);
    


    /**
     * A wake-up message has been arrived and the FSM must be updated
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_wake_up_arrived(server_fsm_t *fsm, const char *sid);
    

    
    /**
     * The session would block, but it can't because the client specified
     * non blocking behaviour
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_would_block(server_fsm_t *fsm, const char *sid);
    


    /**
     * Unblock a session that was previously moved in "would lock" state
     * @param[in,out] fsm is the Finite State Machine object
     * @param[in] sid session id for debugging purposes only
     * @return a reason code
     */     
    int server_fsm_unblock(server_fsm_t *fsm, const char *sid);
    


    /**
     * Retrieve the current state of the Finite State Machine in a human
     * readable format (C string)
     * @param[in] fsm is the object that must be inspected
     * @return a C string that represents the state
     */
    const char *server_fsm_get_state_as_str(const server_fsm_t *fsm);

    

    /**
     * Check if the Finite State Machine is in the state "first message
     * received"
     * @param[in] fsm is the object that must be inspected
     * @return a boolean value
     */
    static inline int server_fsm_is_first_message(const server_fsm_t *fsm) {
        return FSM_FIRST_MESSAGE_ARRIVED == fsm->state;
    }



    /**
     * Check if the session wants to read a message from the connected client
     * @param[in] fsm is the object that must be inspected
     * @return a boolean value
     */     
    static inline int server_fsm_is_ready_for_input(const server_fsm_t *fsm) {
        return FSM_WANT_FIRST_MESSAGE == fsm->state ||
            FSM_WANT_MESSAGE == fsm->state;
    }



    /**
     * Check if the session wants to write a message to the connected client
     * @param[in] fsm is the object that must be inspected
     * @return a boolean value
     */     
    static inline int server_fsm_is_ready_for_output(const server_fsm_t *fsm) {
        return FSM_HAVE_MESSAGE_AND_WANT == fsm->state;
    }
    


    /**
     * Check if the session is migrating / has to migrate to a different
     * thread
     * @param[in] fsm is the object that must be inspected
     * @return a boolean value
     */     
    static inline int server_fsm_is_client_migrating(const server_fsm_t *fsm) {
        return FSM_MUST_SWITCH_THREAD == fsm->state;
    }



    /**
     * Check if the session temporary sleeping and waiting for wake-up from
     * another one
     * @param[in] fsm is the object that must be inspected
     * @return a boolean value
     */     
    static inline int server_fsm_is_waiting_wake_up(const server_fsm_t *fsm) {
        return FSM_WANT_WAKE_UP == fsm->state;
    }



    /**
     * Check if the session is waiting an unblock action 
     * @param[in] fsm is the object that must be inspected
     * @return a boolean value
     */     
    static inline int server_fsm_is_waiting_unblock(const server_fsm_t *fsm) {
        return FSM_WOULD_BLOCK == fsm->state;
    }

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_FSM_H */
