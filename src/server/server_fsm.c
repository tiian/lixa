/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "config.h"



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "server_fsm.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_FSM



void server_fsm_init(server_fsm_t *fsm, const char *sid)
{
    LIXA_TRACE(("server_fsm_init: sessid='%s', old state=%d ('%s')\n",
                STRORNULL(sid),
                fsm->state, server_fsm_get_state_as_str(fsm)));
    fsm->state = FSM_WANT_FIRST_MESSAGE;
    LIXA_TRACE(("server_fsm_init: sessid='%s', new state=%d ('%s')\n",
                STRORNULL(sid),
                fsm->state, server_fsm_get_state_as_str(fsm)));
}



int server_fsm_message_arrived(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_message_arrived\n"));
    TRY {
        LIXA_TRACE(("server_fsm_message_arrived: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_WANT_FIRST_MESSAGE:
                fsm->state = FSM_FIRST_MESSAGE_ARRIVED;
                break;
            case FSM_WANT_MESSAGE:
                fsm->state = FSM_MESSAGE_ARRIVED;
                break;
            default:
                LIXA_TRACE(("server_fsm_message_arrived: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_message_arrived: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_message_arrived/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_send_message_and_wait(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_send_message_and_wait\n"));
    TRY {
        LIXA_TRACE(("server_fsm_send_message_and_wait: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_FIRST_MESSAGE_ARRIVED:
            case FSM_MESSAGE_ARRIVED:
                fsm->state = FSM_HAVE_MESSAGE_AND_WANT;
                break;
            default:
                LIXA_TRACE(("server_fsm_send_message_and_wait: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_send_message_and_wait: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_send_message_and_wait/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_message_sent(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_message_sent\n"));
    TRY {
        LIXA_TRACE(("server_fsm_message_sent: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_HAVE_MESSAGE_AND_WANT:
                fsm->state = FSM_WANT_MESSAGE;
                break;
            default:
                LIXA_TRACE(("server_fsm_message_sent: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_message_sent: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_message_sent/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_want_message(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_want_message\n"));
    TRY {
        LIXA_TRACE(("server_fsm_want_message: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_MESSAGE_ARRIVED:
                fsm->state = FSM_WANT_MESSAGE;
                break;
            default:
                LIXA_TRACE(("server_fsm_want_message: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_want_message: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_want_message/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_switch_thread(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_switch_thread\n"));
    TRY {
        LIXA_TRACE(("server_fsm_switch_thread: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_FIRST_MESSAGE_ARRIVED:
            case FSM_MESSAGE_ARRIVED:
                fsm->state = FSM_MUST_SWITCH_THREAD;
                break;
            default:
                LIXA_TRACE(("server_fsm_switch_thread: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_switch_thread: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_switch_thread/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_want_wake_up(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_want_wake_up\n"));
    TRY {
        LIXA_TRACE(("server_fsm_want_wake_up: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_MESSAGE_ARRIVED:
                fsm->state = FSM_WANT_WAKE_UP;
                break;
            default:
                LIXA_TRACE(("server_fsm_want_wake_up: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_want_wake_up: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_want_wake_up/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_wake_up_arrived(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_wake_up_arrived\n"));
    TRY {
        LIXA_TRACE(("server_fsm_wake_up_arrived: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_WANT_WAKE_UP:
                fsm->state = FSM_HAVE_MESSAGE_AND_WANT;
                break;
            default:
                LIXA_TRACE(("server_fsm_wake_up_arrived: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_wake_up_arrived: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_wake_up_arrived/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_would_block(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_would_block\n"));
    TRY {
        LIXA_TRACE(("server_fsm_would_block: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_MESSAGE_ARRIVED:
                fsm->state = FSM_WOULD_BLOCK;
                break;
            default:
                LIXA_TRACE(("server_fsm_would_block: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_would_block: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_would_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_fsm_unblock(server_fsm_t *fsm, const char *sid)
{
    enum Exception { INVALID_STATE_TRANSITION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_fsm_unblock\n"));
    TRY {
        LIXA_TRACE(("server_fsm_unblock: sessid='%s', "
                    "old state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        switch (fsm->state) {
            case FSM_WOULD_BLOCK:
                fsm->state = FSM_HAVE_MESSAGE_AND_WANT;
                break;
            default:
                LIXA_TRACE(("server_fsm_unblock: sessid='%s', "
                            "this state transition is not allowed\n",
                            STRORNULL(sid)));
                THROW(INVALID_STATE_TRANSITION);
        } /* switch (fsm->state) */
        LIXA_TRACE(("server_fsm_unblock: sessid='%s', "
                    "new state=%d ('%s')\n", STRORNULL(sid),
                    fsm->state, server_fsm_get_state_as_str(fsm)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATE_TRANSITION:
                ret_cod = LIXA_RC_INVALID_STATE_TRANSITION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_fsm_unblock/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



const char *server_fsm_get_state_as_str(const server_fsm_t *fsm)
{
    switch (fsm->state) {
        case FSM_WANT_FIRST_MESSAGE:
            return "WANT_FIRST_MESSAGE";
        case FSM_FIRST_MESSAGE_ARRIVED:
            return "FIRST_MESSAGE_ARRIVED";
        case FSM_HAVE_MESSAGE_AND_WANT:
            return "HAVE_MESSAGE_AND_WANT";
        case FSM_WANT_MESSAGE:
            return "WANT_MESSAGE";
        case FSM_MESSAGE_ARRIVED:
            return "MESSAGE_ARRIVED";
        case FSM_MUST_SWITCH_THREAD:
            return "MUST_SWITCH_THREAD";
        case FSM_WANT_WAKE_UP:
            return "WANT_WAKE_UP";
        case FSM_WOULD_BLOCK:
            return "WOULD_BLOCK";
        default:
            break;
    } /* switch (fsm->state) */
    return "INVALID_STATE";
}
