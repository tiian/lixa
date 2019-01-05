/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_MESSAGES_H
# define SERVER_MESSAGES_H



#include <config.h>



#include <lixa_trace.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER



/**
 * Associated to @ref srv_msg_body_new_client_s
 */
#define SRV_MSG_TYPE_NEW_CLIENT      1
/**
 * Associated to @ref srv_msg_body_switch_req_s
 */
#define SRV_MSG_TYPE_SWITCH_REQ      2
/**
 * Associated to @ref srv_msg_body_switch_rep_s
 */
#define SRV_MSG_TYPE_SWITCH_REP      3
/**
 * Associated to @ref srv_msg_body_shutdown_s
 */
#define SRV_MSG_TYPE_SHUTDOWN        4



/**
 * Signal a manager, it must deal with a new client
 */
struct srv_msg_body_new_client_s {
    /**
     * file descriptor of the socket connected to the new client
     */
    int fd;
};



/**
 * Switch thread request: a client must be moved from a source thread to a
 * destination thread
 */
struct srv_msg_body_switch_req_s {
    /**
     * source thread identifier
     */
    int                       source;
    /**
     * file descriptor of the socket connected to the client
     */    
    int                       fd;
    /**
     * block id of the header block in the source status file
     */
    uint32_t                  block_id;
    /**
     * slot id of the switching client in the polling array
     */
    size_t                    slot_id;
    /**
     * buffer received from the client
     */
    char                     *buffer;
    /**
     * number of significative bytes in buffer
     */
    size_t                    buffer_size;
    /**
     * pointer to the source header block
     */
    struct payload_header_s  *header;
    /**
     * pointers to the source rsrmgr blocks
     */
    struct payload_rsrmgr_s  *rsrmgr[CHAIN_MAX_SIZE];
};



/**
 * Switch thread reply: a cliend must be moved from a source thread to a
 * destination thread
 */
struct srv_msg_body_switch_rep_s {
    /**
     * a standardized reason code of the switch operation: it must be
     * LIXA_RC_OK to go on correctly
     */
    int                       result;
    /**
     * block id of the header block in the source status file
     */
    uint32_t                  block_id;
    /**
     * slot id of the switching client in the polling array
     */
    size_t                    slot_id;
};



/**
 * Shutdown has been requested by the operator
 */
struct srv_msg_body_shutdown_s {
    /**
     * Type of shutdown must be performed
     */
    enum shutdown_type_e      type;
};



/**
 * Used to code a generic message exchanged in the server
 */
struct srv_msg_s {
    /**
     * Message type used to understand the message body
     */
    uint32_t          type;
    /**
     * Message body / payload: it carries information
     */
    union {
        struct srv_msg_body_new_client_s    nc;
        struct srv_msg_body_switch_req_s    sr;
        struct srv_msg_body_switch_rep_s    sp;
        struct srv_msg_body_shutdown_s      sd;
    } body;
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



#endif /* SERVER_MESSAGES_H */
