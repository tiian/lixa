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
 * @param slot_id IN the slot associated to the file descriptor raised the
 *                   POLLIN event
 * @param lmo IN message will be returned to the client
 * @return a standardized return code
 */
    int server_reply_default(struct thread_status_s *ts, size_t slot_id,
                             struct lixa_msg_s *lmo);


/**
 * Send the output message related to "end" verb to the client
 * @param ts IN/OUT thread status structure
 * @param slot_id IN the slot associated to the file descriptor raised the
 *                   POLLIN event
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
 * @param slot_id IN the slot associated to the file descriptor raised the
 *                   POLLIN event
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
 * @param slot_id IN the slot associated to the file descriptor raised the
 *                   POLLIN event
 * @param lmo IN message will be returned to the client
 * @param rc IN return code of the previous operations must be returned
 *              to the client
 * @return a standardized return code
 */
    int server_reply_prepare(struct thread_status_s *ts, size_t slot_id,
                             struct lixa_msg_s *lmo, int rc);


/**
 * Send the output message related to "qrcvr" verb to the client
 * @param ts IN/OUT thread status structure
 * @param slot_id IN the slot associated to the file descriptor raised the
 *                   POLLIN event
 * @param lmo IN message will be returned to the client
 * @return a standardized return code
 */
    int server_reply_qrcvr(struct thread_status_s *ts, size_t slot_id,
                           struct lixa_msg_s *lmo);


/**
 * Send the output message related to "start" verb to the client
 * @param ts IN/OUT thread status structure
 * @param slot_id IN the slot associated to the file descriptor raised the
 *                   POLLIN event
 * @param lmo IN message will be returned to the client
 * @param rc IN return code of the previous operations must be returned
 *              to the client
 * @return a standardized return code
 */
    int server_reply_start(struct thread_status_s *ts, size_t slot_id,
                           struct lixa_msg_s *lmo, int rc);

/**
 * @brief Send the output message related to the "trans" verb to the client
 * @param ts
 * @param slot_id
 * @param lmo
 * @return a standardized return code
 */
    int server_reply_trans(struct thread_status_s *ts, size_t slot_id,
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


#endif /* SERVER_REPLY_H */
