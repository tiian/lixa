/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXA_XML_MSG_TRACE_H
# define LIXA_XML_MSG_TRACE_H


#include <config.h>


#include <lixa_xml_msg.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_XML_MSG


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/**
 * Display the content of a previously deserialized message with
 * @ref lixa_msg_deserialize method
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "close" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_close(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "commit" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_commit(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of an "end" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_end(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "forget" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_forget(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "open" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_open(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "prepare" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_prepare(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "qrcvr" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_qrcvr(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "reg" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_reg(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "rollback" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_rollback(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of a "start" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_start(const struct lixa_msg_s *msg);


/**
 * Convenience function for @ref lixa_msg_trace : it display the content
 * of an "unreg" message
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_unreg(const struct lixa_msg_s *msg);

    
/**
 * @brief Convenience function for @ref lixa_msg_trace : it display the content
 * of an "scan" message
 * @param[in] msg the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_scan(const struct lixa_msg_s *msg);

    
/**
 * @param msg IN the message must be massaged
 * @return a reason code
 */
    int lixa_msg_trace_trans(const struct lixa_msg_s *msg);

    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */


#endif /* LIXA_XML_MSG_TRACE_H */
