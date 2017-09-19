/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXA_XML_MSG_DESERIALIZE_H
# define LIXA_XML_MSG_DESERIALIZE_H


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
 * Deserialize a buffer containing the XML to a message struct
 * @param buffer IN/OUT the buffer that's containing the serialized object
 *                  (it must be null terminated)
 * @param buffer_len OUT number of significative bytes of buffer
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize(char *buffer, size_t buffer_len,
                             struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=close, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_close_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=commit, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_commit_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing a default answer message
 * @param cur IN reference to the XML subtree
 * @param answer OUT reference to the answer contained in the message
 * @return a reason code
 */
    int lixa_msg_deserialize_default_answer(
        xmlNodePtr cur,
        struct lixa_msg_body_answer_s *answer);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=end, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_end_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=end, step=16
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_end_16(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=forget, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_forget_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=open, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_open_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=open, step=16
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_open_16(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=open, step=24
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_open_24(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=prepare, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_prepare_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=prepare, step=16
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_prepare_16(xmlNodePtr cur,
                                        struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=qrcvr, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_qrcvr_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=qrcvr, step=16
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_qrcvr_16(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=qrcvr, step=24
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_qrcvr_24(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=reg, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_reg_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=rollback, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_rollback_8(xmlNodePtr cur,
                                        struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=start, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_start_8(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=start, step=16
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_start_16(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=start, step=24
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_start_24(xmlNodePtr cur, struct lixa_msg_s *msg);


/**
 * Deserialize an XML subtree containing details pertaining to
 * a message with verb=unreg, step=8
 * @param cur IN pointer to XML subtree
 * @param msg OUT the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_unreg_8(xmlNodePtr cur, struct lixa_msg_s *msg);

/**
 * @brief Deserialize an XML subtree containing details pertaining to a message
 * with verb=scan, step=8
 * @param[in] cur pointer to XML subtree
 * @param[out] msg the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_trans_8(xmlNodePtr cur, struct lixa_msg_s *msg);

/**
 * @brief Deserialize an XML subtree containing details pertaining to a message
 * with verb=scan, step=8
 * @param[in] cur pointer to XML subtree
 * @param[out] msg the object after deserialization
 * @return a reason code
 */
    int lixa_msg_deserialize_trans_16(xmlNodePtr cur, struct lixa_msg_s *msg);

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */


#endif /* LIXA_XML_MSG_DESERIALIZE_H */
