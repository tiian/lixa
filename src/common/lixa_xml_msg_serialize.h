/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_XML_MSG_SERIALIZE_H
# define LIXA_XML_MSG_SERIALIZE_H



#include "config.h"



#include "lixa_xml_msg.h"



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
     * Serialize a message struct to an XML buffer for external transmission
     * @param[in] msg the object that must be serialized
     * @param[out] output contains the XML serialized object and MUST BE FREED
     *             by the caller using free() function
     * @param[out] output_len is the length of the produced output
     * @return a reason code
     */
    int lixa_msg_serialize(const struct lixa_msg_s *msg,
                           char **output, size_t *output_len);


    
    /**
     * Serialize the "close_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset offset must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remainig free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_close_8(const struct lixa_msg_s *msg,
                                   char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "commit_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset offset must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_commit_8(const struct lixa_msg_s *msg,
                                    char *buffer,
                                    size_t *offset, size_t *free_chars);

    

    /**
     * Serialize the "end_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset offset must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_end_8(const struct lixa_msg_s *msg,
                                 char *buffer,
                                 size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "forget_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset offset must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_forget_8(const struct lixa_msg_s *msg,
                                    char *buffer,
                                    size_t *offset, size_t *free_chars);

    

    /**
     * Serialize the "end_16" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_end_16(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars);

    

    /**
     * Serialize the "open_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_open_8(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "open_16" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_open_16(const struct lixa_msg_s *msg,
                                   char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "open_24" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_open_24(const struct lixa_msg_s *msg,
                                   char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "prepare_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_prepare_8(const struct lixa_msg_s *msg,
                                     char *buffer,
                                     size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "prepare_16" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_prepare_16(const struct lixa_msg_s *msg,
                                      char *buffer,
                                      size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "prepare_24" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_prepare_24(const struct lixa_msg_s *msg,
                                      char *buffer,
                                      size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "prepare_32" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_prepare_32(const struct lixa_msg_s *msg,
                                      char *buffer,
                                      size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "qrcvr_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_qrcvr_8(const struct lixa_msg_s *msg,
                                   char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "qrcvr_16" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_qrcvr_16(const struct lixa_msg_s *msg,
                                    char *buffer,
                                    size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "qrcvr_24" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_qrcvr_24(const struct lixa_msg_s *msg,
                                    char *buffer,
                                    size_t *offset, size_t *free_chars);

    

    /**
     * Serialize the "reg_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_reg_8(const struct lixa_msg_s *msg,
                                 char *buffer,
                                 size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "rollback_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_rollback_8(const struct lixa_msg_s *msg,
                                      char *buffer,
                                      size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "start_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_start_8(const struct lixa_msg_s *msg,
                                   char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "start_16" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_start_16(const struct lixa_msg_s *msg,
                                    char *buffer,
                                    size_t *offset, size_t *free_chars);


    /**
     * Serialize the "start_24" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_start_24(const struct lixa_msg_s *msg,
                                    char *buffer,
                                    size_t *offset, size_t *free_chars);

    

    /**
     * Serialize the "unreg_8" specific body part of a message
     * @param[in] msg the object must be serialized
     * @param[out] buffer that will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param[in,out] offset that must be used to start serialization inside
     *                      the buffer
     * @param[in,out] free_chars remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_unreg_8(const struct lixa_msg_s *msg,
                                   char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * @brief Serialize the "trans_8" specific body part of a message
     * @param[in] msg
     * @param[out] buffer
     * @param[in,out] offset
     * @param[in,out] free_chars
     * @return a reason code
     */
    int lixa_msg_serialize_trans_8(const struct lixa_msg_s *msg, char *buffer,
                                   size_t *offset, size_t *free_chars);


    
    /**
     * @brief Serialize the "trans_16" specific body part of a message
     * @param msg
     * @param buffer
     * @param offset
     * @param free_chars
     * @return
     */
    int lixa_msg_serialize_trans_16(const struct lixa_msg_s *msg, char *buffer,
                                    size_t *offset, size_t *free_chars);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */


#endif /* LIXA_XML_MSG_SERIALIZE_H */
