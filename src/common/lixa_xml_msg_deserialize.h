/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free 
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
     * a message with verb=end, step=24
     * @param cur IN pointer to XML subtree
     * @param msg OUT the object after deserialization
     * @return a reason code
     */
    int lixa_msg_deserialize_end_24(xmlNodePtr cur, struct lixa_msg_s *msg);



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
    int lixa_msg_deserialize_prepare_16(xmlNodePtr cur, struct lixa_msg_s *msg);



    /**
     * Deserialize an XML subtree containing details pertaining to
     * a message with verb=rollback, step=8
     * @param cur IN pointer to XML subtree
     * @param msg OUT the object after deserialization
     * @return a reason code
     */
    int lixa_msg_deserialize_rollback_8(xmlNodePtr cur, struct lixa_msg_s *msg);



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
