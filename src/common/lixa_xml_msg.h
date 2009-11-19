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
#ifndef LIXA_XML_MSG_H
# define LIXA_XML_MSG_H



#include <config.h>



#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_XML_MSG



/**
 * Default buffer size for XML messages (used for serialization/
 * deserialization)
 **/
#define LIXA_MSG_XML_BUFFER_SIZE 4096



/**
 * Current protocol level; it's used to recognize incompatible client/server
 * configuration at run-time
 */
#define LIXA_MSG_LEVEL        0
/**
 * Id reserved for a null message
 */
#define LIXA_MSG_VERB_NULL    0
/**
 * Id assigned to verb "open"
 */
#define LIXA_MSG_VERB_OPEN    1
/**
 * Id assigned to verb "open"
 */
#define LIXA_MSG_VERB_CLOSE   2



/**
 * Label used to specify "level" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_LEVEL;
/**
 * Label used to specify "name" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_NAME;
/**
 * Label used to specify "profile" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_PROFILE;
/**
 * Label used to specify "rc" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_RC;
/**
 * Label used to specify "rmid" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_RMID;
/**
 * Label used to specify "step" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_STEP;
/**
 * Label used to specify "verb" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_VERB;
/**
 * Label used to specify "answer" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_ANSWER;
/**
 * Label used to specify "client" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_CLIENT;
/**
 * Label used to specify "msg" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_MSG;
/**
 * Label used to specify "rsrmgr" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_RSRMGR;
/**
 * Label used to specify "rsrmgrs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_RSRMGRS;



/**
 * Mandatory header for every message encoded as @ref lixa_msg_s
 */
struct lixa_msg_header_s {
    /**
     * Protocol level must be applied to this message
     */
    int level;
    /**
     * Specifies the verb (open, close, begin, commit, ecc...)
     */
    int verb;
    /**
     * Specifies the step inside the verb (1, 2, 3, ...)
     */
    int step; 
};

 

/**
 * Generic answer message struct
 */
struct lixa_msg_body_answer_s {
    /**
     * Return code of the invoked operation
     */
    int ret_cod;
};



/**
 * Convenience struct for @ref lixa_msg_body_open_1_s
 */
struct lixa_msg_body_open_1_client_s {
    xmlChar   *profile;
};

    

/**
 * Convenience struct for @ref lixa_msg_body_open_1_s
 */
struct lixa_msg_body_open_1_rsrmgr_s {
    int        rmid;
    xmlChar   *name;
};

    

/**
 * Message body for verb "open", step "1"
 */
struct lixa_msg_body_open_1_s {
    struct lixa_msg_body_open_1_client_s   client;
    GArray                                *rsrmgrs;
};



/**
 * This structure maps the messages flowing between LIXA client (lixac) and
 * LIXA server (lixad). The struct is not used for the transmission over the
 * network, but only inside the client and the server.
 * This is a "fake" object; it's defined and used in the hope of simplicity
 */
struct lixa_msg_s {
    /**
     * Message header, common to all messages
     */
    struct lixa_msg_header_s            header;
    /**
     * Message body, it depends from header
     */
    union {
        struct lixa_msg_body_answer_s   answer;
        struct lixa_msg_body_open_1_s   open_1;
    } body;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Serialize a message struct to an XML buffer for external transmission
     * @param msg IN the object must be serialized
     * @param buffer OUT the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param buffer_len IN the space allocated for buffer
     * @param msg_len OUT number of chars used in buffer for serializing msg
     * @return a reason code
     */
    int lixa_msg_serialize(const struct lixa_msg_s *msg,
                           char *buffer, size_t buffer_len,
                           size_t *msg_len);

    

    /**
     * Serialize the "open_1" specific body part of a message
     * @param msg IN the object must be serialized
     * @param buffer OUT the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param offset IN/OUT offset must be used to start serialization inside
     *                      the buffer
     * @param free_chars IN/OUT remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_open_1(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars);


    
    /**
     * Serialize the "open_2" specific body part of a message
     * @param msg IN the object must be serialized
     * @param buffer OUT the buffer will contain the XML serialized object
     *                   (the size has fixed size of
     *                   @ref LIXA_MSG_XML_BUFFER_SIZE bytes) and will be
     *                   null terminated
     * @param offset IN/OUT offset must be used to start serialization inside
     *                      the buffer
     * @param free_chars IN/OUT remaing free chars inside the buffer
     * @return a reason code
     */
    int lixa_msg_serialize_open_2(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars);

    
    
    /**
     * Deserialize a buffer containing the XML to a message struct
     * @param buffer IN the buffer that's containing the serialized object
     *                  (it must be null terminated)
     * @param buffer_len IN number of significative bytes of buffer
     * @param msg OUT the object after deserialization
     * @return a reason code
     */
    int lixa_msg_deserialize(const char *buffer, size_t buffer_len,
                             struct lixa_msg_s *msg);
    

    
    /**
     * Deserialize an XML subtree containing details pertaining to
     * a message with verb=open, step=1
     * @param cur IN pointer to XML subtree
     * @param msg OUT the object after deserialization
     * @return a reason code
     */
    int lixa_msg_deserialize_open_1(xmlNodePtr cur, struct lixa_msg_s *msg);



    /**
     * Free all the dynamically allocated strings previously allocated by
     * @ref lixa_msg_deserialize using xmlGetProp method
     * @param msg IN/OUT the message must be massaged
     * @return a reason code
     */
    int lixa_msg_free(struct lixa_msg_s *msg);


    
    /**
     * Display the content of a previously deserialized message with
     * @ref lixa_msg_deserialize method
     * @param msg IN the message must be massaged
     * @return a reason code
     */
    int lixa_msg_trace(const struct lixa_msg_s *msg);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_XML_MSG_H */
