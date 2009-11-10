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



#define XML_MSG_PROP_TYPE           "type"
#define XML_MSG_TAG_ARGS            "args"
#define XML_MSG_TAG_MSG             "msg"
#define XML_MSG_TAG_PROFILE         "profile"



/**
 * Default buffer size for XML messages
 **/
#define XML_BUFFER_SIZE 4096

/**
 * This is the standard header must be prepended to every XML message
 */
#define XML_MSG_HEADER "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"



/**
 * Type of the first message sent by tx_open function to the server
 */
#define XML_MSG_TX_OPEN1_TYPE (long)1



/*
  level: message level, version
  verb:  xa_open -> open; xa_begin ->begin, ...
  step:  1, 2, 3, ... as necessary
  wait:  0=async message, 1=sync message = the client is waiting reply
  sync:  0 do not synchronize status file, 1 status file must be synchronized

  client->server message (question)
  <msg level="1" verb="open" step="1" wait="1" sync="0">
    <client profile="VZ67"/>
    <rsrmgrs>
      <rsrmgr rmid="1" name="dummyrm"/>
      <rsrmgr rmid="2" name="randomrm"/>
    </rsrmgrs>
  </msg>

  server -> client message (answer)
  <msg level="1" verb="open" step="2" rc="0"/>
 */



/**
 * Text of the first message sent by tx_open function to the server
 */
#define XML_MSG_TX_OPEN1 XML_MSG_HEADER \
    "<" XML_MSG_TAG_MSG " " XML_MSG_PROP_TYPE "=\"%ld\">" \
    "<" XML_MSG_TAG_ARGS ">" \
    "<" XML_MSG_TAG_PROFILE ">%s</" XML_MSG_TAG_PROFILE ">" \
    "</" XML_MSG_TAG_ARGS ">" \
    "</" XML_MSG_TAG_MSG ">"



/**
 * Struct used to map a message of type @ref XML_MSG_TX_OPEN1
 */
struct xml_msg_tx_open1_s {
    /**
     * Dynamically allocated, it must be released after usage
     */
    char *profile;
};



/**
 * Struct used to map a generic message
 */
struct xml_msg_generic_s {
    /**
     * The string associated to the type carried by XML "<type>" tag
     */
    long  type;
    /**
     * Content: it depends on type
     */
    union {
        struct xml_msg_tx_open1_s   tx_open1;
    };
};



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
 * Id assigned to verb "open"
 */
#define LIXA_MSG_VERB_OPEN    1
/**
 * Id assigned to verb "open"
 */
#define LIXA_MSG_VERB_CLOSE   2



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
    /**
     * The sender is waiting an answer
     */
    int wait;
    /**
     * The server must synchronize the received data as soon as possible
     */
    int sync;
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
     * @return a reason code
     */
    int lixa_msg_serialize(const struct lixa_msg_s msg,
                           char *buffer);

    

    /**
     * Deserialize a buffer containing XML to a message struct
     * @param buffer IN the buffer contains the XML serialized object (it must
     *                  be null terminated)
     * @param msg OUT the object after deserialization
     * @return a reason code
     */
    int lixa_msg_deserialize(const char *buffer,
                             struct lixa_msg_s *msg);
    

    
    /**
     * Translate a message from XML encoding to native C structure
     * @param doc IN the XML parsed document
     * @param xmg OUT the reference to the native C structure
     * @return a standardized return code
     */
    int xml_msg_translate(xmlDocPtr doc,
                          struct xml_msg_generic_s *xmg);



    /**
     * Translate args from an XML message to a native C structure
     * @param doc IN the XML parsed document
     * @param node IN the root of the XML parsed tree
     * @param xmg OUT the reference to the native C structure
     * @return a standardized return code
     */
    int xml_msg_translate_args(xmlDocPtr doc, xmlNode *node,
                               struct xml_msg_generic_s *xmg);

    
    
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
