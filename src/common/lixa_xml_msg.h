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
    } cntnt;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Translate a message from XML encoding to native C structure
     * @param root_element IN the root of the XML parsed tree
     * @param xmg OUT the reference to the native C structure
     * @return a standardized return code
     */
    int xml_msg_translate(xmlNode *root_element,
                          struct xml_msg_generic_s *xmg);



    /**
     * Translate args from an XML message to a native C structure
     * @param node IN the root of the XML parsed tree
     * @param xmg OUT the reference to the native C structure
     * @return a standardized return code
     */
    int xml_msg_translate_args(xmlNode *node,
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
