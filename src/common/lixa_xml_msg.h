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



#define XML_MSG_PROP_NAME "name"
#define XML_MSG_TAG_ARGS "args"
#define XML_MSG_TAG_FUNCTION "function"
#define XML_MSG_TAG_MSG "msg"
#define XML_MSG_TAG_PROFILE "profile"
#define XML_MSG_TAG_TYPE "type"



/**
 * Default buffer size for XML messages
 **/
#define XML_BUFFER_SIZE 4096

/**
 * This is the standard header must be prepended to every XML message
 */
#define XML_MSG_HEADER "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"

/**
 * First message sent by tx_open function to the server
 */
#define XML_MSG_TX_OPEN1 XML_MSG_HEADER \
    "<" XML_MSG_TAG_MSG ">" \
    "<" XML_MSG_TAG_TYPE ">%s</" XML_MSG_TAG_TYPE ">" \
    "<" XML_MSG_TAG_FUNCTION " " XML_MSG_PROP_NAME "=\"tx_open\">" \
    "<" XML_MSG_TAG_ARGS ">" \
    "<" XML_MSG_TAG_PROFILE ">%s</" XML_MSG_TAG_PROFILE ">" \
    "</" XML_MSG_TAG_ARGS ">" \
    "</" XML_MSG_TAG_FUNCTION ">" \
    "</" XML_MSG_TAG_MSG ">"

#define XML_MSG_TX_OPEN1_TYPE "TX_OPEN1"


/**
 * Struct used to map a message of type @ref XML_MSG_TX_OPEN1
 */
struct xml_msg_tx_open1_s {
    char *profile;
};


/**
 * Union used to map a generic message
 */
union xml_msg_generic_u {
    struct xml_msg_tx_open1_s   tx_open1;
};



#endif /* LIXA_XML_MSG_H */
