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
#include <config.h>



#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



const xmlChar *LIXA_XML_MSG_PROP_LEVEL =  (xmlChar *)"level";
const xmlChar *LIXA_XML_MSG_PROP_NAME =   (xmlChar *)"name";
const xmlChar *LIXA_XML_MSG_PROP_RMID =   (xmlChar *)"rmid";
const xmlChar *LIXA_XML_MSG_PROP_STEP =   (xmlChar *)"step";
const xmlChar *LIXA_XML_MSG_PROP_SYNC =   (xmlChar *)"sync";
const xmlChar *LIXA_XML_MSG_PROP_VERB =   (xmlChar *)"verb";
const xmlChar *LIXA_XML_MSG_PROP_WAIT =   (xmlChar *)"wait";
const xmlChar *LIXA_XML_MSG_TAG_CLIENT =  (xmlChar *)"client";
const xmlChar *LIXA_XML_MSG_TAG_MSG =     (xmlChar *)"msg";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGR =  (xmlChar *)"rsrmgr";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGRS = (xmlChar *)"rsrmgrs";



int lixa_msg_serialize(const struct lixa_msg_s msg,
                       char *buffer)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize(const char *buffer, size_t buffer_len,
                         struct lixa_msg_s *msg)
{
    enum Exception { XML_READ_MEMORY_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlDocPtr doc = NULL;
    
    LIXA_TRACE(("lixa_msg_deserialize\n"));
    TRY {
        if (NULL == (doc = xmlReadMemory(buffer, (int)buffer_len, "buffer.xml",
                                         NULL, 0)))
            THROW(XML_READ_MEMORY_ERROR);

        /* @@@ perform deserialization logic (tree navigation) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XML_READ_MEMORY_ERROR:
                ret_cod = LIXA_RC_XML_READ_MEMORY_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover resources */
        if (NULL != doc) {
            /* free parsed document */
            xmlFreeDoc(doc);
            /* release libxml2 stuff */
            xmlCleanupParser();
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

