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



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



int xml_msg_translate(xmlNode *root_element,
                      struct xml_msg_generic_s *xmg)
{
    enum Exception { MSG_TAG_NOT_FOUND
                     , TYPE_PROP_NOT_FOUND
                     , TRANSLATE_ARGS
                     , UNKNOWN_XML_MSG_TYPE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xml_msg_translate\n"));
    TRY {
        char *type_str;
        
        /* check the higher level tag is "msg" */
        if (root_element->type != XML_ELEMENT_NODE ||
            xmlStrcmp(root_element->name, (xmlChar *)XML_MSG_TAG_MSG))
            THROW(MSG_TAG_NOT_FOUND);
        /* retrieve the "type" property value */
        if (NULL == (type_str = (char *)xmlGetProp(
                         root_element, (xmlChar *)XML_MSG_PROP_TYPE)))
            THROW(TYPE_PROP_NOT_FOUND);
        xmg->type = strtol(type_str, NULL, 0);
        xmlFree(type_str);

        /* parse specific message */
        switch (xmg->type) {
            case XML_MSG_TX_OPEN1_TYPE:
                if (LIXA_RC_OK != (ret_cod = xml_msg_translate_args(
                                       root_element->children, xmg)))
                    THROW(TRANSLATE_ARGS);
                break;
            default:
                THROW(UNKNOWN_XML_MSG_TYPE);
                break;
        };
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_TAG_NOT_FOUND:
            case TYPE_PROP_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case TRANSLATE_ARGS:
                break;
            case UNKNOWN_XML_MSG_TYPE:
                ret_cod = LIXA_RC_UNKNOWN_XML_MSG_TYPE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xml_msg_translate/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xml_msg_translate_args(xmlNode *node,
                           struct xml_msg_generic_s *xmg)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xml_msg_translate_args\n"));
    TRY {
        restart from here
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
    LIXA_TRACE(("xml_msg_translate_args/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

