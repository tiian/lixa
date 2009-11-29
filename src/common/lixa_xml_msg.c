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



const xmlChar *LIXA_XML_MSG_PROP_FLAGS =        (xmlChar *)"flags";
const xmlChar *LIXA_XML_MSG_PROP_LEVEL =        (xmlChar *)"level";
const xmlChar *LIXA_XML_MSG_PROP_NAME =         (xmlChar *)"name";
const xmlChar *LIXA_XML_MSG_PROP_PROFILE =      (xmlChar *)"profile";
const xmlChar *LIXA_XML_MSG_PROP_RC =           (xmlChar *)"rc";
const xmlChar *LIXA_XML_MSG_PROP_RMID =         (xmlChar *)"rmid";
const xmlChar *LIXA_XML_MSG_PROP_STEP =         (xmlChar *)"step";
const xmlChar *LIXA_XML_MSG_PROP_VERB =         (xmlChar *)"verb";
const xmlChar *LIXA_XML_MSG_PROP_XA_INFO =      (xmlChar *)"xa_info";
const xmlChar *LIXA_XML_MSG_TAG_ANSWER =        (xmlChar *)"answer";
const xmlChar *LIXA_XML_MSG_TAG_CLIENT =        (xmlChar *)"client";
const xmlChar *LIXA_XML_MSG_TAG_MSG =           (xmlChar *)"msg";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGR =        (xmlChar *)"rsrmgr";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGRS =       (xmlChar *)"rsrmgrs";
const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXEC =  (xmlChar *)"xa_open_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXECS = (xmlChar *)"xa_open_execs";



int lixa_msg_serialize(const struct lixa_msg_s *msg,
                       char *buffer, size_t buffer_len,
                       size_t *msg_len)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , SERIALIZE_OPEN_8_ERROR
                     , SERIALIZE_OPEN_16_ERROR
                     , SERIALIZE_OPEN_24_ERROR
                     , INVALID_OPEN_STEP
                     , INVALID_VERB
                     , BUFFER_TOO_SHORT3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int used_chars = 0;
    size_t free_chars = buffer_len, offset = 0;
    
    LIXA_TRACE(("lixa_msg_serialize\n"));
    TRY {
        used_chars = snprintf(buffer, free_chars,
                              "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>");
        if (used_chars >= free_chars)
            THROW(BUFFER_TOO_SHORT1);
        /* <msg ... > */
        free_chars -= used_chars;
        offset += used_chars;
        used_chars = snprintf(buffer + offset, free_chars,
                              "<%s %s=\"%d\" %s=\"%d\" %s=\"%d\">",
                              LIXA_XML_MSG_TAG_MSG,
                              LIXA_XML_MSG_PROP_LEVEL,
                              msg->header.level,
                              LIXA_XML_MSG_PROP_VERB,
                              msg->header.verb,
                              LIXA_XML_MSG_PROP_STEP,
                              msg->header.step);
        if (used_chars >= free_chars)
            THROW(BUFFER_TOO_SHORT2);
        free_chars -= used_chars;
        offset += used_chars;

        switch (msg->header.verb) {
            case LIXA_MSG_VERB_OPEN:
                switch (msg->header.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_open_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_OPEN_8_ERROR);
                        break;
                    case 16:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_open_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_OPEN_16_ERROR);
                        break;
                    case 24:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_open_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_OPEN_24_ERROR);
                        break;
                    default:
                        THROW(INVALID_OPEN_STEP);
                }
                break;
            default:
                THROW(INVALID_VERB);
        }

        /* @@@ */

        /* </msg> */
        used_chars = snprintf(buffer + offset, free_chars,
                              "</%s>", LIXA_XML_MSG_TAG_MSG);
        if (used_chars >= free_chars)
            THROW(BUFFER_TOO_SHORT3);
        free_chars -= used_chars;
        offset += used_chars;

        *msg_len = offset;
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_TOO_SHORT1:
            case BUFFER_TOO_SHORT2:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case SERIALIZE_OPEN_8_ERROR:
            case SERIALIZE_OPEN_16_ERROR:
            case SERIALIZE_OPEN_24_ERROR:
                break;
            case INVALID_OPEN_STEP:
            case INVALID_VERB:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case BUFFER_TOO_SHORT3:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
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



int lixa_msg_serialize_open_8(const struct lixa_msg_s *msg,
                              char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_open_8\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\"/>",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_PROFILE,
                              msg->body.open_8.client.profile);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgr> */
        for (i=0; i<msg->body.open_8.rsrmgrs->len; ++i) {
            struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr;
            rsrmgr = &g_array_index(
                msg->body.open_8.rsrmgrs,
                struct lixa_msg_body_open_8_rsrmgr_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"%s\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid,
                                  LIXA_XML_MSG_PROP_NAME,
                                  rsrmgr->name);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT4);
        *free_chars -= used_chars;
        *offset += used_chars;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_TOO_SHORT1:
            case BUFFER_TOO_SHORT2:
            case BUFFER_TOO_SHORT3:
            case BUFFER_TOO_SHORT4:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_open_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_open_16(const struct lixa_msg_s *msg,
                              char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_open_16\n"));
    TRY {
        int used_chars;
        
        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.open_16.answer.rc);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT);
        *free_chars -= used_chars;
        *offset += used_chars;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_TOO_SHORT:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_open_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_open_24(const struct lixa_msg_s *msg,
                              char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_open_24\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <xa_open_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_XA_OPEN_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_open_exec> */
        for (i=0; i<msg->body.open_24.xa_open_execs->len; ++i) {
            struct lixa_msg_body_open_24_xa_open_execs_s *xa_open_exec;
            xa_open_exec = &g_array_index(
                msg->body.open_24.xa_open_execs,
                struct lixa_msg_body_open_24_xa_open_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%s\" %s=\"%d\" %s=\"%ld\" "
                                  "%s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_OPEN_EXEC,
                                  LIXA_XML_MSG_PROP_XA_INFO,
                                  (char *)xa_open_exec->xa_info,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_open_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_open_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_open_exec->rc);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT2);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_open_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_OPEN_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT3);
        *free_chars -= used_chars;
        *offset += used_chars;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_TOO_SHORT1:
            case BUFFER_TOO_SHORT2:
            case BUFFER_TOO_SHORT3:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_open_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize(const char *buffer, size_t buffer_len,
                         struct lixa_msg_s *msg)
{
    enum Exception { XML_READ_MEMORY_ERROR
                     , EMPTY_XML_MSG
                     , ROOT_TAG_IS_NOT_MSG
                     , MISSING_TAGS
                     , VERB_NOT_FOUND
                     , STEP_NOT_FOUND
                     , LEVEL_NOT_FOUND
                     , INVALID_STEP1
                     , INVALID_VERB
                     , CHILD_LEVEL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlDocPtr doc = NULL;
    xmlNodePtr cur = NULL;
    
    LIXA_TRACE(("lixa_msg_deserialize\n"));
    TRY {
        xmlChar *tmp = NULL;
        
        if (NULL == (doc = xmlReadMemory(buffer, (int)buffer_len, "buffer.xml",
                                         NULL, 0)))
            THROW(XML_READ_MEMORY_ERROR);

        /* retrieve header message properties */
        if (NULL == (cur = xmlDocGetRootElement(doc)))
            THROW(EMPTY_XML_MSG);
        if (xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_MSG))
            THROW(ROOT_TAG_IS_NOT_MSG);
        /* check there is at least a child */
        if (NULL == cur->xmlChildrenNode)
            THROW(MISSING_TAGS);
        /* reset output message */
        memset(msg, 0, sizeof(struct lixa_msg_s));
        /* retrieve verb */
        if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_VERB)))
            THROW(VERB_NOT_FOUND);
        msg->header.verb = (int)strtol((char *)tmp, NULL, 0);
        xmlFree(tmp);
        /* retrieve step */
        if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_STEP)))
            THROW(STEP_NOT_FOUND);
        msg->header.step = (int)strtol((char *)tmp, NULL, 0);
        xmlFree(tmp);
        /* retrieve level */
        if (msg->header.step == 1) {
            if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_LEVEL)))
                THROW(LEVEL_NOT_FOUND);
            msg->header.level = (int)strtol((char *)tmp, NULL, 0);
            xmlFree(tmp);
        }
        switch (msg->header.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                switch (msg->header.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_open_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 16:
                        ret_cod = lixa_msg_deserialize_open_16(
                            cur->xmlChildrenNode, msg);
                        break;                        
                    case 24:
                        ret_cod = lixa_msg_deserialize_open_24(
                            cur->xmlChildrenNode, msg);
                        break;                        
                    default:
                        THROW(INVALID_STEP1);
                }
                break;
            default:
                THROW(INVALID_VERB);
        }
        if (LIXA_RC_OK != ret_cod)
            THROW(CHILD_LEVEL_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XML_READ_MEMORY_ERROR:
                ret_cod = LIXA_RC_XML_READ_MEMORY_ERROR;
                break;
            case EMPTY_XML_MSG:
                ret_cod = LIXA_RC_EMPTY_XML_MSG;
                break;
            case ROOT_TAG_IS_NOT_MSG:
            case MISSING_TAGS:
            case VERB_NOT_FOUND:
            case STEP_NOT_FOUND:
            case LEVEL_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case INVALID_STEP1:
            case INVALID_VERB:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case CHILD_LEVEL_ERROR: /* propagate child's ret_cod */
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
            /* release libxml2 stuff
            xmlCleanupParser();
            */
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_open_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { PROFILE_NOT_FOUND
                     , RMID_NOT_FOUND
                     , NAME_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_open_8\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CLIENT)) {
                /* retrieve client properties */
                if (NULL == (msg->body.open_8.client.profile =
                             xmlGetProp(cur, LIXA_XML_MSG_PROP_PROFILE)))
                    THROW(PROFILE_NOT_FOUND);
            } else if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_RSRMGRS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.open_8.rsrmgrs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_open_8_rsrmgr_s),
                    LIXA_MSG_XML_START_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name, LIXA_XML_MSG_TAG_RSRMGR)) {
                        xmlChar *tmp;
                        struct lixa_msg_body_open_8_rsrmgr_s rsrmgr;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        rsrmgr.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve name */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_NAME)))
                            THROW(NAME_NOT_FOUND);
                        rsrmgr.name = tmp;
                        g_array_append_val(msg->body.open_8.rsrmgrs, rsrmgr);
                    }
                    cur2 = cur2->next;
                } /* while (NULL != child) */
            } else
                THROW(XML_UNRECOGNIZED_TAG);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RMID_NOT_FOUND:
            case NAME_NOT_FOUND:
            case PROFILE_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case XML_UNRECOGNIZED_TAG:
                ret_cod = LIXA_RC_XML_UNRECOGNIZED_TAG;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize_open_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_open_16(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { RC_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_open_16\n"));
    TRY {
        xmlChar *tmp = NULL;

        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_ANSWER)) {
                /* retrieve answer result */
                if (NULL == (tmp = (xmlGetProp(cur, LIXA_XML_MSG_PROP_RC))))
                    THROW(RC_NOT_FOUND);
                msg->body.open_16.answer.rc =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else
                THROW(XML_UNRECOGNIZED_TAG);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RC_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case XML_UNRECOGNIZED_TAG:
                ret_cod = LIXA_RC_XML_UNRECOGNIZED_TAG;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize_open_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_open_24(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { XA_INFO_NOT_FOUND
                     , RMID_NOT_FOUND
                     , FLAGS_NOT_FOUND
                     , RC_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_open_24\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_XA_OPEN_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.open_24.xa_open_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_open_24_xa_open_execs_s),
                    LIXA_MSG_XML_START_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name, LIXA_XML_MSG_TAG_XA_OPEN_EXEC)) {
                        xmlChar *tmp;
                        struct lixa_msg_body_open_24_xa_open_execs_s
                            xa_open_exec;
                        /* retrieve xa_info */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_XA_INFO)))
                            THROW(XA_INFO_NOT_FOUND);
                        xa_open_exec.xa_info = tmp;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        xa_open_exec.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve flags */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FLAGS)))
                            THROW(FLAGS_NOT_FOUND);
                        xa_open_exec.flags = strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        xa_open_exec.rc = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.open_24.xa_open_execs,
                                           xa_open_exec);
                    }
                    cur2 = cur2->next;
                } /* while (NULL != child) */
            } else
                THROW(XML_UNRECOGNIZED_TAG);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XA_INFO_NOT_FOUND:
            case RMID_NOT_FOUND:
            case FLAGS_NOT_FOUND:
            case RC_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case XML_UNRECOGNIZED_TAG:
                ret_cod = LIXA_RC_XML_UNRECOGNIZED_TAG;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize_open_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_free(struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP1
                     , INVALID_VERB
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_free\n"));
    TRY {
        guint i;
        switch (msg->header.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                switch (msg->header.step) {
                    case 8:
                        xmlFree(msg->body.open_8.client.profile);
                        msg->body.open_8.client.profile = NULL;
                        for (i=0; i<msg->body.open_8.rsrmgrs->len; ++i) {
                            struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr =
                                &g_array_index(
                                    msg->body.open_8.rsrmgrs,
                                    struct lixa_msg_body_open_8_rsrmgr_s, i);
                            xmlFree(rsrmgr->name);
                        }
                        g_array_free(msg->body.open_8.rsrmgrs, TRUE);
                        msg->body.open_8.rsrmgrs = NULL;
                        break;
                    case 24:
                        for (i=0; i<msg->body.open_24.xa_open_execs->len; ++i) {
                            struct lixa_msg_body_open_24_xa_open_execs_s
                                *xa_open_exec =
                                &g_array_index(
                                    msg->body.open_24.xa_open_execs,
                                    struct
                                    lixa_msg_body_open_24_xa_open_execs_s, i);
                            xmlFree(xa_open_exec->xa_info);
                        }
                        g_array_free(msg->body.open_24.xa_open_execs, TRUE);
                        msg->body.open_24.xa_open_execs = NULL;
                        break;
                    default:
                        THROW(INVALID_STEP1);
                }
                break;
            default:
                THROW(INVALID_VERB);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP1:
            case INVALID_VERB:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_free/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_trace(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP1
                     , INVALID_VERB
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace\n"));
    TRY {
        guint i;
        static const xmlChar *nil_str = (xmlChar *)"(nil)";
        
        LIXA_TRACE(("lixa_msg_trace: header[level=%d,verb=%d,step=%d]\n",
                    msg->header.level, msg->header.verb, msg->header.step));
        switch (msg->header.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                switch (msg->header.step) {
                    case 8:
                        LIXA_TRACE(("lixa_msg_trace: body[client[profile["
                                    "%s]]]\n",
                                    msg->body.open_8.client.profile ?
                                    msg->body.open_8.client.profile :
                                    nil_str));
                        if (NULL != msg->body.open_8.rsrmgrs) {
                            for (i=0; i<msg->body.open_8.rsrmgrs->len; ++i) {
                                struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr =
                                    &g_array_index(
                                        msg->body.open_8.rsrmgrs,
                                        struct lixa_msg_body_open_8_rsrmgr_s,
                                        i);
                                LIXA_TRACE(("lixa_msg_trace: body[rsrmgrs["
                                            "rsrmgr[rmid=%d,name='%s']]]\n",
                                            rsrmgr->rmid,
                                            rsrmgr->name ?
                                            rsrmgr->name : nil_str));
                            }
                        }
                        break;
                    case 16:
                        LIXA_TRACE(("lixa_msg_trace: body[answer[rc[%d]]]\n",
                                    msg->body.open_16.answer.rc));
                        break;
                    case 24:
                        if (NULL != msg->body.open_24.xa_open_execs) {
                            for (i=0; i<msg->body.open_24.xa_open_execs->len;
                                 ++i) {
                                struct lixa_msg_body_open_24_xa_open_execs_s
                                    *xa_open_exec =
                                    &g_array_index(
                                        msg->body.open_24.xa_open_execs,
                                        struct
                                        lixa_msg_body_open_24_xa_open_execs_s,
                                        i);
                                LIXA_TRACE(("lixa_msg_trace: body["
                                            "xa_open_execs["
                                            "xa_open_exec["
                                            "xa_info='%s',rmid=%d,flags=%ld,"
                                            "rc=%d]]]\n",
                                            (char *)xa_open_exec->xa_info,
                                            xa_open_exec->rmid,
                                            xa_open_exec->flags,
                                            xa_open_exec->rc));
                            }
                        }
                        break;
                    default:
                        THROW(INVALID_STEP1);
                }
                break;
            default:
                THROW(INVALID_VERB);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP1:
            case INVALID_VERB:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

