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
#include <lixa_xml_msg_deserialize.h>
#include <lixa_common_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



int lixa_msg_deserialize(char *buffer, size_t buffer_len,
                         struct lixa_msg_s *msg)
{
    enum Exception { XML_READ_MEMORY
                     , EMPTY_XML_MSG
                     , ROOT_TAG_IS_NOT_MSG
                     , MISSING_TAGS
                     , VERB_NOT_FOUND
                     , STEP_NOT_FOUND
                     , LEVEL_NOT_FOUND
                     , INVALID_STEP1
                     , INVALID_STEP2
                     , INVALID_STEP3
                     , INVALID_STEP4
                     , INVALID_STEP5
                     , INVALID_STEP6
                     , INVALID_VERB
                     , CHILD_LEVEL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlDocPtr doc = NULL;
    xmlNodePtr cur = NULL;
    
    LIXA_TRACE(("lixa_msg_deserialize\n"));
    TRY {
        xmlChar *tmp = NULL;

        LIXA_TRACE(("lixa_msg_deserialize: deserializing |%*.*s|\n",
                    buffer_len, buffer_len, buffer));
        if (NULL == (doc = xmlReadMemory(
                         buffer, buffer_len, "buffer.xml", NULL, 0)))
            THROW(XML_READ_MEMORY);
        
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
        msg->header.pvs.verb = (int)strtol((char *)tmp, NULL, 0);
        xmlFree(tmp);
        /* retrieve step */
        if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_STEP)))
            THROW(STEP_NOT_FOUND);
        msg->header.pvs.step = (int)strtol((char *)tmp, NULL, 0);
        xmlFree(tmp);
        /* retrieve level */
        if (msg->header.pvs.step == LIXA_MSG_STEP_INCR) {
            if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_LEVEL)))
                THROW(LEVEL_NOT_FOUND);
            msg->header.level = (int)strtol((char *)tmp, NULL, 0);
            xmlFree(tmp);
        }
        switch (msg->header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                switch (msg->header.pvs.step) {
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
            case LIXA_MSG_VERB_CLOSE: /* close */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_close_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP2);
                }
                break;
            case LIXA_MSG_VERB_START: /* start */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_start_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 16:
                        ret_cod = lixa_msg_deserialize_start_16(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 24:
                        ret_cod = lixa_msg_deserialize_start_24(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP3);
                }
                break;
            case LIXA_MSG_VERB_END: /* end */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_end_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 16:
                        ret_cod = lixa_msg_deserialize_end_16(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 24:
                        ret_cod = lixa_msg_deserialize_end_24(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP4);
                }
                break;
            case LIXA_MSG_VERB_PREPARE: /* prepare */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_prepare_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 16:
                        ret_cod = lixa_msg_deserialize_prepare_16(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP5);
                }
                break;
            case LIXA_MSG_VERB_COMMIT: /* commit */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_commit_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP6);
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
            case XML_READ_MEMORY:
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
            case INVALID_STEP2:
            case INVALID_STEP3:
            case INVALID_STEP4:
            case INVALID_STEP5:
            case INVALID_STEP6:
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



int lixa_msg_deserialize_close_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { RMID_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_close_8\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_RSRMGRS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.close_8.rsrmgrs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_close_8_rsrmgr_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    struct lixa_msg_body_close_8_rsrmgr_s rsrmgr;
                    if (!xmlStrcmp(cur2->name, LIXA_XML_MSG_TAG_RSRMGR)) {
                        xmlChar *tmp;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        rsrmgr.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.close_8.rsrmgrs, rsrmgr);
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
    LIXA_TRACE(("lixa_msg_deserialize_close_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_commit_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { FINISHED_NOT_FOUND
                     , XA_INFO_NOT_FOUND
                     , RMID_NOT_FOUND
                     , FLAGS_NOT_FOUND
                     , RC_NOT_FOUND
                     , STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_commit_8\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                /* retrieve control thread properties */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_FINISHED)))
                    THROW(FINISHED_NOT_FOUND);
                msg->body.commit_8.conthr.finished =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name,
                                  LIXA_XML_MSG_TAG_XA_COMMIT_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.commit_8.xa_commit_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_commit_8_xa_commit_execs_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name,
                                   LIXA_XML_MSG_TAG_XA_COMMIT_EXEC)) {
                        struct lixa_msg_body_commit_8_xa_commit_execs_s
                            xa_commit_exec;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        xa_commit_exec.rmid =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve flags */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FLAGS)))
                            THROW(FLAGS_NOT_FOUND);
                        xa_commit_exec.flags = strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        xa_commit_exec.rc = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve r_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_R_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_commit_exec.r_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve s_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_S_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_commit_exec.s_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.commit_8.xa_commit_execs,
                                           xa_commit_exec);
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
            case FINISHED_NOT_FOUND:
            case XA_INFO_NOT_FOUND:
            case RMID_NOT_FOUND:
            case FLAGS_NOT_FOUND:
            case RC_NOT_FOUND:
            case STATE_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_commit_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_default_answer(xmlNodePtr cur,
                                        struct lixa_msg_body_answer_s *answer)
{
    enum Exception { RC_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_default_answer\n"));
    TRY {
        xmlChar *tmp = NULL;

        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_ANSWER)) {
                /* retrieve answer result */
                if (NULL == (tmp = (xmlGetProp(cur, LIXA_XML_MSG_PROP_RC))))
                    THROW(RC_NOT_FOUND);
                answer->rc = (int)strtol((char *)tmp, NULL, 0);
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
    LIXA_TRACE(("lixa_msg_deserialize_default_answer/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_end_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { COMMIT_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_end_8\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                xmlChar *tmp;
                /* retrieve commit property */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_COMMIT)))
                    THROW(COMMIT_NOT_FOUND);
                msg->body.end_8.conthr.commit =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else
                THROW(XML_UNRECOGNIZED_TAG);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COMMIT_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_end_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_end_16(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    int ret_cod;
    
    LIXA_TRACE(("lixa_msg_deserialize_end_16\n"));
    ret_cod = lixa_msg_deserialize_default_answer(
        cur, &msg->body.end_16.answer);
    LIXA_TRACE(("lixa_msg_deserialize_end_16/"
                "ret_cod=%d/errno=%d\n", ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_end_24(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { CONTHR_NOT_FOUND
                     , XA_INFO_NOT_FOUND
                     , RMID_NOT_FOUND
                     , FLAGS_NOT_FOUND
                     , RC_NOT_FOUND
                     , S_STATE_NOT_FOUND
                     , T_STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_end_24\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name,
                           LIXA_XML_MSG_TAG_XA_END_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.end_24.xa_end_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_end_24_xa_end_execs_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name,
                                   LIXA_XML_MSG_TAG_XA_END_EXEC)) {
                        struct lixa_msg_body_end_24_xa_end_execs_s
                            xa_end_exec;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        xa_end_exec.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve flags */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FLAGS)))
                            THROW(FLAGS_NOT_FOUND);
                        xa_end_exec.flags = strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        xa_end_exec.rc = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve s_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_S_STATE)))
                            THROW(S_STATE_NOT_FOUND);
                        xa_end_exec.s_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve t_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_T_STATE)))
                            THROW(T_STATE_NOT_FOUND);
                        xa_end_exec.t_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.end_24.xa_end_execs,
                                           xa_end_exec);
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
            case CONTHR_NOT_FOUND:
            case XA_INFO_NOT_FOUND:
            case RMID_NOT_FOUND:
            case FLAGS_NOT_FOUND:
            case RC_NOT_FOUND:
            case S_STATE_NOT_FOUND:
            case T_STATE_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_end_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_open_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { PROFILE_NOT_FOUND
                     , RMID_NOT_FOUND
                     , NAME_NOT_FOUND
                     , XA_NAME_NOT_FOUND
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
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
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
                        /* retrieve xa_name */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_XA_NAME)))
                            THROW(XA_NAME_NOT_FOUND);
                        rsrmgr.xa_name = tmp;
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
            case PROFILE_NOT_FOUND:
            case RMID_NOT_FOUND:
            case NAME_NOT_FOUND:
            case XA_NAME_NOT_FOUND:
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
    int ret_cod;
    
    LIXA_TRACE(("lixa_msg_deserialize_open_16\n"));
    ret_cod = lixa_msg_deserialize_default_answer(
        cur, &msg->body.open_16.answer);
    LIXA_TRACE(("lixa_msg_deserialize_open_16/"
                "ret_cod=%d/errno=%d\n", ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_open_24(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { CONTHR_NOT_FOUND
                     , XA_INFO_NOT_FOUND
                     , RMID_NOT_FOUND
                     , FLAGS_NOT_FOUND
                     , RC_NOT_FOUND
                     , STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_open_24\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                /* retrieve control thread properties */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_STATE)))
                    THROW(CONTHR_NOT_FOUND);
                msg->body.open_24.conthr.state =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_XA_OPEN_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.open_24.xa_open_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_open_24_xa_open_execs_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name,
                                   LIXA_XML_MSG_TAG_XA_OPEN_EXEC)) {
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
                        /* retrieve state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_R_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_open_exec.r_state =
                            (int)strtol((char *)tmp, NULL, 0);
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
            case CONTHR_NOT_FOUND:
            case XA_INFO_NOT_FOUND:
            case RMID_NOT_FOUND:
            case FLAGS_NOT_FOUND:
            case RC_NOT_FOUND:
            case STATE_NOT_FOUND:
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



int lixa_msg_deserialize_prepare_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { COMMIT_NOT_FOUND
                     , XA_INFO_NOT_FOUND
                     , RMID_NOT_FOUND
                     , FLAGS_NOT_FOUND
                     , RC_NOT_FOUND
                     , STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_prepare_8\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                /* retrieve control thread properties */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_COMMIT)))
                    THROW(COMMIT_NOT_FOUND);
                msg->body.prepare_8.conthr.commit =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name,
                                  LIXA_XML_MSG_TAG_XA_PREPARE_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.prepare_8.xa_prepare_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_prepare_8_xa_prepare_execs_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name,
                                   LIXA_XML_MSG_TAG_XA_PREPARE_EXEC)) {
                        struct lixa_msg_body_prepare_8_xa_prepare_execs_s
                            xa_prepare_exec;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        xa_prepare_exec.rmid =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve flags */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FLAGS)))
                            THROW(FLAGS_NOT_FOUND);
                        xa_prepare_exec.flags = strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        xa_prepare_exec.rc = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve s_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_S_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_prepare_exec.s_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve t_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_T_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_prepare_exec.t_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.prepare_8.xa_prepare_execs,
                                           xa_prepare_exec);
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
            case COMMIT_NOT_FOUND:
            case XA_INFO_NOT_FOUND:
            case RMID_NOT_FOUND:
            case FLAGS_NOT_FOUND:
            case RC_NOT_FOUND:
            case STATE_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_prepare_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_prepare_16(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    int ret_cod;
    
    LIXA_TRACE(("lixa_msg_deserialize_prepare_16\n"));
    ret_cod = lixa_msg_deserialize_default_answer(
        cur, &msg->body.prepare_16.answer);
    LIXA_TRACE(("lixa_msg_deserialize_prepare_16/"
                "ret_cod=%d/errno=%d\n", ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_start_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { XID_NOT_FOUND
                     , XID_DESERIALIZE_ERROR
                     , RMID_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_start_8\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                xmlChar *tmp;
                /* retrieve xid properties */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_XID)))
                    THROW(XID_NOT_FOUND);
                if (LIXA_RC_OK != (ret_cod = xid_deserialize(
                                       (char *)tmp, 
                                       &msg->body.start_8.conthr.xid))) {
                    xmlFree(tmp);
                    THROW(XID_DESERIALIZE_ERROR);
                } else
                    xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_RSRMGRS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.start_8.rsrmgrs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_start_8_rsrmgr_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name, LIXA_XML_MSG_TAG_RSRMGR)) {
                        xmlChar *tmp;
                        struct lixa_msg_body_start_8_rsrmgr_s rsrmgr;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        rsrmgr.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.start_8.rsrmgrs, rsrmgr);
                        cur2 = cur2->next;
                    }
                } /* while (NULL != child) */
            } else
                THROW(XML_UNRECOGNIZED_TAG);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XID_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case XID_DESERIALIZE_ERROR:
                break;
            case RMID_NOT_FOUND:
            case XML_UNRECOGNIZED_TAG:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize_start_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_start_16(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    int ret_cod;
    
    LIXA_TRACE(("lixa_msg_deserialize_start_16\n"));
    ret_cod = lixa_msg_deserialize_default_answer(
        cur, &msg->body.start_16.answer);
    LIXA_TRACE(("lixa_msg_deserialize_start_16/"
                "ret_cod=%d/errno=%d\n", ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_start_24(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { CONTHR_NOT_FOUND
                     , XA_INFO_NOT_FOUND
                     , RMID_NOT_FOUND
                     , FLAGS_NOT_FOUND
                     , RC_NOT_FOUND
                     , STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_start_24\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                /* retrieve control thread properties */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_STATE)))
                    THROW(CONTHR_NOT_FOUND);
                msg->body.start_24.conthr.state =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name,
                                  LIXA_XML_MSG_TAG_XA_START_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.start_24.xa_start_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_start_24_xa_start_execs_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name,
                                   LIXA_XML_MSG_TAG_XA_START_EXEC)) {
                        struct lixa_msg_body_start_24_xa_start_execs_s
                            xa_start_exec;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        xa_start_exec.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve flags */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FLAGS)))
                            THROW(FLAGS_NOT_FOUND);
                        xa_start_exec.flags = strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        xa_start_exec.rc = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_T_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_start_exec.t_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.start_24.xa_start_execs,
                                           xa_start_exec);
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
            case CONTHR_NOT_FOUND:
            case XA_INFO_NOT_FOUND:
            case RMID_NOT_FOUND:
            case FLAGS_NOT_FOUND:
            case RC_NOT_FOUND:
            case STATE_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_start_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



