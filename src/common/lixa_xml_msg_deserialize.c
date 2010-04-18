/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
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
                     , INVALID_STEP7
                     , INVALID_STEP8
                     , INVALID_VERB
                     , CHILD_LEVEL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlDocPtr doc = NULL;
    xmlNodePtr cur = NULL;
    
    LIXA_TRACE(("lixa_msg_deserialize\n"));
    TRY {
        xmlChar *tmp = NULL;

        LIXA_TRACE(("lixa_msg_deserialize: deserializing message |%*.*s|\n",
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
            case LIXA_MSG_VERB_ROLLBACK: /* rollback */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_rollback_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP7);
                }
                break;
            case LIXA_MSG_VERB_QRCVR: /* qrcvr */
                switch (msg->header.pvs.step) {
                    case 8:
                        ret_cod = lixa_msg_deserialize_qrcvr_8(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 16:
                        ret_cod = lixa_msg_deserialize_qrcvr_16(
                            cur->xmlChildrenNode, msg);
                        break;
                    case 24:
                        ret_cod = lixa_msg_deserialize_qrcvr_24(
                            cur->xmlChildrenNode, msg);
                        break;
                    default:
                        THROW(INVALID_STEP8);
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
            case INVALID_STEP7:
            case INVALID_STEP8:
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
                     , TD_STATE_NOT_FOUND
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
                        /* retrieve td_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_TD_STATE)))
                            THROW(TD_STATE_NOT_FOUND);
                        xa_end_exec.td_state =
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
            case TD_STATE_NOT_FOUND:
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
    enum Exception { JOB_NOT_FOUND
                     , CONFIG_DIGEST_NOT_FOUND
                     , RMID_NOT_FOUND
                     , DYNAMIC_NOT_FOUND
                     , NAME_NOT_FOUND
                     , XA_NAME_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_open_8\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CLIENT)) {
                xmlChar *tmp;
                /* retrieve client properties */
                if (NULL == (msg->body.open_8.client.job =
                             xmlGetProp(cur, LIXA_XML_MSG_PROP_JOB)))
                    THROW(JOB_NOT_FOUND);
                if (NULL == (tmp = xmlGetProp(
                                 cur, LIXA_XML_MSG_PROP_CONFIG_DIGEST)))
                    THROW(CONFIG_DIGEST_NOT_FOUND);
                strncpy(msg->body.open_8.client.config_digest,
                        (char *)tmp, sizeof(md5_digest_hex_t));
                msg->body.open_8.client.config_digest[
                    MD5_DIGEST_LENGTH * 2] = '\0';
                xmlFree(tmp);
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
                        /* retrieve dynamic */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_DYNAMIC)))
                            THROW(DYNAMIC_NOT_FOUND);
                        rsrmgr.dynamic = (int)strtol((char *)tmp, NULL, 0);
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
            case JOB_NOT_FOUND:
            case CONFIG_DIGEST_NOT_FOUND:
            case RMID_NOT_FOUND:
            case DYNAMIC_NOT_FOUND:
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
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_TXSTATE)))
                    THROW(CONTHR_NOT_FOUND);
                msg->body.open_24.conthr.txstate =
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
                     , S_STATE_NOT_FOUND
                     , TD_STATE_NOT_FOUND
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
                            THROW(S_STATE_NOT_FOUND);
                        xa_prepare_exec.s_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve td_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_TD_STATE)))
                            THROW(TD_STATE_NOT_FOUND);
                        xa_prepare_exec.td_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(
                            msg->body.prepare_8.xa_prepare_execs,
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
            case S_STATE_NOT_FOUND:
            case TD_STATE_NOT_FOUND:
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



int lixa_msg_deserialize_qrcvr_8(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { JOB_NOT_FOUND
                     , CONFIG_DIGEST_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_qrcvr_8\n"));
    TRY {
        while (NULL != cur) {
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CLIENT)) {
                xmlChar *tmp;
                /* retrieve client properties */
                if (NULL == (msg->body.qrcvr_8.client.job =
                             xmlGetProp(cur, LIXA_XML_MSG_PROP_JOB)))
                    THROW(JOB_NOT_FOUND);
                if (NULL == (tmp = xmlGetProp(
                                 cur, LIXA_XML_MSG_PROP_CONFIG_DIGEST)))
                    THROW(CONFIG_DIGEST_NOT_FOUND);
                strncpy(msg->body.qrcvr_8.client.config_digest,
                        (char *)tmp, sizeof(md5_digest_hex_t));
                msg->body.qrcvr_8.client.config_digest[
                    MD5_DIGEST_LENGTH * 2] = '\0';
                xmlFree(tmp);
            } else
                THROW(XML_UNRECOGNIZED_TAG);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case JOB_NOT_FOUND:
            case CONFIG_DIGEST_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_qrcvr_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_qrcvr_16(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { RC_NOT_FOUND
                     , JOB_NOT_FOUND
                     , CONFIG_DIGEST_NOT_FOUND
                     , PROP_VERB_NOT_FOUND
                     , PROP_STEP_NOT_FOUND
                     , PROP_FINISHED_NOT_FOUND
                     , PROP_TXSTATE_NOT_FOUND
                     , PROP_WILL_COMMIT_NOT_FOUND
                     , PROP_WILL_ROLLBACK_NOT_FOUND
                     , XID_NOT_FOUND
                     , XID_DESERIALIZE_ERROR
                     , XML_UNRECOGNIZED_TAG1
                     , RMID_NOT_FOUND
                     , NEXT_VERB_NOT_FOUND
                     , R_STATE_NOT_FOUND
                     , S_STATE_NOT_FOUND
                     , TD_STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_qrcvr_16\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_ANSWER)) {
                /* retrieve answer result */
                if (NULL == (tmp = (xmlGetProp(cur, LIXA_XML_MSG_PROP_RC))))
                    THROW(RC_NOT_FOUND);
                msg->body.qrcvr_16.answer.rc =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CLIENT)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* retrieve client properties */
                if (NULL == (msg->body.qrcvr_16.client.job =
                             xmlGetProp(cur, LIXA_XML_MSG_PROP_JOB)))
                    THROW(JOB_NOT_FOUND);
                if (NULL == (tmp = xmlGetProp(
                                 cur, LIXA_XML_MSG_PROP_CONFIG_DIGEST)))
                    THROW(CONFIG_DIGEST_NOT_FOUND);
                strncpy(msg->body.qrcvr_16.client.config_digest,
                        (char *)tmp, sizeof(md5_digest_hex_t));
                msg->body.qrcvr_16.client.config_digest[
                    MD5_DIGEST_LENGTH * 2] = '\0';
                xmlFree(tmp);

                while (NULL != cur2) {
                    if (!xmlStrcmp(
                            cur2->name, LIXA_XML_MSG_TAG_LAST_VERB_STEP)) {
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_VERB)))
                            THROW(PROP_VERB_NOT_FOUND);
                        msg->body.qrcvr_16.client.last_verb_step.verb =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_STEP)))
                            THROW(PROP_STEP_NOT_FOUND);
                        msg->body.qrcvr_16.client.last_verb_step.step =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                    } else if (!xmlStrcmp(cur2->name,
                                          LIXA_XML_MSG_TAG_STATE)) {
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FINISHED)))
                            THROW(PROP_FINISHED_NOT_FOUND);
                        msg->body.qrcvr_16.client.state.finished =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_TXSTATE)))
                            THROW(PROP_TXSTATE_NOT_FOUND);
                        msg->body.qrcvr_16.client.state.txstate =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_WILL_COMMIT)))
                            THROW(PROP_WILL_COMMIT_NOT_FOUND);
                        msg->body.qrcvr_16.client.state.will_commit =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        if (NULL == (
                                tmp = xmlGetProp(
                                    cur2, LIXA_XML_MSG_PROP_WILL_ROLLBACK)))
                            THROW(PROP_WILL_ROLLBACK_NOT_FOUND);
                        msg->body.qrcvr_16.client.state.will_rollback =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve xid properties */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_XID)))
                            THROW(XID_NOT_FOUND);
                        if (LIXA_RC_OK != (
                                ret_cod = xid_deserialize(
                                    (char *)tmp, 
                                    &msg->body.qrcvr_16.client.state.xid))) {
                            xmlFree(tmp);
                            THROW(XID_DESERIALIZE_ERROR);
                        } else
                            xmlFree(tmp);  
                    } else
                        THROW(XML_UNRECOGNIZED_TAG1);
                    cur2 = cur2->next;
                } /* while (NULL != child) */
            } else if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_RSRMGRS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.qrcvr_16.rsrmgrs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_qrcvr_16_rsrmgr_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name, LIXA_XML_MSG_TAG_RSRMGR)) {
                        struct lixa_msg_body_qrcvr_16_rsrmgr_s rsrmgr;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        rsrmgr.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* next_verb */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_NEXT_VERB)))
                            THROW(NEXT_VERB_NOT_FOUND);
                        rsrmgr.next_verb = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* r_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_R_STATE)))
                            THROW(R_STATE_NOT_FOUND);
                        rsrmgr.r_state = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* s_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_S_STATE)))
                            THROW(S_STATE_NOT_FOUND);
                        rsrmgr.s_state = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* td_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_TD_STATE)))
                            THROW(TD_STATE_NOT_FOUND);
                        rsrmgr.td_state = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.qrcvr_16.rsrmgrs, rsrmgr);
                    }
                    cur2 = cur2->next;
                } /* while (NULL != child) */
            } else
                THROW(XML_UNRECOGNIZED_TAG2);
            cur = cur->next;
        } /* while (NULL != cur) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RC_NOT_FOUND:
            case JOB_NOT_FOUND:
            case CONFIG_DIGEST_NOT_FOUND:
            case PROP_VERB_NOT_FOUND:
            case PROP_STEP_NOT_FOUND:
            case PROP_FINISHED_NOT_FOUND:
            case PROP_TXSTATE_NOT_FOUND:
            case PROP_WILL_COMMIT_NOT_FOUND:
            case PROP_WILL_ROLLBACK_NOT_FOUND:
            case XID_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case XID_DESERIALIZE_ERROR:
                break;
            case XML_UNRECOGNIZED_TAG1:
                ret_cod = LIXA_RC_XML_UNRECOGNIZED_TAG;
                break;
            case RMID_NOT_FOUND:
            case NEXT_VERB_NOT_FOUND:
            case R_STATE_NOT_FOUND:
            case S_STATE_NOT_FOUND:
            case TD_STATE_NOT_FOUND:
                ret_cod = LIXA_RC_MALFORMED_XML_MSG;
                break;
            case XML_UNRECOGNIZED_TAG2:
                ret_cod = LIXA_RC_XML_UNRECOGNIZED_TAG;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_deserialize_qrcvr_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_qrcvr_24(xmlNodePtr cur, struct lixa_msg_s *msg)
{
    enum Exception { FAILED_NOT_FOUND
                     , COMMIT_NOT_FOUND
                     , RMID_NOT_FOUND
                     , RC_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_qrcvr_24\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_RECOVERY)) {
                /* retrieve recovery properties */
                if (NULL == (tmp = (xmlGetProp(cur,
                                               LIXA_XML_MSG_PROP_FAILED))))
                    THROW(FAILED_NOT_FOUND);
                msg->body.qrcvr_24.recovery.failed =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
                if (NULL == (tmp = (xmlGetProp(cur,
                                               LIXA_XML_MSG_PROP_COMMIT))))
                    THROW(COMMIT_NOT_FOUND);
                msg->body.qrcvr_24.recovery.commit =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_RSRMGRS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.qrcvr_24.rsrmgrs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct lixa_msg_body_qrcvr_24_rsrmgr_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name, LIXA_XML_MSG_TAG_RSRMGR)) {
                        struct lixa_msg_body_qrcvr_24_rsrmgr_s rsrmgr;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        rsrmgr.rmid = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        rsrmgr.rc = (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.qrcvr_24.rsrmgrs, rsrmgr);
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
            case FAILED_NOT_FOUND:
            case COMMIT_NOT_FOUND:
            case RMID_NOT_FOUND:
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
    LIXA_TRACE(("lixa_msg_deserialize_qrcvr_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_deserialize_rollback_8(xmlNodePtr cur, struct lixa_msg_s *msg)
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
    
    LIXA_TRACE(("lixa_msg_deserialize_rollback_8\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                /* retrieve control thread properties */
                if (NULL == (tmp = xmlGetProp(
                                 cur, LIXA_XML_MSG_PROP_FINISHED)))
                    THROW(FINISHED_NOT_FOUND);
                msg->body.rollback_8.conthr.finished =
                    (int)strtol((char *)tmp, NULL, 0);
                xmlFree(tmp);
            } else if (!xmlStrcmp(cur->name,
                                  LIXA_XML_MSG_TAG_XA_ROLLBACK_EXECS)) {
                xmlNodePtr cur2 = cur->xmlChildrenNode;
                /* initialize array (3 slots may be a good choice for
                   initial size) */
                msg->body.rollback_8.xa_rollback_execs = g_array_sized_new(
                    FALSE, FALSE,
                    sizeof(struct
                           lixa_msg_body_rollback_8_xa_rollback_execs_s),
                    LIXA_MSG_XML_DEFAULT_RSRMGRS);
                /* retrieve resource managers */
                while (NULL != cur2) {
                    if (!xmlStrcmp(cur2->name,
                                   LIXA_XML_MSG_TAG_XA_ROLLBACK_EXEC)) {
                        struct lixa_msg_body_rollback_8_xa_rollback_execs_s
                            xa_rollback_exec;
                        /* retrieve rmid */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RMID)))
                            THROW(RMID_NOT_FOUND);
                        xa_rollback_exec.rmid =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve flags */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_FLAGS)))
                            THROW(FLAGS_NOT_FOUND);
                        xa_rollback_exec.flags = strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve rc */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_RC)))
                            THROW(RC_NOT_FOUND);
                        xa_rollback_exec.rc = (int)strtol((char *)tmp,
                                                          NULL, 0);
                        xmlFree(tmp);
                        /* retrieve r_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_R_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_rollback_exec.r_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        /* retrieve s_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_S_STATE)))
                            THROW(STATE_NOT_FOUND);
                        xa_rollback_exec.s_state =
                            (int)strtol((char *)tmp, NULL, 0);
                        xmlFree(tmp);
                        g_array_append_val(msg->body.rollback_8.
                                           xa_rollback_execs,
                                           xa_rollback_exec);
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
    LIXA_TRACE(("lixa_msg_deserialize_rollback_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
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
                     , TD_STATE_NOT_FOUND
                     , XML_UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_deserialize_start_24\n"));
    TRY {
        while (NULL != cur) {
            xmlChar *tmp;
            if (!xmlStrcmp(cur->name, LIXA_XML_MSG_TAG_CONTHR)) {
                /* retrieve control thread properties */
                if (NULL == (tmp = xmlGetProp(cur, LIXA_XML_MSG_PROP_TXSTATE)))
                    THROW(CONTHR_NOT_FOUND);
                msg->body.start_24.conthr.txstate =
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
                        /* retrieve td_state */
                        if (NULL == (tmp = xmlGetProp(
                                         cur2, LIXA_XML_MSG_PROP_TD_STATE)))
                            THROW(TD_STATE_NOT_FOUND);
                        xa_start_exec.td_state =
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
            case TD_STATE_NOT_FOUND:
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



