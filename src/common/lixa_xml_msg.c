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
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg.h>
#include <lixa_common_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



const xmlChar *LIXA_XML_MSG_HEADER =               (xmlChar *)"<?xml";
const xmlChar *LIXA_XML_MSG_PROP_COMMIT =          (xmlChar *)"commit";
const xmlChar *LIXA_XML_MSG_PROP_FINISHED =        (xmlChar *)"finished";
const xmlChar *LIXA_XML_MSG_PROP_FLAGS =           (xmlChar *)"flags";
const xmlChar *LIXA_XML_MSG_PROP_LEVEL =           (xmlChar *)"level";
const xmlChar *LIXA_XML_MSG_PROP_NAME =            (xmlChar *)"name";
const xmlChar *LIXA_XML_MSG_PROP_PROFILE =         (xmlChar *)"profile";
const xmlChar *LIXA_XML_MSG_PROP_RC =              (xmlChar *)"rc";
const xmlChar *LIXA_XML_MSG_PROP_RMID =            (xmlChar *)"rmid";
const xmlChar *LIXA_XML_MSG_PROP_R_STATE =         (xmlChar *)"r_state";
const xmlChar *LIXA_XML_MSG_PROP_S_STATE =         (xmlChar *)"s_state";
const xmlChar *LIXA_XML_MSG_PROP_T_STATE =         (xmlChar *)"t_state";
const xmlChar *LIXA_XML_MSG_PROP_STATE =           (xmlChar *)"state";
const xmlChar *LIXA_XML_MSG_PROP_STEP =            (xmlChar *)"step";
const xmlChar *LIXA_XML_MSG_PROP_VERB =            (xmlChar *)"verb";
const xmlChar *LIXA_XML_MSG_PROP_XA_INFO =         (xmlChar *)"xa_info";
const xmlChar *LIXA_XML_MSG_PROP_XA_NAME =         (xmlChar *)"xa_name";
const xmlChar *LIXA_XML_MSG_PROP_XID =             (xmlChar *)"xid";
const xmlChar *LIXA_XML_MSG_TAG_ANSWER =           (xmlChar *)"answer";
const xmlChar *LIXA_XML_MSG_TAG_CLIENT =           (xmlChar *)"client";
const xmlChar *LIXA_XML_MSG_TAG_CONTHR =           (xmlChar *)"conthr";
const xmlChar *LIXA_XML_MSG_TAG_MSG =              (xmlChar *)"msg";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGR =           (xmlChar *)"rsrmgr";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGRS =          (xmlChar *)"rsrmgrs";
const xmlChar *LIXA_XML_MSG_TAG_XA_END_EXEC =      (xmlChar *)"xa_end_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_END_EXECS =     (xmlChar *)"xa_end_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_COMMIT_EXEC =   (xmlChar *)"xa_commit_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_COMMIT_EXECS =  (xmlChar *)"xa_commit_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXEC =     (xmlChar *)"xa_open_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXECS =    (xmlChar *)"xa_open_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_PREPARE_EXEC =  (xmlChar *)"xa_prepare_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_PREPARE_EXECS = (xmlChar *)"xa_prepare_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_ROLLBACK_EXEC = (xmlChar *)"xa_rollback_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_ROLLBACK_EXECS =(xmlChar *)"xa_rollback_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_START_EXEC =    (xmlChar *)"xa_start_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_START_EXECS =   (xmlChar *)"xa_start_execs";



int lixa_msg_retrieve(int fd,
                      char *buf, size_t buf_size,
                      ssize_t *read_bytes)
{
    enum Exception { RECV_ERROR1
                     , CONNECTION_CLOSED
                     , INVALID_PREFIX_SIZE
                     , BUFFER_OVERFLOW
                     , RECV_ERROR2
                     , INVALID_LENGTH_XML_MSG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_retrieve\n"));
    TRY {
        char prefix[LIXA_MSG_XML_PREFIX_DIGITS+1];
        ssize_t to_read = 0;

        /* read the prefix to determine message size */
        if (0 > (*read_bytes = recv(
                     fd, prefix, LIXA_MSG_XML_PREFIX_DIGITS, 0))) {
            THROW(RECV_ERROR1);
        } else if (*read_bytes == 0) {
            THROW(CONNECTION_CLOSED);
        } else if (*read_bytes != LIXA_MSG_XML_PREFIX_DIGITS) {
            /* retrieve XML message size */
            LIXA_TRACE(("lixa_msg_retrieve: peer sent "
                        SSIZE_T_FORMAT " bytes, expected %d bytes for "
                        "XML message prefix\n", read_bytes,
                        LIXA_MSG_XML_PREFIX_DIGITS));
            THROW(INVALID_PREFIX_SIZE);
        } else {
            prefix[LIXA_MSG_XML_PREFIX_DIGITS] = '\0';
            to_read = strtol(prefix, NULL, 10);
            LIXA_TRACE(("lixa_msg_retrieve: XML message prefix "
                        "is '%s' (" SSIZE_T_FORMAT ")\n", prefix, to_read));
        }

        if (to_read > buf_size)
            THROW(BUFFER_OVERFLOW);
        
        if (0 > (*read_bytes = recv(fd, buf, to_read, 0)))
            THROW(RECV_ERROR2);
        
        LIXA_TRACE(("lixa_msg_retrieve: fd = %d returned "
                    SSIZE_T_FORMAT " bytes\n", fd, *read_bytes));
        if (to_read != *read_bytes) {
            LIXA_TRACE(("lixa_msg_retrieve: expected " SSIZE_T_FORMAT
                        " bytes, received " SSIZE_T_FORMAT " bytes\n",
                        to_read, *read_bytes));
            THROW(INVALID_LENGTH_XML_MSG);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RECV_ERROR1:
                ret_cod = LIXA_RC_RECV_ERROR;
                break;
            case CONNECTION_CLOSED:
                ret_cod = LIXA_RC_CONNECTION_CLOSED;
                break;
            case INVALID_PREFIX_SIZE:
                ret_cod = LIXA_RC_INVALID_PREFIX_SIZE;
                break;
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case RECV_ERROR2:
                ret_cod = LIXA_RC_RECV_ERROR;
                break;
            case INVALID_LENGTH_XML_MSG:
                ret_cod = LIXA_RC_INVALID_LENGTH_XML_MSG;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_retrieve/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_free(struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP1
                     , INVALID_STEP2
                     , INVALID_STEP3
                     , INVALID_STEP4
                     , INVALID_STEP5
                     , INVALID_STEP6
                     , INVALID_VERB
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_free\n"));
    TRY {
        guint i;
        switch (msg->header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                switch (msg->header.pvs.step) {
                    case 8:
                        xmlFree(msg->body.open_8.client.profile);
                        msg->body.open_8.client.profile = NULL;
                        for (i=0; i<msg->body.open_8.rsrmgrs->len; ++i) {
                            struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr =
                                &g_array_index(
                                    msg->body.open_8.rsrmgrs,
                                    struct lixa_msg_body_open_8_rsrmgr_s, i);
                            xmlFree(rsrmgr->name);
                            xmlFree(rsrmgr->xa_name);
                        }
                        g_array_free(msg->body.open_8.rsrmgrs, TRUE);
                        msg->body.open_8.rsrmgrs = NULL;
                        break;
                    case 24:
                        for (i=0; i<msg->body.open_24.xa_open_execs->len;
                             ++i) {
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
            case LIXA_MSG_VERB_CLOSE: /* close */
                switch (msg->header.pvs.step) {
                    case 8:
                        g_array_free(msg->body.close_8.rsrmgrs, TRUE);
                        msg->body.open_8.rsrmgrs = NULL;
                        break;
                    default:
                        THROW(INVALID_STEP2);
                }
                break;
            case LIXA_MSG_VERB_START: /* start */
                switch (msg->header.pvs.step) {
                    case 8:
                        g_array_free(msg->body.start_8.rsrmgrs, TRUE);
                        msg->body.start_8.rsrmgrs = NULL;
                        break;
                    case 24:
                        g_array_free(msg->body.start_24.xa_start_execs, TRUE);
                        msg->body.start_24.xa_start_execs = NULL;
                        break;
                    default:
                        THROW(INVALID_STEP3);
                }
                break;
            case LIXA_MSG_VERB_END: /* end */
                switch (msg->header.pvs.step) {
                    case 8: /* nothing to do */
                        break;
                    case 24:
                        g_array_free(msg->body.end_24.xa_end_execs, TRUE);
                        msg->body.end_24.xa_end_execs = NULL;
                        break;
                    default:
                        THROW(INVALID_STEP4);
                }
                break;
            case LIXA_MSG_VERB_PREPARE: /* prepare */
                switch (msg->header.pvs.step) {
                    case 8:
                        g_array_free(msg->body.prepare_8.xa_prepare_execs,
                                     TRUE);
                        msg->body.prepare_8.xa_prepare_execs = NULL;
                        break;
                    default:
                        THROW(INVALID_STEP5);
                }
                break;
            case LIXA_MSG_VERB_COMMIT: /* commit */
                switch (msg->header.pvs.step) {
                    case 8:
                        g_array_free(msg->body.commit_8.xa_commit_execs,
                                     TRUE);
                        msg->body.commit_8.xa_commit_execs = NULL;
                        break;
                    default:
                        THROW(INVALID_STEP6);
                }
                break;
            default:
                THROW(INVALID_VERB);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP1:
            case INVALID_STEP2:
            case INVALID_STEP3:
            case INVALID_STEP4:
            case INVALID_STEP5:
            case INVALID_STEP6:
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
