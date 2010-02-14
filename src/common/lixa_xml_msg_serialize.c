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
#include <lixa_xml_msg_serialize.h>
#include <lixa_common_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



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
                     , SERIALIZE_CLOSE_8_ERROR
                     , INVALID_CLOSE_STEP
                     , SERIALIZE_START_8_ERROR
                     , SERIALIZE_START_16_ERROR
                     , SERIALIZE_START_24_ERROR
                     , INVALID_START_STEP
                     , SERIALIZE_END_8_ERROR
                     , SERIALIZE_END_16_ERROR
                     , SERIALIZE_END_24_ERROR
                     , INVALID_END_STEP
                     , SERIALIZE_PREPARE_8_ERROR
                     , SERIALIZE_PREPARE_16_ERROR
                     , INVALID_PREPARE_STEP
                     , SERIALIZE_COMMIT_8_ERROR
                     , INVALID_COMMIT_STEP
                     , SERIALIZE_ROLLBACK_8_ERROR
                     , INVALID_ROLLBACK_STEP
                     , INVALID_VERB
                     , BUFFER_TOO_SHORT3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int used_chars = 0;
    size_t free_chars = buffer_len, offset = 0;
    char prefix[LIXA_MSG_XML_PREFIX_DIGITS + 1];
    
    LIXA_TRACE(("lixa_msg_serialize\n"));
    TRY {
        /* reserving space for prefix size */
        free_chars -= LIXA_MSG_XML_PREFIX_DIGITS;
        offset += LIXA_MSG_XML_PREFIX_DIGITS;
        /* <xml ... > */
        used_chars = snprintf(buffer + offset, free_chars,
                              "%s version=\"1.0\" encoding=\"UTF-8\" ?>",
                              LIXA_XML_MSG_HEADER);
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
                              msg->header.pvs.verb,
                              LIXA_XML_MSG_PROP_STEP,
                              msg->header.pvs.step);
        if (used_chars >= free_chars)
            THROW(BUFFER_TOO_SHORT2);
        free_chars -= used_chars;
        offset += used_chars;

        switch (msg->header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN:
                switch (msg->header.pvs.step) {
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
            case LIXA_MSG_VERB_CLOSE:
                switch (msg->header.pvs.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_close_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_CLOSE_8_ERROR);
                        break;
                    default:
                        THROW(INVALID_CLOSE_STEP);
                }
                break;
            case LIXA_MSG_VERB_START:
                switch (msg->header.pvs.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_start_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_START_8_ERROR);
                        break;
                    case 16:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_start_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_START_16_ERROR);
                        break;
                    case 24:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_start_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_START_24_ERROR);
                        break;
                    default:
                        THROW(INVALID_START_STEP);
                }
                break;
            case LIXA_MSG_VERB_END:
                switch (msg->header.pvs.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_end_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_END_8_ERROR);
                        break;
                    case 16:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_end_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_END_16_ERROR);
                        break;
                    case 24:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_end_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_END_24_ERROR);
                        break;
                    default:
                        THROW(INVALID_END_STEP);
                }
                break;
            case LIXA_MSG_VERB_PREPARE:
                switch (msg->header.pvs.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_prepare_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_PREPARE_8_ERROR);
                        break;
                    case 16:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_prepare_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_PREPARE_16_ERROR);
                        break;
                    default:
                        THROW(INVALID_PREPARE_STEP);
                }
                break;
            case LIXA_MSG_VERB_COMMIT:
                switch (msg->header.pvs.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_commit_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_COMMIT_8_ERROR);
                        break;
                    default:
                        THROW(INVALID_COMMIT_STEP);
                }
                break;
            case LIXA_MSG_VERB_ROLLBACK:
                switch (msg->header.pvs.step) {
                    case 8:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_rollback_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_ROLLBACK_8_ERROR);
                        break;
                    default:
                        THROW(INVALID_ROLLBACK_STEP);
                }
                break;
            default:
                THROW(INVALID_VERB);
        }
        /* </msg> */
        used_chars = snprintf(buffer + offset, free_chars,
                              "</%s>", LIXA_XML_MSG_TAG_MSG);
        if (used_chars >= free_chars)
            THROW(BUFFER_TOO_SHORT3);
        free_chars -= used_chars;
        offset += used_chars;

        /* writing prefix size at buffer head */
        snprintf(prefix, sizeof(prefix), "%*.*d",
                 LIXA_MSG_XML_PREFIX_DIGITS,
                 LIXA_MSG_XML_PREFIX_DIGITS,
                 (int)(offset - LIXA_MSG_XML_PREFIX_DIGITS));
        strncpy(buffer, prefix, LIXA_MSG_XML_PREFIX_DIGITS);
        
        *msg_len = offset;
        
        LIXA_TRACE(("lixa_msg_serialize: serialized message is |%*.*s|\n",
                    *msg_len, *msg_len, buffer));
        
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
            case SERIALIZE_CLOSE_8_ERROR:
            case SERIALIZE_START_8_ERROR:
            case SERIALIZE_START_16_ERROR:
            case SERIALIZE_START_24_ERROR:
            case SERIALIZE_END_8_ERROR:
            case SERIALIZE_END_16_ERROR:
            case SERIALIZE_END_24_ERROR:
            case SERIALIZE_PREPARE_8_ERROR:
            case SERIALIZE_PREPARE_16_ERROR:
            case SERIALIZE_COMMIT_8_ERROR:
            case SERIALIZE_ROLLBACK_8_ERROR:
                break;
            case INVALID_OPEN_STEP:
            case INVALID_CLOSE_STEP:
            case INVALID_START_STEP:
            case INVALID_END_STEP:
            case INVALID_PREPARE_STEP:
            case INVALID_COMMIT_STEP:
            case INVALID_ROLLBACK_STEP:
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



int lixa_msg_serialize_close_8(const struct lixa_msg_s *msg, char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_close_8\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgr> */
        for (i=0; i<msg->body.close_8.rsrmgrs->len; ++i) {
            struct lixa_msg_body_close_8_rsrmgr_s *rsrmgr;
            rsrmgr = &g_array_index(
                msg->body.close_8.rsrmgrs,
                struct lixa_msg_body_close_8_rsrmgr_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT2);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
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
    LIXA_TRACE(("lixa_msg_serialize_close_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_commit_8(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_commit_8\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_FINISHED,
                              msg->body.commit_8.conthr.finished);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_commit_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars, "<%s>",
                              LIXA_XML_MSG_TAG_XA_COMMIT_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_commit_exec> */
        for (i=0; i<msg->body.commit_8.xa_commit_execs->len; ++i) {
            struct lixa_msg_body_commit_8_xa_commit_execs_s *xa_commit_exec;
            xa_commit_exec = &g_array_index(
                msg->body.commit_8.xa_commit_execs,
                struct lixa_msg_body_commit_8_xa_commit_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_COMMIT_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_commit_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_commit_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_commit_exec->rc,
                                  LIXA_XML_MSG_PROP_R_STATE,
                                  xa_commit_exec->r_state,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  xa_commit_exec->s_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_commit_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_COMMIT_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_commit_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_end_8(const struct lixa_msg_s *msg,
                               char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_end_8\n"));
    TRY {
        int used_chars;
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_COMMIT,
                              msg->body.end_8.conthr.commit ? 1 : 0);
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
    LIXA_TRACE(("lixa_msg_serialize_end_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_end_16(const struct lixa_msg_s *msg,
                              char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_end_16\n"));
    TRY {
        int used_chars;
        
        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.end_16.answer.rc);
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
    LIXA_TRACE(("lixa_msg_serialize_end_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_end_24(const struct lixa_msg_s *msg,
                              char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_end_24\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <xa_end_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_XA_END_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_end_exec> */
        for (i=0; i<msg->body.end_24.xa_end_execs->len; ++i) {
            struct lixa_msg_body_end_24_xa_end_execs_s *xa_end_exec;
            xa_end_exec = &g_array_index(
                msg->body.end_24.xa_end_execs,
                struct lixa_msg_body_end_24_xa_end_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_END_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_end_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_end_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_end_exec->rc,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  xa_end_exec->s_state,
                                  LIXA_XML_MSG_PROP_T_STATE,
                                  xa_end_exec->t_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT2);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_end_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_END_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_end_24/excp=%d/"
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
                              "<%s %s=\"%s\" %s=\"%s\"/>",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_PROFILE,
                              msg->body.open_8.client.profile,
                              LIXA_XML_MSG_PROP_CONF_DIGEST,
                              msg->body.open_8.client.lixac_conf_digest);
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
                                  "<%s %s=\"%d\" %s=\"%s\" %s=\"%s\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid,
                                  LIXA_XML_MSG_PROP_NAME,
                                  rsrmgr->name,
                                  LIXA_XML_MSG_PROP_XA_NAME,
                                  rsrmgr->xa_name);
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



int lixa_msg_serialize_open_24(const struct lixa_msg_s *msg, char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_open_24\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_STATE,
                              msg->body.open_24.conthr.state);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_open_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_XA_OPEN_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_open_exec> */
        for (i=0; i<msg->body.open_24.xa_open_execs->len; ++i) {
            struct lixa_msg_body_open_24_xa_open_execs_s *xa_open_exec;
            xa_open_exec = &g_array_index(
                msg->body.open_24.xa_open_execs,
                struct lixa_msg_body_open_24_xa_open_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%s\" %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_OPEN_EXEC,
                                  LIXA_XML_MSG_PROP_XA_INFO,
                                  (char *)xa_open_exec->xa_info,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_open_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_open_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_open_exec->rc,
                                  LIXA_XML_MSG_PROP_R_STATE,
                                  xa_open_exec->r_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_open_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_OPEN_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_open_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_prepare_8(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_prepare_8\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_COMMIT,
                              msg->body.prepare_8.conthr.commit);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_prepare_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars, "<%s>",
                              LIXA_XML_MSG_TAG_XA_PREPARE_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_prepare_exec> */
        for (i=0; i<msg->body.prepare_8.xa_prepare_execs->len; ++i) {
            struct lixa_msg_body_prepare_8_xa_prepare_execs_s *xa_prepare_exec;
            xa_prepare_exec = &g_array_index(
                msg->body.prepare_8.xa_prepare_execs,
                struct lixa_msg_body_prepare_8_xa_prepare_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_PREPARE_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_prepare_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_prepare_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_prepare_exec->rc,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  xa_prepare_exec->s_state,
                                  LIXA_XML_MSG_PROP_T_STATE,
                                  xa_prepare_exec->t_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_prepare_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_PREPARE_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_prepare_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_prepare_16(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_prepare_16\n"));
    TRY {
        int used_chars;
        
        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.prepare_16.answer.rc);
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
    LIXA_TRACE(("lixa_msg_serialize_prepare_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_rollback_8(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_rollback_8\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_FINISHED,
                              msg->body.rollback_8.conthr.finished);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_rollback_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars, "<%s>",
                              LIXA_XML_MSG_TAG_XA_ROLLBACK_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_rollback_exec> */
        for (i=0; i<msg->body.rollback_8.xa_rollback_execs->len; ++i) {
            struct lixa_msg_body_rollback_8_xa_rollback_execs_s
                *xa_rollback_exec;
            xa_rollback_exec = &g_array_index(
                msg->body.rollback_8.xa_rollback_execs,
                struct lixa_msg_body_rollback_8_xa_rollback_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_ROLLBACK_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_rollback_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_rollback_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_rollback_exec->rc,
                                  LIXA_XML_MSG_PROP_R_STATE,
                                  xa_rollback_exec->r_state,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  xa_rollback_exec->s_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_rollback_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_ROLLBACK_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_rollback_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_start_8(const struct lixa_msg_s *msg,
                               char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { XID_SERIALIZE_ERROR
                     , BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    char *ser_xid = NULL;
    
    LIXA_TRACE(("lixa_msg_serialize_start_8\n"));
    TRY {
        int used_chars;
        guint i;

        if (NULL == (ser_xid = xid_serialize(&msg->body.start_8.conthr.xid)))
            THROW(XID_SERIALIZE_ERROR);
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\"/>",
                              LIXA_XML_MSG_TAG_CONTHR, LIXA_XML_MSG_PROP_XID,
                              ser_xid);
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
        for (i=0; i<msg->body.start_8.rsrmgrs->len; ++i) {
            struct lixa_msg_body_close_8_rsrmgr_s *rsrmgr;
            rsrmgr = &g_array_index(
                msg->body.start_8.rsrmgrs,
                struct lixa_msg_body_close_8_rsrmgr_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid);
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
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
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

        /* memory recovery */
        if (NULL != ser_xid)
            free(ser_xid);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_start_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_start_16(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_start_16\n"));
    TRY {
        int used_chars;
        
        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.start_16.answer.rc);
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
    LIXA_TRACE(("lixa_msg_serialize_start_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_serialize_start_24(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_serialize_start_24\n"));
    TRY {
        int used_chars;
        guint i;
        
        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_STATE,
                              msg->body.start_24.conthr.state);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_start_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_XA_START_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_start_exec> */
        for (i=0; i<msg->body.start_24.xa_start_execs->len; ++i) {
            struct lixa_msg_body_start_24_xa_start_execs_s *xa_start_exec;
            xa_start_exec = &g_array_index(
                msg->body.start_24.xa_start_execs,
                struct lixa_msg_body_start_24_xa_start_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_START_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_start_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_start_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_start_exec->rc,
                                  LIXA_XML_MSG_PROP_T_STATE,
                                  xa_start_exec->t_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_start_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_START_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_start_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



