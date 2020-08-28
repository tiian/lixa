/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_xml_msg_serialize.h"
#include "lixa_xid.h"
#include "lixa_xml_msg.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



int lixa_msg_serialize(const struct lixa_msg_s *msg,
                       char **output, size_t *output_len)
{
    enum Exception {
        BUFFER_TOO_SHORT1,
        BUFFER_TOO_SHORT2,
        SERIALIZE_OPEN_8_ERROR,
        SERIALIZE_OPEN_16_ERROR,
        SERIALIZE_OPEN_24_ERROR,
        LAST_STEP_EXECEEDED_OPEN,
        INVALID_OPEN_STEP,
        SERIALIZE_CLOSE_8_ERROR,
        LAST_STEP_EXECEEDED_CLOSE,
        INVALID_CLOSE_STEP,
        SERIALIZE_START_8_ERROR,
        SERIALIZE_START_16_ERROR,
        SERIALIZE_START_24_ERROR,
        LAST_STEP_EXECEEDED_START,
        INVALID_START_STEP,
        SERIALIZE_END_8_ERROR,
        SERIALIZE_END_16_ERROR,
        LAST_STEP_EXECEEDED_END,
        INVALID_END_STEP,
        SERIALIZE_PREPARE_8_ERROR,
        SERIALIZE_PREPARE_16_ERROR,
        SERIALIZE_PREPARE_24_ERROR,
        SERIALIZE_PREPARE_32_ERROR,
        LAST_STEP_EXECEEDED_PREPARE,
        INVALID_PREPARE_STEP,
        SERIALIZE_COMMIT_8_ERROR,
        LAST_STEP_EXECEEDED_COMMIT,
        INVALID_COMMIT_STEP,
        SERIALIZE_ROLLBACK_8_ERROR,
        LAST_STEP_EXECEEDED_ROLLBACK,
        INVALID_ROLLBACK_STEP,
        SERIALIZE_QRCVR_8_ERROR,
        SERIALIZE_QRCVR_16_ERROR,
        SERIALIZE_QRCVR_24_ERROR,
        LAST_STEP_EXECEEDED_QRCVR,
        INVALID_QRCVR_STEP,
        SERIALIZE_REG_8_ERROR,
        INVALID_REG_STEP,
        LAST_STEP_EXECEEDED_REG,
        SERIALIZE_UNREG_8_ERROR,
        INVALID_UNREG_STEP,
        LAST_STEP_EXECEEDED_UNREG,
        SERIALIZE_FORGET_8_ERROR,
        LAST_STEP_EXECEEDED_FORGET,
        INVALID_FORGET_STEP,
        SERIALIZE_TRANS_8_ERROR,
        SERIALIZE_TRANS_16_ERROR,
        LAST_STEP_EXECEEDED_TRANS,
        INVALID_TRANS_STEP,
        INVALID_VERB,
        BUFFER_TOO_SHORT3,
        STRDUP_ERROR, 
        NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int used_chars = 0;
    char prefix[LIXA_MSG_XML_PREFIX_DIGITS + 1];
    char buffer[LIXA_MSG_XML_BUFFER_SIZE];
    size_t free_chars = sizeof(buffer), offset = 0;

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
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_open_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_OPEN_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_open_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_OPEN_16_ERROR);
                        break;
                    case 3*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_open_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_OPEN_24_ERROR);
                        break;
                    case 4*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_OPEN);
                        break;
                    default:
                        THROW(INVALID_OPEN_STEP);
                }
                break;
            case LIXA_MSG_VERB_CLOSE:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_close_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_CLOSE_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_CLOSE);
                        break;
                    default:
                        THROW(INVALID_CLOSE_STEP);
                }
                break;
            case LIXA_MSG_VERB_START:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_start_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_START_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_start_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_START_16_ERROR);
                        break;
                    case 3*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_start_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_START_24_ERROR);
                        break;
                    case 4*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_START);
                        break;
                    default:
                        THROW(INVALID_START_STEP);
                }
                break;
            case LIXA_MSG_VERB_END:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_end_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_END_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_end_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_END_16_ERROR);
                        break;
                    case 3*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_END);
                        break;
                    default:
                        THROW(INVALID_END_STEP);
                }
                break;
            case LIXA_MSG_VERB_PREPARE:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_prepare_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_PREPARE_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_prepare_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_PREPARE_16_ERROR);
                        break;
                    case 3*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_prepare_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_PREPARE_24_ERROR);
                        break;
                    case 4*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_prepare_32(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_PREPARE_32_ERROR);
                        break;
                    case 5*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_PREPARE);
                        break;
                    default:
                        THROW(INVALID_PREPARE_STEP);
                }
                break;
            case LIXA_MSG_VERB_COMMIT:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_commit_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_COMMIT_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_COMMIT);
                        break;
                    default:
                        THROW(INVALID_COMMIT_STEP);
                }
                break;
            case LIXA_MSG_VERB_ROLLBACK:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_rollback_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_ROLLBACK_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_ROLLBACK);
                        break;
                    default:
                        THROW(INVALID_ROLLBACK_STEP);
                }
                break;
            case LIXA_MSG_VERB_QRCVR:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_qrcvr_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_QRCVR_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_qrcvr_16(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_QRCVR_16_ERROR);
                        break;
                    case 3*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_qrcvr_24(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_QRCVR_24_ERROR);
                        break;
                    case 4*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_QRCVR);
                        break;
                    default:
                        THROW(INVALID_QRCVR_STEP);
                }
                break;
            case LIXA_MSG_VERB_REG:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_reg_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_REG_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_REG);
                        break;
                    default: THROW(INVALID_REG_STEP);
                }
                break;
            case LIXA_MSG_VERB_UNREG:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_unreg_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_UNREG_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_UNREG);
                        break;
                    default:
                        THROW(INVALID_UNREG_STEP);
                }
                break;
            case LIXA_MSG_VERB_FORGET:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK != (
                                ret_cod =
                                lixa_msg_serialize_forget_8(
                                    msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_FORGET_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_FORGET);
                        break;
                    default:
                        THROW(INVALID_FORGET_STEP);
                }
                break;
            case LIXA_MSG_VERB_TRANS:
                switch (msg->header.pvs.step) {
                    case 1*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK !=
                            (ret_cod = lixa_msg_serialize_trans_8(
                                msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_TRANS_8_ERROR);
                        break;
                    case 2*LIXA_MSG_STEP_INCR:
                        if (LIXA_RC_OK !=
                            (ret_cod = lixa_msg_serialize_trans_16(
                                msg, buffer, &offset, &free_chars)))
                            THROW(SERIALIZE_TRANS_16_ERROR);
                        break;
                    case 3*LIXA_MSG_STEP_INCR:
                        THROW(LAST_STEP_EXECEEDED_TRANS);
                        break;
                    default:
                        THROW(INVALID_TRANS_STEP);
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
                 (int) (offset - LIXA_MSG_XML_PREFIX_DIGITS));
        strncpy(buffer, prefix, LIXA_MSG_XML_PREFIX_DIGITS);

        *output_len = offset;
        LIXA_TRACE(("lixa_msg_serialize: serialized message is |%*.*s|\n",
                    *output_len, *output_len, buffer));

        /* create a dynamic copy for the caller */
        if (NULL != *output) {
            LIXA_TRACE(("lixa_msg_serialize: WARNING, output is not NULL, "
                        "trying to free it before a new allocation...\n"));
            free(*output);
            *output = NULL;
        }
        if (NULL == (*output = strndup(buffer, sizeof(buffer))))
            THROW(STRDUP_ERROR);
        
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
            case SERIALIZE_PREPARE_8_ERROR:
            case SERIALIZE_PREPARE_16_ERROR:
            case SERIALIZE_PREPARE_24_ERROR:
            case SERIALIZE_PREPARE_32_ERROR:
            case SERIALIZE_COMMIT_8_ERROR:
            case SERIALIZE_ROLLBACK_8_ERROR:
            case SERIALIZE_QRCVR_8_ERROR:
            case SERIALIZE_QRCVR_16_ERROR:
            case SERIALIZE_QRCVR_24_ERROR:
            case SERIALIZE_REG_8_ERROR:
            case SERIALIZE_UNREG_8_ERROR:
            case SERIALIZE_FORGET_8_ERROR:
            case SERIALIZE_TRANS_8_ERROR:
            case SERIALIZE_TRANS_16_ERROR:
                break;
            case LAST_STEP_EXECEEDED_OPEN:
            case LAST_STEP_EXECEEDED_CLOSE:
            case LAST_STEP_EXECEEDED_START:
            case LAST_STEP_EXECEEDED_END:
            case LAST_STEP_EXECEEDED_PREPARE:
            case LAST_STEP_EXECEEDED_COMMIT:
            case LAST_STEP_EXECEEDED_ROLLBACK:
            case LAST_STEP_EXECEEDED_QRCVR:
            case LAST_STEP_EXECEEDED_REG:
            case LAST_STEP_EXECEEDED_UNREG:
            case LAST_STEP_EXECEEDED_FORGET:
            case LAST_STEP_EXECEEDED_TRANS:
                ret_cod = LIXA_RC_LAST_STEP_EXCEEDED;
                break;
            case INVALID_OPEN_STEP:
            case INVALID_CLOSE_STEP:
            case INVALID_START_STEP:
            case INVALID_END_STEP:
            case INVALID_PREPARE_STEP:
            case INVALID_COMMIT_STEP:
            case INVALID_ROLLBACK_STEP:
            case INVALID_QRCVR_STEP:
            case INVALID_REG_STEP:
            case INVALID_UNREG_STEP:
            case INVALID_FORGET_STEP:
            case INVALID_TRANS_STEP:
            case INVALID_VERB:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case BUFFER_TOO_SHORT3:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
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
    LIXA_TRACE_STACK();
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
        for (i = 0; i < msg->body.close_8.rsrmgrs->len; ++i) {
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_commit_8(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
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
        for (i = 0; i < msg->body.commit_8.xa_commit_execs->len; ++i) {
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_end_8(const struct lixa_msg_s *msg,
                             char *buffer,
                             size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT,
                     BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_end_8\n"));
    TRY {
        int used_chars;
        guint i;

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

        /* <xa_end_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_XA_END_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_end_exec> */
        for (i = 0; i < msg->body.end_8.xa_end_execs->len; ++i) {
            struct lixa_msg_body_end_8_xa_end_execs_s *xa_end_exec;
            xa_end_exec = &g_array_index(
                msg->body.end_8.xa_end_execs,
                struct lixa_msg_body_end_8_xa_end_execs_s, i);
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
                                  LIXA_XML_MSG_PROP_TD_STATE,
                                  xa_end_exec->td_state);
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
            case BUFFER_TOO_SHORT:
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
    LIXA_TRACE(("lixa_msg_serialize_end_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_forget_8(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_forget_8\n"));
    TRY {
        int used_chars;
        guint i;

        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_FINISHED,
                              msg->body.forget_8.conthr.finished);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_forget_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars, "<%s>",
                              LIXA_XML_MSG_TAG_XA_FORGET_EXECS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <xa_forget_exec> */
        for (i = 0; i < msg->body.forget_8.xa_forget_execs->len; ++i) {
            struct lixa_msg_body_forget_8_xa_forget_execs_s *xa_forget_exec;
            xa_forget_exec = &g_array_index(
                msg->body.forget_8.xa_forget_execs,
                struct lixa_msg_body_forget_8_xa_forget_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_FORGET_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_forget_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_forget_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_forget_exec->rc,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  xa_forget_exec->s_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT3);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </xa_forget_execs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_XA_FORGET_EXECS);
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
    LIXA_TRACE(("lixa_msg_serialize_forget_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_open_8(const struct lixa_msg_s *msg, char *buffer,
                              size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1
                     , BUFFER_TOO_SHORT2
                     , G_BASE64_ENCODE_ERROR1
                     , G_BASE64_ENCODE_ERROR2
                     , BUFFER_TOO_SHORT3
                     , BUFFER_TOO_SHORT4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    gchar *name_base64 = NULL;
    gchar *xa_name_base64 = NULL;
    
    LIXA_TRACE(("lixa_msg_serialize_open_8\n"));
    TRY {
        int used_chars;
        guint i;

        /* <client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\" %s=\"%s\" %s=\"%s\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_JOB,
                              msg->body.open_8.client.job,
                              LIXA_XML_MSG_PROP_CONFIG_DIGEST,
                              msg->body.open_8.client.config_digest,
                              LIXA_XML_MSG_PROP_SESSID,
                              lixa_session_get_sid(
                                  &(msg->body.open_8.client.session)),
                              LIXA_XML_MSG_PROP_MAINT,
                              msg->body.open_8.client.maint);
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
        for (i = 0; i < msg->body.open_8.rsrmgrs->len; ++i) {
            struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr;
            rsrmgr = &g_array_index(
                msg->body.open_8.rsrmgrs,
                struct lixa_msg_body_open_8_rsrmgr_s, i);
            if (NULL == (name_base64 = g_base64_encode(
                             rsrmgr->name,
                             strlen((const char *)rsrmgr->name))))
                THROW(G_BASE64_ENCODE_ERROR1);
            if (NULL == (xa_name_base64 = g_base64_encode(
                             rsrmgr->xa_name,
                             strlen((const char *)rsrmgr->xa_name))))
                THROW(G_BASE64_ENCODE_ERROR2);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"%d\" %s=\"%s\" "
                                  "%s=\"%s\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid,
                                  LIXA_XML_MSG_PROP_DYNAMIC,
                                  rsrmgr->dynamic,
                                  LIXA_XML_MSG_PROP_NAME,
                                  name_base64,
                                  LIXA_XML_MSG_PROP_XA_NAME,
                                  xa_name_base64);
            g_free(name_base64);
            name_base64 = NULL;
            g_free(xa_name_base64);
            xa_name_base64 = NULL;
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
            case G_BASE64_ENCODE_ERROR1:
            case G_BASE64_ENCODE_ERROR2:
                ret_cod = LIXA_RC_G_BASE64_ENCODE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover memory if necessary */
        if (NULL != name_base64) {
            g_free(name_base64);
            name_base64 = NULL;
        }
        if (NULL != xa_name_base64) {
            g_free(xa_name_base64);
            xa_name_base64 = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_open_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_open_24(const struct lixa_msg_s *msg, char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     G_BASE64_ENCODE_ERROR,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    gchar *xa_info_base64 = NULL;
    
    LIXA_TRACE(("lixa_msg_serialize_open_24\n"));
    TRY {
        int used_chars;
        guint i;

        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_TXSTATE,
                              msg->body.open_24.conthr.txstate);
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
        for (i = 0; i < msg->body.open_24.xa_open_execs->len; ++i) {
            struct lixa_msg_body_open_24_xa_open_execs_s *xa_open_exec;
            xa_open_exec = &g_array_index(
                msg->body.open_24.xa_open_execs,
                struct lixa_msg_body_open_24_xa_open_execs_s, i);
            if (NULL == (xa_info_base64 = g_base64_encode(
                             xa_open_exec->xa_info,
                             strlen((const char *)xa_open_exec->xa_info))))
                THROW(G_BASE64_ENCODE_ERROR);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%s\" %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_OPEN_EXEC,
                                  LIXA_XML_MSG_PROP_XA_INFO,
                                  xa_info_base64,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_open_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_open_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_open_exec->rc,
                                  LIXA_XML_MSG_PROP_R_STATE,
                                  xa_open_exec->r_state);
            g_free(xa_info_base64);
            xa_info_base64 = NULL;
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
            case G_BASE64_ENCODE_ERROR:
                ret_cod = LIXA_RC_G_BASE64_ENCODE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (NULL != xa_info_base64) {
            g_free(xa_info_base64);
            xa_info_base64 = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_open_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_prepare_8(const struct lixa_msg_s *msg,
                                 char *buffer,
                                 size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_prepare_8\n"));
    TRY {
        int used_chars;
        guint i;

        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_COMMIT,
                              msg->body.prepare_8.conthr.commit,
                              LIXA_XML_MSG_PROP_NON_BLOCK,
                              msg->body.prepare_8.conthr.non_block);
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
        for (i = 0; i < msg->body.prepare_8.xa_prepare_execs->len; ++i) {
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
                                  LIXA_XML_MSG_PROP_TD_STATE,
                                  xa_prepare_exec->td_state);
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
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_prepare_24(const struct lixa_msg_s *msg,
                                 char *buffer,
                                 size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_prepare_24\n"));
    TRY {
        int used_chars=0;

        /* nothing to do */
        *free_chars -= used_chars;
        *offset += used_chars;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_TOO_SHORT1:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_prepare_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_prepare_32(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_prepare_32\n"));
    TRY {
        int used_chars;

        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.prepare_32.answer.rc);
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
    LIXA_TRACE(("lixa_msg_serialize_prepare_32/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_qrcvr_8(const struct lixa_msg_s *msg,
                               char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_qrcvr_8\n"));
    TRY {
        int used_chars;

        /* <client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\" %s=\"%s\"/>",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_JOB,
                              msg->body.qrcvr_8.client.job,
                              LIXA_XML_MSG_PROP_CONFIG_DIGEST,
                              msg->body.qrcvr_8.client.config_digest);
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
    LIXA_TRACE(("lixa_msg_serialize_qrcvr_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_qrcvr_16(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { XID_SERIALIZE_ERROR,
                     BUFFER_TOO_SHORT1,
                     BYPASS_CLIENT_RSRMGRS,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     BUFFER_TOO_SHORT5,
                     BUFFER_TOO_SHORT6,
                     BUFFER_TOO_SHORT7,
                     BUFFER_TOO_SHORT8,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    lixa_ser_xid_t ser_xid;

    LIXA_TRACE(("lixa_msg_serialize_qrcvr_16\n"));
    TRY {
        int used_chars;
        guint i;

        if (!lixa_xid_serialize(&msg->body.qrcvr_16.client.state.xid,
                                ser_xid))
            THROW(XID_SERIALIZE_ERROR);

        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.qrcvr_16.answer.rc);
        if (used_chars >= *free_chars) THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        if (LIXA_RC_OBJ_NOT_FOUND == msg->body.qrcvr_16.answer.rc)
            THROW(BYPASS_CLIENT_RSRMGRS);
        /* <client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\" %s=\"%s\">",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_JOB,
                              msg->body.qrcvr_16.client.job,
                              LIXA_XML_MSG_PROP_CONFIG_DIGEST,
                              msg->body.qrcvr_16.client.config_digest);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <last_verb_step/> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_LAST_VERB_STEP,
                              LIXA_XML_MSG_PROP_VERB,
                              msg->body.qrcvr_16.client.last_verb_step.verb,
                              LIXA_XML_MSG_PROP_STEP,
                              msg->body.qrcvr_16.client.last_verb_step.step);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT3);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <state/> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"%d\" %s=\"%d\" %s=\"%d\" "
                              "%s=\"%d\" %s=\"%s\"/>",
                              LIXA_XML_MSG_TAG_STATE,
                              LIXA_XML_MSG_PROP_FINISHED,
                              msg->body.qrcvr_16.client.state.finished,
                              LIXA_XML_MSG_PROP_TXSTATE,
                              msg->body.qrcvr_16.client.state.txstate,
                              LIXA_XML_MSG_PROP_WILL_COMMIT,
                              msg->body.qrcvr_16.client.state.will_commit,
                              LIXA_XML_MSG_PROP_WILL_ROLLBACK,
                              msg->body.qrcvr_16.client.state.will_rollback,
                              LIXA_XML_MSG_PROP_GLOBAL_RECOVERY,
                              msg->body.qrcvr_16.client.state.global_recovery,
                              LIXA_XML_MSG_PROP_XID,
                              ser_xid);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT4);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* </client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_CLIENT);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT5);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT6);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgr/> */
        for (i = 0; i < msg->body.qrcvr_16.rsrmgrs->len; ++i) {
            struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr;
            rsrmgr = &g_array_index(
                msg->body.qrcvr_16.rsrmgrs,
                struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"%d\" %s=\"%d\" "
                                  "%s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid,
                                  LIXA_XML_MSG_PROP_NEXT_VERB,
                                  rsrmgr->next_verb,
                                  LIXA_XML_MSG_PROP_R_STATE,
                                  rsrmgr->r_state,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  rsrmgr->s_state,
                                  LIXA_XML_MSG_PROP_TD_STATE,
                                  rsrmgr->td_state);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT7);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT8);
        *free_chars -= used_chars;
        *offset += used_chars;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case BUFFER_TOO_SHORT1:
                break;
            case BYPASS_CLIENT_RSRMGRS:
                ret_cod = LIXA_RC_OK;
                break;
            case BUFFER_TOO_SHORT2:
            case BUFFER_TOO_SHORT3:
            case BUFFER_TOO_SHORT4:
            case BUFFER_TOO_SHORT5:
            case BUFFER_TOO_SHORT6:
            case BUFFER_TOO_SHORT7:
            case BUFFER_TOO_SHORT8:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_qrcvr_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_qrcvr_24(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_qrcvr_24\n"));
    TRY {
        int used_chars;
        guint i;

        /* <recovery/> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_RECOVERY,
                              LIXA_XML_MSG_PROP_FAILED,
                              msg->body.qrcvr_24.recovery.failed,
                              LIXA_XML_MSG_PROP_COMMIT,
                              msg->body.qrcvr_24.recovery.commit);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars) THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgr/> */
        for (i = 0; i < msg->body.qrcvr_24.rsrmgrs->len; ++i) {
            struct lixa_msg_body_qrcvr_24_rsrmgr_s *rsrmgr;
            rsrmgr = &g_array_index(
                msg->body.qrcvr_24.rsrmgrs,
                struct lixa_msg_body_qrcvr_24_rsrmgr_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_RSRMGR,
                                  LIXA_XML_MSG_PROP_RMID,
                                  rsrmgr->rmid,
                                  LIXA_XML_MSG_PROP_RC,
                                  rsrmgr->rc);
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
    LIXA_TRACE(("lixa_msg_serialize_qrcvr_24/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_reg_8(const struct lixa_msg_s *msg,
                             char *buffer,
                             size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_reg_8\n"));
    TRY {
        int used_chars;

        /* <ax_reg_exec> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"0x%lx\" "
                              "%s=\"%d\" %s=\"%d\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_AX_REG_EXEC,
                              LIXA_XML_MSG_PROP_RMID,
                              msg->body.reg_8.ax_reg_exec.rmid,
                              LIXA_XML_MSG_PROP_FLAGS,
                              msg->body.reg_8.ax_reg_exec.flags,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.reg_8.ax_reg_exec.rc,
                              LIXA_XML_MSG_PROP_TD_STATE,
                              msg->body.reg_8.ax_reg_exec.td_state,
                              LIXA_XML_MSG_PROP_S_STATE,
                              msg->body.reg_8.ax_reg_exec.s_state);
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
    LIXA_TRACE(("lixa_msg_serialize_reg_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_rollback_8(const struct lixa_msg_s *msg,
                                  char *buffer,
                                  size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
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
        for (i = 0; i < msg->body.rollback_8.xa_rollback_execs->len; ++i) {
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_start_8(const struct lixa_msg_s *msg,
                               char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { XID_SERIALIZE_ERROR,
                     BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    lixa_ser_xid_t ser_xid;

    LIXA_TRACE(("lixa_msg_serialize_start_8\n"));
    TRY {
        int used_chars;
        guint i;

        if (!lixa_xid_serialize(&msg->body.start_8.conthr.xid, ser_xid))
            THROW(XID_SERIALIZE_ERROR);

        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_XID, ser_xid,
                              LIXA_XML_MSG_PROP_SUB_BRANCH,
                              msg->body.start_8.conthr.sub_branch);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgrs> */
        used_chars = snprintf(buffer + *offset, *free_chars, "<%s>",
                              LIXA_XML_MSG_TAG_RSRMGRS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <rsrmgr> */
        for (i = 0; i < msg->body.start_8.rsrmgrs->len; ++i) {
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
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_start_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_start_24(const struct lixa_msg_s *msg,
                                char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_start_24\n"));
    TRY {
        int used_chars;
        guint i;

        /* <conthr> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CONTHR,
                              LIXA_XML_MSG_PROP_TXSTATE,
                              msg->body.start_24.conthr.txstate);
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
        for (i = 0; i < msg->body.start_24.xa_start_execs->len; ++i) {
            struct lixa_msg_body_start_24_xa_start_execs_s *xa_start_exec;
            xa_start_exec = &g_array_index(
                msg->body.start_24.xa_start_execs,
                struct lixa_msg_body_start_24_xa_start_execs_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%d\" %s=\"0x%lx\" "
                                  "%s=\"%d\" %s=\"%d\" %s=\"%d\"/>",
                                  LIXA_XML_MSG_TAG_XA_START_EXEC,
                                  LIXA_XML_MSG_PROP_RMID,
                                  xa_start_exec->rmid,
                                  LIXA_XML_MSG_PROP_FLAGS,
                                  xa_start_exec->flags,
                                  LIXA_XML_MSG_PROP_RC,
                                  xa_start_exec->rc,
                                  LIXA_XML_MSG_PROP_TD_STATE,
                                  xa_start_exec->td_state,
                                  LIXA_XML_MSG_PROP_S_STATE,
                                  xa_start_exec->s_state);
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_unreg_8(const struct lixa_msg_s *msg,
                               char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_unreg_8\n"));
    TRY {
        int used_chars;

        /* <ax_unreg_exec> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"0x%lx\" "
                              "%s=\"%d\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_AX_UNREG_EXEC,
                              LIXA_XML_MSG_PROP_RMID,
                              msg->body.unreg_8.ax_unreg_exec.rmid,
                              LIXA_XML_MSG_PROP_FLAGS,
                              msg->body.unreg_8.ax_unreg_exec.flags,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.unreg_8.ax_unreg_exec.rc,
                              LIXA_XML_MSG_PROP_TD_STATE,
                              msg->body.unreg_8.ax_unreg_exec.td_state);
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
    LIXA_TRACE(("lixa_msg_serialize_unreg_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_trans_8(const struct lixa_msg_s *msg, char *buffer,
                               size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_trans_8\n"));
    TRY {
        int used_chars;

        /* <client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\" %s=\"%s\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_JOB,
                              msg->body.trans_8.client.job,
                              LIXA_XML_MSG_PROP_CONFIG_DIGEST,
                              msg->body.trans_8.client.config_digest,
                              LIXA_XML_MSG_PROP_MAINT,
                              msg->body.open_8.client.maint);
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
    LIXA_TRACE(("lixa_msg_serialize_trans_8/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_msg_serialize_trans_16(const struct lixa_msg_s *msg, char *buffer,
                                size_t *offset, size_t *free_chars)
{
    enum Exception { BUFFER_TOO_SHORT1,
                     BUFFER_TOO_SHORT2,
                     BUFFER_TOO_SHORT3,
                     BUFFER_TOO_SHORT4,
                     BUFFER_TOO_SHORT5,
                     BUFFER_TOO_SHORT6,
                     BUFFER_TOO_SHORT7,
                     BYPASS_TRANSACTIONS,
                     XID_SERIALIZE_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_serialize_trans_16\n"));
    TRY {
        int used_chars;
        guint i;

        /* <answer> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_ANSWER,
                              LIXA_XML_MSG_PROP_RC,
                              msg->body.trans_16.answer.rc);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT1);
        *free_chars -= used_chars;
        *offset += used_chars;
        if (LIXA_RC_OBJ_NOT_FOUND == msg->body.trans_16.answer.rc)
            THROW(BYPASS_TRANSACTIONS);
        /* <client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%s\" %s=\"%s\">",
                              LIXA_XML_MSG_TAG_CLIENT,
                              LIXA_XML_MSG_PROP_JOB,
                              msg->body.trans_16.client.job,
                              LIXA_XML_MSG_PROP_CONFIG_DIGEST,
                              msg->body.trans_16.client.config_digest);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT2);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <last_verb_step/> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s %s=\"%d\" %s=\"%d\"/>",
                              LIXA_XML_MSG_TAG_LAST_VERB_STEP,
                              LIXA_XML_MSG_PROP_VERB,
                              msg->body.trans_16.client.last_verb_step.verb,
                              LIXA_XML_MSG_PROP_STEP,
                              msg->body.trans_16.client.last_verb_step.step);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT3);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* </client> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_CLIENT);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT4);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <transactions> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "<%s>",
                              LIXA_XML_MSG_TAG_TRANS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT5);
        *free_chars -= used_chars;
        *offset += used_chars;
        /* <transaction> */
        for (i = 0; i < msg->body.trans_16.transactions->len; ++i) {
            struct lixa_msg_body_trans_16_transaction_s *trans;
            trans = &g_array_index(
                msg->body.trans_16.transactions,
                struct lixa_msg_body_trans_16_transaction_s, i);
            used_chars = snprintf(buffer + *offset, *free_chars,
                                  "<%s %s=\"%s\"/>",
                                  LIXA_XML_MSG_TAG_TRAN,
                                  LIXA_XML_MSG_PROP_XID,
                                  trans->xid);
            if (used_chars >= *free_chars)
                THROW(BUFFER_TOO_SHORT6);
            *free_chars -= used_chars;
            *offset += used_chars;
        }
        /* </transactions> */
        used_chars = snprintf(buffer + *offset, *free_chars,
                              "</%s>",
                              LIXA_XML_MSG_TAG_TRANS);
        if (used_chars >= *free_chars)
            THROW(BUFFER_TOO_SHORT7);
        *free_chars -= used_chars;
        *offset += used_chars;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case BUFFER_TOO_SHORT1:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case BYPASS_TRANSACTIONS:
                ret_cod = LIXA_RC_OK;
                break;
            case BUFFER_TOO_SHORT2:
            case BUFFER_TOO_SHORT3:
            case BUFFER_TOO_SHORT4:
            case BUFFER_TOO_SHORT5:
            case BUFFER_TOO_SHORT6:
            case BUFFER_TOO_SHORT7:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_serialize_trans_16/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
