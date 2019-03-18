/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_xml_msg.h"
#include "lixa_common_status.h"
#include "lixa_syslog.h"
#include "lixa_xml_msg.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG


const xmlChar *LIXA_XML_MSG_HEADER = (xmlChar *) "<?xml";
const xmlChar *LIXA_XML_MSG_PROP_COMMIT = (xmlChar *) "commit";
const xmlChar *LIXA_XML_MSG_PROP_CONFIG_DIGEST = (xmlChar *) "config_digest";
const xmlChar *LIXA_XML_MSG_PROP_DYNAMIC = (xmlChar *) "dynamic";
const xmlChar *LIXA_XML_MSG_PROP_FAILED = (xmlChar *) "failed";
const xmlChar *LIXA_XML_MSG_PROP_FINISHED = (xmlChar *) "finished";
const xmlChar *LIXA_XML_MSG_PROP_FLAGS = (xmlChar *) "flags";
const xmlChar *LIXA_XML_MSG_PROP_GLOBAL_RECOVERY = (xmlChar *) "global_recovery";
const xmlChar *LIXA_XML_MSG_PROP_JOB = (xmlChar *) "job";
const xmlChar *LIXA_XML_MSG_PROP_LEVEL = (xmlChar *) "level";
const xmlChar *LIXA_XML_MSG_PROP_MAINT = (xmlChar *) "maint";
const xmlChar *LIXA_XML_MSG_PROP_NAME = (xmlChar *) "name";
const xmlChar *LIXA_XML_MSG_PROP_NEXT_VERB = (xmlChar *) "next_verb";
const xmlChar *LIXA_XML_MSG_PROP_NON_BLOCK = (xmlChar *) "non_block";
const xmlChar *LIXA_XML_MSG_PROP_RC = (xmlChar *) "rc";
const xmlChar *LIXA_XML_MSG_PROP_RMID = (xmlChar *) "rmid";
const xmlChar *LIXA_XML_MSG_PROP_R_STATE = (xmlChar *) "r_state";
const xmlChar *LIXA_XML_MSG_PROP_SESSID = (xmlChar *) "sessid";
const xmlChar *LIXA_XML_MSG_PROP_S_STATE = (xmlChar *) "s_state";
const xmlChar *LIXA_XML_MSG_PROP_SUB_BRANCH = (xmlChar *) "sub_branch";
const xmlChar *LIXA_XML_MSG_PROP_TD_STATE = (xmlChar *) "td_state";
const xmlChar *LIXA_XML_MSG_PROP_TIMEOUT = (xmlChar *) "timeout";
const xmlChar *LIXA_XML_MSG_PROP_TXSTATE = (xmlChar *) "txstate";
const xmlChar *LIXA_XML_MSG_PROP_STEP = (xmlChar *) "step";
const xmlChar *LIXA_XML_MSG_PROP_VERB = (xmlChar *) "verb";
const xmlChar *LIXA_XML_MSG_PROP_WILL_COMMIT = (xmlChar *) "will_commit";
const xmlChar *LIXA_XML_MSG_PROP_WILL_ROLLBACK = (xmlChar *) "will_rollback";
const xmlChar *LIXA_XML_MSG_PROP_XA_INFO = (xmlChar *) "xa_info";
const xmlChar *LIXA_XML_MSG_PROP_XA_NAME = (xmlChar *) "xa_name";
const xmlChar *LIXA_XML_MSG_PROP_XID = (xmlChar *) "xid";
const xmlChar *LIXA_XML_MSG_TAG_ANSWER = (xmlChar *) "answer";
const xmlChar *LIXA_XML_MSG_TAG_AX_REG_EXEC = (xmlChar *) "ax_reg_exec";
const xmlChar *LIXA_XML_MSG_TAG_AX_UNREG_EXEC = (xmlChar *) "ax_unreg_exec";
const xmlChar *LIXA_XML_MSG_TAG_CLIENT = (xmlChar *) "client";
const xmlChar *LIXA_XML_MSG_TAG_CONTHR = (xmlChar *) "conthr";
const xmlChar *LIXA_XML_MSG_TAG_LAST_VERB_STEP = (xmlChar *) "last_verb_step";
const xmlChar *LIXA_XML_MSG_TAG_MSG = (xmlChar *) "msg";
const xmlChar *LIXA_XML_MSG_TAG_RECOVERY = (xmlChar *) "recovery";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGR = (xmlChar *) "rsrmgr";
const xmlChar *LIXA_XML_MSG_TAG_RSRMGRS = (xmlChar *) "rsrmgrs";
const xmlChar *LIXA_XML_MSG_TAG_STATE = (xmlChar *) "state";
const xmlChar *LIXA_XML_MSG_TAG_XA_COMMIT_EXEC = (xmlChar *) "xa_commit_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_COMMIT_EXECS = (xmlChar *) "xa_commit_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_END_EXEC = (xmlChar *) "xa_end_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_END_EXECS = (xmlChar *) "xa_end_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_FORGET_EXEC = (xmlChar *) "xa_forget_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_FORGET_EXECS = (xmlChar *) "xa_forget_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXEC = (xmlChar *) "xa_open_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXECS = (xmlChar *) "xa_open_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_PREPARE_EXEC = (xmlChar *) "xa_prepare_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_PREPARE_EXECS = (xmlChar *) "xa_prepare_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_ROLLBACK_EXEC = (xmlChar *) "xa_rollback_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_ROLLBACK_EXECS = (xmlChar *) "xa_rollback_execs";
const xmlChar *LIXA_XML_MSG_TAG_XA_START_EXEC = (xmlChar *) "xa_start_exec";
const xmlChar *LIXA_XML_MSG_TAG_XA_START_EXECS = (xmlChar *) "xa_start_execs";
const xmlChar *LIXA_XML_MSG_TAG_TRAN = (xmlChar *) "transaction";
const xmlChar *LIXA_XML_MSG_TAG_TRANS = (xmlChar *) "transactions";



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
        char prefix[LIXA_MSG_XML_PREFIX_DIGITS + 1];
        ssize_t to_read = 0;

        /* read the prefix to determine message size */
        if (0 > (*read_bytes = recv(
                     fd, prefix, LIXA_MSG_XML_PREFIX_DIGITS, 0))) {
            THROW(RECV_ERROR1);
        } else if (*read_bytes == 0) {
            THROW(CONNECTION_CLOSED);
        } else if (*read_bytes != LIXA_MSG_XML_PREFIX_DIGITS) {
            /* retrieve XML message size */
            LIXA_TRACE(("lixa_msg_retrieve: peer sent " SSIZE_T_FORMAT
                        " bytes, expected %d bytes for "
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

        LIXA_TRACE(("lixa_msg_retrieve: fd=%d returned " SSIZE_T_FORMAT
                    " bytes\n", fd, *read_bytes));
        if (to_read != *read_bytes) {
            LIXA_TRACE(("lixa_msg_retrieve: expected " SSIZE_T_FORMAT
                        " bytes, received " SSIZE_T_FORMAT
                        " bytes\n", to_read, *read_bytes));
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



int lixa_msg_send(int fd, const char *buf, size_t buf_size)
{
    enum Exception { GETSOCKOPT_ERROR
                     , CONNECTION_CLOSED
                     , SEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_send\n"));
    TRY {
        ssize_t wrote_bytes;
        int optval;
        socklen_t optlen = sizeof(optval);

        if (0 != getsockopt(fd, SOL_SOCKET, SO_ERROR, &optval, &optlen))
            THROW(GETSOCKOPT_ERROR);
        LIXA_TRACE(("lixa_msg_send: so_error=%d (EPIPE=%d, ECONNRESET=%d)\n",
                    optval, EPIPE, ECONNRESET));
        if (EPIPE == optval || ECONNRESET == optval) {
            int rc = 0;
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXC027N, getpid(),
                         pthread_self()));
            rc = shutdown(fd, SHUT_RDWR);
            LIXA_TRACE(("lixa_msg_send: socket with fd=%d was shutdown "
                        "(rc=%d,errno=%d)\n", fd, rc, errno));
            THROW(CONNECTION_CLOSED);
        }

        LIXA_TRACE(("lixa_msg_send: sending " SIZE_T_FORMAT
                    " bytes to the server (fd=%d)...\n", buf_size, fd));
        wrote_bytes = send(fd, buf, buf_size, 0);
        if (buf_size != wrote_bytes) {
            LIXA_TRACE(("lixa_msg_send: sent " SSIZE_T_FORMAT
                        " bytes instead of " SIZE_T_FORMAT
                        " to the server\n", wrote_bytes, buf_size));
            THROW(SEND_ERROR);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GETSOCKOPT_ERROR:
                ret_cod = LIXA_RC_GETSOCKOPT_ERROR;
                break;
            case CONNECTION_CLOSED:
                ret_cod = LIXA_RC_CONNECTION_CLOSED;
                break;
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_send/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_free(struct lixa_msg_s *msg)
{
    enum Exception {
        INVALID_STEP1,
        INVALID_STEP2,
        INVALID_STEP3,
        INVALID_STEP4,
        INVALID_STEP5,
        INVALID_STEP6,
        INVALID_STEP7,
        INVALID_STEP8,
        INVALID_STEP9,
        INVALID_STEP10,
        INVALID_STEP11,
        INVALID_STEP12,
        INVALID_VERB,
        NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_free\n"));
    TRY {
        guint i;
        switch (msg->header.pvs.verb) {
            case LIXA_MSG_VERB_NULL: /* nothing to release */
                break;
            case LIXA_MSG_VERB_OPEN: /* open */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.open_8.client.job) {
                            xmlFree(msg->body.open_8.client.job);
                            msg->body.open_8.client.job = NULL;
                        }
                        if (NULL != msg->body.open_8.rsrmgrs) {
                            for (i = 0;
                                 i < msg->body.open_8.rsrmgrs->len; ++i) {
                                struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr =
                                    &g_array_index(
                                        msg->body.open_8.rsrmgrs,
                                        struct lixa_msg_body_open_8_rsrmgr_s,
                                        i);
                                if (NULL != rsrmgr->name)
                                    g_free(rsrmgr->name);
                                if (NULL != rsrmgr->xa_name)
                                    g_free(rsrmgr->xa_name);
                            }
                            g_array_free(msg->body.open_8.rsrmgrs, TRUE);
                            msg->body.open_8.rsrmgrs = NULL;
                        }
                        break;
                    case 16: /* nothing to release */
                        break;
                    case 24:
                        if (NULL != msg->body.open_24.xa_open_execs) {
                            for (i = 0;
                                 i < msg->body.open_24.xa_open_execs->len;
                                 ++i) {
                                struct lixa_msg_body_open_24_xa_open_execs_s
                                    *xa_open_exec =
                                    &g_array_index(
                                        msg->body.open_24.xa_open_execs,
                                        struct
                                        lixa_msg_body_open_24_xa_open_execs_s,
                                        i);
                                if (NULL != xa_open_exec->xa_info)
                                    xmlFree(xa_open_exec->xa_info);
                            }
                            g_array_free(msg->body.open_24.xa_open_execs,
                                         TRUE);
                            msg->body.open_24.xa_open_execs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP1);
                }
                break;
            case LIXA_MSG_VERB_CLOSE: /* close */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.close_8.rsrmgrs) {
                            g_array_free(msg->body.close_8.rsrmgrs, TRUE);
                            msg->body.open_8.rsrmgrs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP2);
                }
                break;
            case LIXA_MSG_VERB_START: /* start */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.start_8.rsrmgrs) {
                            g_array_free(msg->body.start_8.rsrmgrs, TRUE);
                            msg->body.start_8.rsrmgrs = NULL;
                        }
                        break;
                    case 16: /* nothing to release */
                        break;
                    case 24:
                        if (NULL != msg->body.start_24.xa_start_execs) {
                            g_array_free(msg->body.start_24.xa_start_execs,
                                         TRUE);
                            msg->body.start_24.xa_start_execs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP3);
                }
                break;
            case LIXA_MSG_VERB_END: /* end */
                switch (msg->header.pvs.step) {
                    case 8: /* nothing to do */
                        if (NULL != msg->body.end_8.xa_end_execs) {
                            g_array_free(msg->body.end_8.xa_end_execs, TRUE);
                            msg->body.end_8.xa_end_execs = NULL;
                        }
                        break;
                    case 16: /* nothing to release */
                        break;
                    default:
                        THROW(INVALID_STEP4);
                }
                break;
            case LIXA_MSG_VERB_PREPARE: /* prepare */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.prepare_8.xa_prepare_execs) {
                            g_array_free(msg->body.prepare_8.xa_prepare_execs,
                                         TRUE);
                            msg->body.prepare_8.xa_prepare_execs = NULL;
                        }
                        break;
                    case 16: /* nothing to release */
                    case 24: /* nothing to release */
                    case 32: /* nothing to release */
                        break;
                    default:
                        THROW(INVALID_STEP5);
                }
                break;
            case LIXA_MSG_VERB_COMMIT: /* commit */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.commit_8.xa_commit_execs) {
                            g_array_free(msg->body.commit_8.xa_commit_execs,
                                         TRUE);
                            msg->body.commit_8.xa_commit_execs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP6);
                }
                break;
            case LIXA_MSG_VERB_ROLLBACK: /* rollback */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.rollback_8.xa_rollback_execs) {
                            g_array_free(
                                msg->body.rollback_8.xa_rollback_execs,
                                TRUE);
                            msg->body.rollback_8.xa_rollback_execs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP7);
                }
                break;
            case LIXA_MSG_VERB_QRCVR: /* qrcvr */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.qrcvr_8.client.job) {
                            xmlFree(msg->body.qrcvr_8.client.job);
                            msg->body.qrcvr_8.client.job = NULL;
                        }
                        break;
                    case 16:
                        if (LIXA_RC_OBJ_NOT_FOUND !=
                            msg->body.qrcvr_16.answer.rc) {
                            if (NULL != msg->body.qrcvr_16.client.job) {
                                xmlFree(msg->body.qrcvr_16.client.job);
                                msg->body.qrcvr_16.client.job = NULL;
                            }
                            if (NULL != msg->body.qrcvr_16.rsrmgrs) {
                                g_array_free(msg->body.qrcvr_16.rsrmgrs, TRUE);
                                msg->body.qrcvr_16.rsrmgrs = NULL;
                            }
                        }
                        break;
                    case 24:
                        if (NULL != msg->body.qrcvr_24.rsrmgrs) {
                            g_array_free(msg->body.qrcvr_24.rsrmgrs, TRUE);
                            msg->body.qrcvr_24.rsrmgrs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP8);
                }
                break;
            case LIXA_MSG_VERB_REG: /* reg */
                switch (msg->header.pvs.step) {
                    case 8:
                        break;
                    default:
                        THROW(INVALID_STEP9);
                }
                break;
            case LIXA_MSG_VERB_UNREG: /* unreg */
                switch (msg->header.pvs.step) {
                    case 8:
                        break;
                    default:
                        THROW(INVALID_STEP10);
                }
                break;
            case LIXA_MSG_VERB_FORGET: /* forget */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.forget_8.xa_forget_execs) {
                            g_array_free(msg->body.forget_8.xa_forget_execs,
                                         TRUE);
                            msg->body.forget_8.xa_forget_execs = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP11);
                }
                break;
            case LIXA_MSG_VERB_TRANS: /* trans */
                switch (msg->header.pvs.step) {
                    case 8:
                        if (NULL != msg->body.trans_8.client.job) {
                            xmlFree(msg->body.trans_8.client.job);
                            msg->body.trans_8.client.job = NULL;
                        }
                        break;
                    default:
                        THROW(INVALID_STEP12);
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
            case INVALID_STEP7:
            case INVALID_STEP8:
            case INVALID_STEP9:
            case INVALID_STEP10:
            case INVALID_STEP11:
            case INVALID_STEP12:
            case INVALID_VERB:
                LIXA_TRACE(("lixa_msg_free: verb=%d, step=%d\n",
                            msg->header.pvs.verb, msg->header.pvs.step));
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
