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
#include <lixa_xml_msg_trace.h>
#include <lixa_common_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG



static const xmlChar *nil_str = (xmlChar *)"(nil)";



int lixa_msg_trace(const struct lixa_msg_s *msg)
{
    enum Exception { TRACE_OPEN_ERROR
                     , TRACE_CLOSE_ERROR
                     , TRACE_START_ERROR
                     , TRACE_END_ERROR
                     , TRACE_PREPARE_ERROR
                     , TRACE_COMMIT_ERROR
                     , TRACE_ROLLBACK_ERROR
                     , INVALID_VERB
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace\n"));
    TRY {
        LIXA_TRACE(("lixa_msg_trace: header[level=%d,pvs.verb=%d,"
                    "pvs.step=%d]\n",
                    msg->header.level, msg->header.pvs.verb,
                    msg->header.pvs.step));
        switch (msg->header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_open(msg)))
                    THROW(TRACE_OPEN_ERROR);
                break;
            case LIXA_MSG_VERB_CLOSE: /* close */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_close(msg)))
                    THROW(TRACE_CLOSE_ERROR);
                break;
            case LIXA_MSG_VERB_START: /* start */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_start(msg)))
                    THROW(TRACE_START_ERROR);
                break;
            case LIXA_MSG_VERB_END: /* end */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_end(msg)))
                    THROW(TRACE_END_ERROR);
                break;
            case LIXA_MSG_VERB_PREPARE: /* prepare */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_prepare(msg)))
                    THROW(TRACE_PREPARE_ERROR);
                break;
            case LIXA_MSG_VERB_COMMIT: /* commit */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_commit(msg)))
                    THROW(TRACE_COMMIT_ERROR);
                break;
            case LIXA_MSG_VERB_ROLLBACK: /* rollback */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_rollback(msg)))
                    THROW(TRACE_ROLLBACK_ERROR);
                break;
            default:
                THROW(INVALID_VERB);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case TRACE_OPEN_ERROR:
            case TRACE_CLOSE_ERROR:
            case TRACE_START_ERROR:
            case TRACE_END_ERROR:
            case TRACE_PREPARE_ERROR:
            case TRACE_COMMIT_ERROR:
            case TRACE_ROLLBACK_ERROR:
                break;
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



int lixa_msg_trace_commit(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_commit\n"));
    TRY {
        guint i;
        
        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.commit_8.xa_commit_execs) {
                    LIXA_TRACE(("lixa_msg_trace: body["
                                "conthr[finished=%d]]\n",
                                msg->body.commit_8.conthr.finished));
                    for (i=0; i<msg->body.commit_8.xa_commit_execs->len;
                         ++i) {
                        struct lixa_msg_body_commit_8_xa_commit_execs_s
                            *xa_commit_exec =
                            &g_array_index(
                                msg->body.commit_8.xa_commit_execs,
                                struct
                                lixa_msg_body_commit_8_xa_commit_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body["
                                    "xa_commit_execs["
                                    "xa_commit_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,r_state=%d,s_state=%d]]]\n",
                                    xa_commit_exec->rmid,
                                    xa_commit_exec->flags,
                                    xa_commit_exec->rc,
                                    xa_commit_exec->r_state,
                                    xa_commit_exec->s_state));
                    }
                }
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_trace_close(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_close\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.close_8.rsrmgrs) {
                    for (i=0; i<msg->body.close_8.rsrmgrs->len; ++i) {
                        struct lixa_msg_body_close_8_rsrmgr_s *rsrmgr =
                            &g_array_index(
                                msg->body.close_8.rsrmgrs,
                                struct lixa_msg_body_close_8_rsrmgr_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_close: body[rsrmgrs["
                                    "rsrmgr[rmid=%d]]]\n",
                                    rsrmgr->rmid));
                    }
                }
                break;
            default:
                THROW(INVALID_STEP);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_trace_end(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_end\n"));
    TRY {
        guint i;
        
        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_end: body[conthr[commit["
                            "%d]]]\n", msg->body.end_8.conthr.commit));
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_end: body[answer[rc[%d]]]\n",
                            msg->body.end_16.answer.rc));
                break;
            case 24:
                if (NULL != msg->body.end_24.xa_end_execs) {
                    for (i=0; i<msg->body.end_24.xa_end_execs->len;
                         ++i) {
                        struct lixa_msg_body_end_24_xa_end_execs_s
                            *xa_end_exec =
                            &g_array_index(
                                msg->body.end_24.xa_end_execs,
                                struct
                                lixa_msg_body_end_24_xa_end_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body["
                                    "xa_end_execs["
                                    "xa_end_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,s_state=%d,t_state=%d]]]\n",
                                    xa_end_exec->rmid,
                                    xa_end_exec->flags,
                                    xa_end_exec->rc,
                                    xa_end_exec->s_state,
                                    xa_end_exec->t_state));
                    }
                }
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_trace_open(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_open\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace: body[client[job="
                            "'%s',config_digest='%s']]]\n",
                            msg->body.open_8.client.job,
                            msg->body.open_8.client.config_digest));
                if (NULL != msg->body.open_8.rsrmgrs) {
                    for (i=0; i<msg->body.open_8.rsrmgrs->len; ++i) {
                        struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr =
                            &g_array_index(
                                msg->body.open_8.rsrmgrs,
                                struct lixa_msg_body_open_8_rsrmgr_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body[rsrmgrs["
                                    "rsrmgr[rmid=%d,name='%s',"
                                    "xa_name='%s']]]\n",
                                    rsrmgr->rmid,
                                    rsrmgr->name ?
                                    rsrmgr->name : nil_str,
                                    rsrmgr->xa_name ?
                                    rsrmgr->xa_name : nil_str));
                    }
                }
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace: body[answer[rc[%d]]]\n",
                            msg->body.open_16.answer.rc));
                break;
            case 24:
                if (NULL != msg->body.open_24.xa_open_execs) {
                    LIXA_TRACE(("lixa_msg_trace: body["
                                "conthr[state=%d]]\n",
                                msg->body.open_24.conthr.state));
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
                                    "xa_info='%s',rmid=%d,flags=0x%lx,"
                                    "rc=%d,r_state=%d]]]\n",
                                    (char *)xa_open_exec->xa_info,
                                    xa_open_exec->rmid,
                                    xa_open_exec->flags,
                                    xa_open_exec->rc,
                                    xa_open_exec->r_state));
                    }
                }
                break;
            default:
                THROW(INVALID_STEP);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int lixa_msg_trace_prepare(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_prepare\n"));
    TRY {
        guint i;
        
        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.prepare_8.xa_prepare_execs) {
                    LIXA_TRACE(("lixa_msg_trace: body["
                                "conthr[commit=%d]]\n",
                                msg->body.prepare_8.conthr.commit));
                    for (i=0; i<msg->body.prepare_8.xa_prepare_execs->len;
                         ++i) {
                        struct lixa_msg_body_prepare_8_xa_prepare_execs_s
                            *xa_prepare_exec =
                            &g_array_index(
                                msg->body.prepare_8.xa_prepare_execs,
                                struct
                                lixa_msg_body_prepare_8_xa_prepare_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body["
                                    "xa_prepare_execs["
                                    "xa_prepare_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,s_state=%d,t_state=%d]]]\n",
                                    xa_prepare_exec->rmid,
                                    xa_prepare_exec->flags,
                                    xa_prepare_exec->rc,
                                    xa_prepare_exec->s_state,
                                    xa_prepare_exec->t_state));
                    }
                }
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_prepare: body[answer[rc[%d]]]\n",
                            msg->body.prepare_16.answer.rc));
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_trace_rollback(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_rollback\n"));
    TRY {
        guint i;
        
        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.rollback_8.xa_rollback_execs) {
                    LIXA_TRACE(("lixa_msg_trace: body["
                                "conthr[finished=%d]]\n",
                                msg->body.rollback_8.conthr.finished));
                    for (i=0; i<msg->body.rollback_8.xa_rollback_execs->len;
                         ++i) {
                        struct lixa_msg_body_rollback_8_xa_rollback_execs_s
                            *xa_rollback_exec =
                            &g_array_index(
                                msg->body.rollback_8.xa_rollback_execs,
                                struct
                                lixa_msg_body_rollback_8_xa_rollback_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body["
                                    "xa_rollback_execs["
                                    "xa_rollback_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,r_state=%d,s_state=%d]]]\n",
                                    xa_rollback_exec->rmid,
                                    xa_rollback_exec->flags,
                                    xa_rollback_exec->rc,
                                    xa_rollback_exec->r_state,
                                    xa_rollback_exec->s_state));
                    }
                }
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_msg_trace_start(const struct lixa_msg_s *msg)
{
    enum Exception { INVALID_STEP
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_msg_trace_start\n"));
    TRY {
        guint i;
        char *xid_str = NULL;
        
        switch (msg->header.pvs.step) {
            case 8:
#ifdef _TRACE
                xid_str = xid_serialize(&msg->body.start_8.conthr.xid);
                LIXA_TRACE(("lixa_msg_trace_start: body[conthr[xid["
                            "%s]]]\n", xid_str != NULL ?
                            xid_str : (char *)nil_str));
                free(xid_str);
#endif /* _TRACE */
                if (NULL != msg->body.start_8.rsrmgrs) {
                    for (i=0; i<msg->body.start_8.rsrmgrs->len; ++i) {
                        struct lixa_msg_body_start_8_rsrmgr_s *rsrmgr =
                            &g_array_index(
                                msg->body.start_8.rsrmgrs,
                                struct lixa_msg_body_start_8_rsrmgr_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_start: body[rsrmgrs["
                                    "rsrmgr[rmid=%d]]]\n",
                                    rsrmgr->rmid));
                    }
                }
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_start: body[answer[rc[%d]]]\n",
                            msg->body.start_16.answer.rc));
                break;
            case 24:
                if (NULL != msg->body.start_24.xa_start_execs) {
                    LIXA_TRACE(("lixa_msg_trace: body["
                                "conthr[state=%d]]\n",
                                msg->body.start_24.conthr.state));
                    for (i=0; i<msg->body.start_24.xa_start_execs->len;
                         ++i) {
                        struct lixa_msg_body_start_24_xa_start_execs_s
                            *xa_start_exec =
                            &g_array_index(
                                msg->body.start_24.xa_start_execs,
                                struct
                                lixa_msg_body_start_24_xa_start_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body["
                                    "xa_start_execs["
                                    "xa_start_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,t_state=%d]]]\n",
                                    xa_start_exec->rmid,
                                    xa_start_exec->flags,
                                    xa_start_exec->rc,
                                    xa_start_exec->t_state));
                    }
                }
                break;
            default:
                THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STEP:
                ret_cod = LIXA_RC_PROPERTY_INVALID_VALUE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_msg_trace_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



