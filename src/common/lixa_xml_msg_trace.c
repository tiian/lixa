/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
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
#include <lixa_xml_msg_trace.h>
#include <lixa_xid.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XML_MSG


static const xmlChar *nil_str = (xmlChar *) "(nil)";


int lixa_msg_trace(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        TRACE_OPEN_ERROR,
        TRACE_CLOSE_ERROR,
        TRACE_START_ERROR,
        TRACE_END_ERROR,
        TRACE_PREPARE_ERROR,
        TRACE_COMMIT_ERROR,
        TRACE_ROLLBACK_ERROR,
        TRACE_QRCVR_ERROR,
        TRACE_REG_ERROR,
        TRACE_UNREG_ERROR,
        TRACE_FORGET_ERROR,
        TRACE_TRANS_ERROR,
        INVALID_VERB,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace\n"));
    TRY {
        LIXA_TRACE(("lixa_msg_trace: header[level=%d,pvs.verb=%d,"
                    "pvs.step=%d]\n",
                    msg->header.level, msg->header.pvs.verb,
                    msg->header.pvs.step));
        switch (msg->header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN: /* open */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_open(msg))) THROW(
                    TRACE_OPEN_ERROR);
                break;
            case LIXA_MSG_VERB_CLOSE: /* close */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_close(msg))) THROW(
                    TRACE_CLOSE_ERROR);
                break;
            case LIXA_MSG_VERB_START: /* start */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_start(msg))) THROW(
                    TRACE_START_ERROR);
                break;
            case LIXA_MSG_VERB_END: /* end */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_end(msg))) THROW(
                    TRACE_END_ERROR);
                break;
            case LIXA_MSG_VERB_PREPARE: /* prepare */
                if (LIXA_RC_OK !=
                    (ret_cod = lixa_msg_trace_prepare(msg))) THROW(
                        TRACE_PREPARE_ERROR);
                break;
            case LIXA_MSG_VERB_COMMIT: /* commit */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_commit(msg))) THROW(
                    TRACE_COMMIT_ERROR);
                break;
            case LIXA_MSG_VERB_ROLLBACK: /* rollback */
                if (LIXA_RC_OK !=
                    (ret_cod = lixa_msg_trace_rollback(msg))) THROW(
                        TRACE_ROLLBACK_ERROR);
                break;
            case LIXA_MSG_VERB_QRCVR: /* qrcvr */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_qrcvr(msg))) THROW(
                    TRACE_QRCVR_ERROR);
                break;
            case LIXA_MSG_VERB_REG: /* reg */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_reg(msg))) THROW(
                    TRACE_REG_ERROR);
                break;
            case LIXA_MSG_VERB_UNREG: /* unreg */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_unreg(msg))) THROW(
                    TRACE_UNREG_ERROR);
                break;
            case LIXA_MSG_VERB_FORGET: /* forget */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_forget(msg))) THROW(
                    TRACE_FORGET_ERROR);
                break;
            case LIXA_MSG_VERB_TRANS: /* transactions */
                if (LIXA_RC_OK != (ret_cod = lixa_msg_trace_trans(msg))) THROW(
                    TRACE_TRANS_ERROR);
                break;
            default: THROW(INVALID_VERB);
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case TRACE_OPEN_ERROR:
                case TRACE_CLOSE_ERROR:
                case TRACE_START_ERROR:
                case TRACE_END_ERROR:
                case TRACE_PREPARE_ERROR:
                case TRACE_COMMIT_ERROR:
                case TRACE_ROLLBACK_ERROR:
                case TRACE_QRCVR_ERROR:
                case TRACE_REG_ERROR:
                case TRACE_UNREG_ERROR:
                case TRACE_FORGET_ERROR:
                case TRACE_TRANS_ERROR:
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
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_commit\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.commit_8.xa_commit_execs) {
                    LIXA_TRACE(("lixa_msg_trace_commit: body["
                                "conthr[finished=%d]]\n",
                                msg->body.commit_8.conthr.finished));
                    for (i = 0; i < msg->body.commit_8.xa_commit_execs->len;
                         ++i) {
                        struct lixa_msg_body_commit_8_xa_commit_execs_s
                            *xa_commit_exec =
                            &g_array_index(
                                msg->body.commit_8.xa_commit_execs,
                                struct
                                lixa_msg_body_commit_8_xa_commit_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_commit: body["
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
            default: THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_close\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.close_8.rsrmgrs) {
                    for (i = 0; i < msg->body.close_8.rsrmgrs->len; ++i) {
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
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_end\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_end: body[conthr[commit["
                            "%d]]]\n", msg->body.end_8.conthr.commit));
                if (NULL != msg->body.end_8.xa_end_execs) {
                    for (i = 0; i < msg->body.end_8.xa_end_execs->len;
                         ++i) {
                        struct lixa_msg_body_end_8_xa_end_execs_s
                            *xa_end_exec =
                            &g_array_index(
                                msg->body.end_8.xa_end_execs,
                                struct
                                lixa_msg_body_end_8_xa_end_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace: body["
                                    "xa_end_execs["
                                    "xa_end_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,s_state=%d,td_state=%d]]]\n",
                                    xa_end_exec->rmid,
                                    xa_end_exec->flags,
                                    xa_end_exec->rc,
                                    xa_end_exec->s_state,
                                    xa_end_exec->td_state));
                    }
                }
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_end: body[answer[rc[%d]]]\n",
                            msg->body.end_16.answer.rc));
                break;
            default: THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */

        THROW(NONE);
    }
    CATCH
        {
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


int lixa_msg_trace_forget(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_forget\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.forget_8.xa_forget_execs) {
                    LIXA_TRACE(("lixa_msg_trace_forget: body["
                                "conthr[finished=%d]]\n",
                                msg->body.forget_8.conthr.finished));
                    for (i = 0; i < msg->body.forget_8.xa_forget_execs->len;
                         ++i) {
                        struct lixa_msg_body_forget_8_xa_forget_execs_s
                            *xa_forget_exec =
                            &g_array_index(
                                msg->body.forget_8.xa_forget_execs,
                                struct
                                lixa_msg_body_forget_8_xa_forget_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_forget: body["
                                    "xa_forget_execs["
                                    "xa_forget_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,s_state=%d]]]\n",
                                    xa_forget_exec->rmid,
                                    xa_forget_exec->flags,
                                    xa_forget_exec->rc,
                                    xa_forget_exec->s_state));
                    }
                }
                break;
            default: THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */

        THROW(NONE);
    }
    CATCH
        {
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
    LIXA_TRACE(("lixa_msg_trace_forget/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int lixa_msg_trace_open(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_open\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_open: body[client[job="
                            "'%s',config_digest='%s',maint=%d]]]\n",
                            msg->body.open_8.client.job,
                            msg->body.open_8.client.config_digest,
                            msg->body.open_8.client.maint));
                if (NULL != msg->body.open_8.rsrmgrs) {
                    for (i = 0; i < msg->body.open_8.rsrmgrs->len; ++i) {
                        struct lixa_msg_body_open_8_rsrmgr_s *rsrmgr =
                            &g_array_index(
                                msg->body.open_8.rsrmgrs,
                                struct lixa_msg_body_open_8_rsrmgr_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_open: body[rsrmgrs["
                                    "rsrmgr[rmid=%d,dynamic=%d,name='%s',"
                                    "xa_name='%s']]]\n",
                                    rsrmgr->rmid,
                                    rsrmgr->dynamic,
                                    rsrmgr->name ?
                                    rsrmgr->name : nil_str,
                                    rsrmgr->xa_name ?
                                    rsrmgr->xa_name : nil_str));
                    }
                }
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_open: body[answer[rc[%d]]]\n",
                            msg->body.open_16.answer.rc));
                break;
            case 24:
                if (NULL != msg->body.open_24.xa_open_execs) {
                    LIXA_TRACE(("lixa_msg_trace_open: body["
                                "conthr[txstate=%d]]\n",
                                msg->body.open_24.conthr.txstate));
                    for (i = 0; i < msg->body.open_24.xa_open_execs->len;
                         ++i) {
                        struct lixa_msg_body_open_24_xa_open_execs_s
                            *xa_open_exec =
                            &g_array_index(
                                msg->body.open_24.xa_open_execs,
                                struct
                                lixa_msg_body_open_24_xa_open_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_open: body["
                                    "xa_open_execs["
                                    "xa_open_exec["
                                    "xa_info='%s',rmid=%d,flags=0x%lx,"
                                    "rc=%d,r_state=%d]]]\n",
                                    (char *) xa_open_exec->xa_info,
                                    xa_open_exec->rmid,
                                    xa_open_exec->flags,
                                    xa_open_exec->rc,
                                    xa_open_exec->r_state));
                    }
                }
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_prepare\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.prepare_8.xa_prepare_execs) {
                    LIXA_TRACE(("lixa_msg_trace_prepare: body["
                                "conthr[commit=%d]]\n",
                                msg->body.prepare_8.conthr.commit));
                    for (i = 0; i < msg->body.prepare_8.xa_prepare_execs->len;
                         ++i) {
                        struct lixa_msg_body_prepare_8_xa_prepare_execs_s
                            *xa_prepare_exec =
                            &g_array_index(
                                msg->body.prepare_8.xa_prepare_execs,
                                struct
                                lixa_msg_body_prepare_8_xa_prepare_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_prepare: body["
                                    "xa_prepare_execs["
                                    "xa_prepare_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,s_state=%d,td_state=%d]]]\n",
                                    xa_prepare_exec->rmid,
                                    xa_prepare_exec->flags,
                                    xa_prepare_exec->rc,
                                    xa_prepare_exec->s_state,
                                    xa_prepare_exec->td_state));
                    }
                }
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_prepare: body[answer[rc[%d]]]\n",
                            msg->body.prepare_16.answer.rc));
                break;
            default: THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */

        THROW(NONE);
    }
    CATCH
        {
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


int lixa_msg_trace_qrcvr(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        XID_SERIALIZE_ERROR, INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    lixa_ser_xid_t ser_xid = "";

    LIXA_TRACE(("lixa_msg_trace_qrcvr\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_qrcvr: body[client[job="
                            "'%s',config_digest='%s']]]\n",
                            msg->body.qrcvr_8.client.job,
                            msg->body.qrcvr_8.client.config_digest));
                break;
            case 16:
                LIXA_TRACE(("lixa_msg_trace_qrcvr: body[answer[rc[%d]]]\n",
                            msg->body.qrcvr_16.answer.rc));
                if (LIXA_RC_OBJ_NOT_FOUND == msg->body.qrcvr_16.answer.rc)
                    break;
                if (!lixa_xid_serialize(
                        &msg->body.qrcvr_16.client.state.xid, ser_xid)) THROW(
                            XID_SERIALIZE_ERROR);
                LIXA_TRACE(("lixa_msg_trace_qrcvr: body[client[job="
                            "'%s',config_digest='%s']]]\n",
                            msg->body.qrcvr_16.client.job,
                            msg->body.qrcvr_16.client.config_digest));
                LIXA_TRACE(("lixa_msg_trace_qrcvr: body[client[last_verb_step["
                            "verb=%d,step=%d]]]]\n",
                            msg->body.qrcvr_16.client.last_verb_step.verb,
                            msg->body.qrcvr_16.client.last_verb_step.step));
                LIXA_TRACE(("lixa_msg_trace_qrcvr: body[client[state["
                            "finished=%d,txstate=%d,will_commit=%d,"
                            "will_rollback=%d,xid='%s']]]]\n",
                            msg->body.qrcvr_16.client.state.finished,
                            msg->body.qrcvr_16.client.state.txstate,
                            msg->body.qrcvr_16.client.state.will_commit,
                            msg->body.qrcvr_16.client.state.will_rollback,
                            ser_xid));
                if (NULL != msg->body.qrcvr_16.rsrmgrs) {
                    for (i = 0; i < msg->body.qrcvr_16.rsrmgrs->len; ++i) {
                        struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                            &g_array_index(
                                msg->body.qrcvr_16.rsrmgrs,
                                struct lixa_msg_body_qrcvr_16_rsrmgr_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_qrcvr: body[rsrmgrs["
                                    "rsrmgr[rmid=%d,next_verb=%d,r_state=%d,"
                                    "s_state=%d,td_state=%d]]]\n",
                                    rsrmgr->rmid, rsrmgr->next_verb,
                                    rsrmgr->r_state, rsrmgr->s_state,
                                    rsrmgr->td_state));
                    }
                }
                break;
            case 24:
                LIXA_TRACE(("lixa_msg_trace_qrcvr: body[recovery[failed=%d,"
                            "commit=%d]]\n",
                            msg->body.qrcvr_24.recovery.failed,
                            msg->body.qrcvr_24.recovery.commit));
                if (NULL != msg->body.qrcvr_24.rsrmgrs) {
                    for (i = 0; i < msg->body.qrcvr_24.rsrmgrs->len; ++i) {
                        struct lixa_msg_body_qrcvr_24_rsrmgr_s *rsrmgr =
                            &g_array_index(
                                msg->body.qrcvr_24.rsrmgrs,
                                struct lixa_msg_body_qrcvr_24_rsrmgr_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_qrcvr: body[rsrmgrs["
                                    "rsrmgr[rmid=%d,rc=%d]]]\n",
                                    rsrmgr->rmid, rsrmgr->rc));
                    }
                }
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case XID_SERIALIZE_ERROR:
                    ret_cod = LIXA_RC_NULL_OBJECT;
                    break;
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
    LIXA_TRACE(("lixa_msg_trace_qrcvr/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int lixa_msg_trace_reg(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_reg\n"));
    TRY {
        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_reg: body[ax_reg_exec["
                            "rmid=%d,flags=0x%lx,rc=%d,td_state=%d,"
                            "s_state=%d]]\n",
                            msg->body.reg_8.ax_reg_exec.rmid,
                            msg->body.reg_8.ax_reg_exec.flags,
                            msg->body.reg_8.ax_reg_exec.rc,
                            msg->body.reg_8.ax_reg_exec.td_state,
                            msg->body.reg_8.ax_reg_exec.s_state));
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    }
    CATCH
        {
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
    LIXA_TRACE(("lixa_msg_trace_reg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int lixa_msg_trace_rollback(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_rollback\n"));
    TRY {
        guint i;

        switch (msg->header.pvs.step) {
            case 8:
                if (NULL != msg->body.rollback_8.xa_rollback_execs) {
                    LIXA_TRACE(("lixa_msg_trace_rollback: body["
                                "conthr[finished=%d]]\n",
                                msg->body.rollback_8.conthr.finished));
                    for (i = 0; i < msg->body.rollback_8.xa_rollback_execs->len;
                         ++i) {
                        struct lixa_msg_body_rollback_8_xa_rollback_execs_s
                            *xa_rollback_exec =
                            &g_array_index(
                                msg->body.rollback_8.xa_rollback_execs,
                                struct
                                lixa_msg_body_rollback_8_xa_rollback_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_rollback: body["
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
            default: THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_start\n"));
    TRY {
        guint i;
        lixa_ser_xid_t xid_str;

        switch (msg->header.pvs.step) {
            case 8:
#ifdef _TRACE
                if (lixa_xid_serialize(
                        &msg->body.start_8.conthr.xid, xid_str)) {
                    LIXA_TRACE(("lixa_msg_trace_start: body[conthr[xid["
                                "%s]]]\n", xid_str != NULL ?
                                xid_str : (char *)nil_str));
                }
#endif /* _TRACE */
                if (NULL != msg->body.start_8.rsrmgrs) {
                    for (i = 0; i < msg->body.start_8.rsrmgrs->len; ++i) {
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
                    LIXA_TRACE(("lixa_msg_trace_start: body["
                                "conthr[txstate=%d]]\n",
                                msg->body.start_24.conthr.txstate));
                    for (i = 0; i < msg->body.start_24.xa_start_execs->len;
                         ++i) {
                        struct lixa_msg_body_start_24_xa_start_execs_s
                            *xa_start_exec =
                            &g_array_index(
                                msg->body.start_24.xa_start_execs,
                                struct
                                lixa_msg_body_start_24_xa_start_execs_s,
                                i);
                        LIXA_TRACE(("lixa_msg_trace_start: body["
                                    "xa_start_execs["
                                    "xa_start_exec["
                                    "rmid=%d,flags=0x%lx,"
                                    "rc=%d,td_state=%d,s_state=%d]]]\n",
                                    xa_start_exec->rmid,
                                    xa_start_exec->flags,
                                    xa_start_exec->rc,
                                    xa_start_exec->td_state,
                                    xa_start_exec->s_state));
                    }
                }
                break;
            default: THROW(INVALID_STEP);
        } /* switch (msg->header.pvs.step) */

        THROW(NONE);
    }
    CATCH
        {
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


int lixa_msg_trace_unreg(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_unreg\n"));
    TRY {
        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_unreg: body[ax_unreg_exec["
                            "rmid=%d,flags=0x%lx,rc=%d,td_state=%d]]\n",
                            msg->body.unreg_8.ax_unreg_exec.rmid,
                            msg->body.unreg_8.ax_unreg_exec.flags,
                            msg->body.unreg_8.ax_unreg_exec.rc,
                            msg->body.unreg_8.ax_unreg_exec.td_state));
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    }
    CATCH
        {
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
    LIXA_TRACE(("lixa_msg_trace_unreg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

int lixa_msg_trace_trans(const struct lixa_msg_s *msg)
{
    enum Exception
    {
        INVALID_STEP, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_msg_trace_trans\n"));
    TRY {
        switch (msg->header.pvs.step) {
            case 8:
                LIXA_TRACE(("lixa_msg_trace_trans: body[client[job="
                            "'%s',config_digest='%s']]]\n",
                            msg->body.qrcvr_8.client.job,
                            msg->body.qrcvr_8.client.config_digest));
                break;
            default: THROW(INVALID_STEP);
        }

        THROW(NONE);
    }
    CATCH
        {
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
    LIXA_TRACE(("lixa_msg_trace_trans/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
