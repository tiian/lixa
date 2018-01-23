/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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



#include <client_status.h>
#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg_serialize.h>
#include <lixa_xid.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



int ax_reg(int rmid, XID *xid, long flags)
{
    enum Exception { CLIENT_NOT_INITIALIZED
                     , COLL_GET_CS_ERROR
                     , OUT_OF_RANGE
                     , NOT_DYNAMIC
                     , INVALID_TX_STATE
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , NONE } excp;
    int ret_cod = TMER_TMERR;
    int xa_ret_cod = TM_OK;

    LIXA_TRACE_INIT;
    LIXA_TRACE(("ax_reg: rmid=%d, xid=%p, flags=0x%lx\n", rmid, xid, flags));
    TRY {
        client_status_t *cs;
        struct client_status_rsrmgr_s *csr;
        struct lixa_msg_s msg;
        size_t buffer_size = 0;
        int fd, txstate, next_xa_td_state = XA_STATE_D1;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        struct act_rsrmgr_config_s *act_rsrmgr;
        lixa_ser_xid_t ser_xid = "";

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                LIXA_TRACE(("ax_reg: status not found\n"));
                THROW(CLIENT_NOT_INITIALIZED);
                break;
            default:
                THROW(COLL_GET_CS_ERROR);
        }
        
        /* check the rmid value is not out of range */
        if (0 > rmid || global_ccc.actconf.rsrmgrs->len <= rmid) {
            LIXA_TRACE(("ax_unreg: rmid out of range\n"));
            THROW(OUT_OF_RANGE);
        }
        
        /* check the resource manager declared dynamic registration */
        if (TM_OK == xa_ret_cod) {
            act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, rmid);
            if (!(TMREGISTER &
                  lixa_iface_get_flags(&act_rsrmgr->lixa_iface))) {
                LIXA_TRACE(("ax_reg: resource manager %d ('%s') did not set "
                            "TMREGISTER flag (flags=0x%lx)\n", rmid,
                            lixa_iface_get_name(&act_rsrmgr->lixa_iface),
                            lixa_iface_get_flags(&act_rsrmgr->lixa_iface)));
                xa_ret_cod = TMER_TMERR;
            }
        }
        
        /* check the status of the transaction manager */
        txstate = client_status_get_txstate(cs);
        if (TM_OK == xa_ret_cod)
            switch (txstate) {
                case TX_STATE_S0:
                    LIXA_TRACE(("ax_reg: the transaction has not yet opened "
                                "the resource managers (TX state S%d)\n",
                                txstate));
                    xa_ret_cod = TMER_PROTO;
                    break;
                case TX_STATE_S1:
                case TX_STATE_S2:
                    LIXA_TRACE(("ax_reg: the application program has not yet "
                                "started a transaction (TX states S%d); "
                                "null XID will be returned\n", txstate));
                    xid->formatID = -1;
                    next_xa_td_state = XA_STATE_D3;
                    break;
                case TX_STATE_S3:
                case TX_STATE_S4:
                    lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
#ifdef _TRACE
                    LIXA_TRACE(("ax_reg: the application program has "
                                "started a transaction (TX states S%d); "
                                "this XID '%s' will be returned\n",
                                txstate,
                                NULL == ser_xid ? "" : ser_xid));
#endif
                    memcpy(xid, client_status_get_xid(cs), sizeof(XID));
                    break;
                default:
                    LIXA_TRACE(("ax_reg: invalid TX state (%d)\n", txstate));
                    THROW(INVALID_TX_STATE);
            }
        
        /* check the status of the resource manager */
        csr = &g_array_index(cs->rmstatus, struct client_status_rsrmgr_s,
                             rmid);
        if ((TM_OK == xa_ret_cod) && !csr->common.dynamic) {
            LIXA_TRACE(("ax_reg: resource manager # %d is not using "
                        "dynamic registration\n", rmid));
            xa_ret_cod = TMER_INVAL;
        }

        if ((TM_OK == xa_ret_cod) && (XA_STATE_R1 != csr->common.xa_r_state)) {
            LIXA_TRACE(("ax_reg: invalid XA r_state (%d)\n",
                        csr->common.xa_r_state));
            xa_ret_cod = TMER_PROTO;
        }

        if ((TM_OK == xa_ret_cod) &&
            (XA_STATE_D0 != csr->common.xa_td_state)) {
            LIXA_TRACE(("ax_reg: invalid XA td_state (%d)\n",
                        csr->common.xa_td_state));
            xa_ret_cod = TMER_PROTO;
        }

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_REG;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;
        
        msg.body.reg_8.ax_reg_exec.rmid = rmid;
        msg.body.reg_8.ax_reg_exec.flags = flags;
        msg.body.reg_8.ax_reg_exec.rc = ret_cod;
        msg.body.reg_8.ax_reg_exec.td_state = next_xa_td_state;
        if (XA_STATE_D1 == next_xa_td_state)
            msg.body.reg_8.ax_reg_exec.s_state = XA_STATE_S1;
        else
            msg.body.reg_8.ax_reg_exec.s_state = csr->common.xa_s_state;
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);
        
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);
        LIXA_TRACE(("ax_reg: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_AX_REG_1,
                   client_status_get_crash_count(cs));
        
        /* new resource manager state */
        csr->common.xa_td_state = next_xa_td_state;
        if (XA_STATE_D1 == next_xa_td_state)
            csr->common.xa_s_state = XA_STATE_S1;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_NOT_INITIALIZED:
                ret_cod = TMER_PROTO;
                break;
            case COLL_GET_CS_ERROR:
                ret_cod = TMER_TMERR;
                break;
            case OUT_OF_RANGE:
                ret_cod = TMER_INVAL;
                break;
            case NOT_DYNAMIC:
            case INVALID_TX_STATE:
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
                ret_cod = TMER_TMERR;
                break;
            case NONE:
                ret_cod = xa_ret_cod;
                break;
            default:
                ret_cod = TMER_TMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("ax_reg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int ax_unreg(int rmid, long flags)
{
    enum Exception { COLL_GET_CS_NOT_FOUND
                     , COLL_GET_CS_ERROR
                     , OUT_OF_RANGE
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , NONE } excp;
    int ret_cod = TMER_TMERR;
    int xa_ret_cod = TM_OK;
    
    LIXA_TRACE_INIT;
    LIXA_TRACE(("ax_unreg: rmid=%d, flags=0x%lx\n", rmid, flags));
    TRY {
        client_status_t *cs;
        struct client_status_rsrmgr_s *csr;
        struct lixa_msg_s msg;
        size_t buffer_size = 0;
        int fd;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        struct act_rsrmgr_config_s *act_rsrmgr;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                LIXA_TRACE(("ax_unreg: status not found\n"));
                THROW(COLL_GET_CS_NOT_FOUND);
                break;
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        /* check the rmid value is not out of range */
        if (0 > rmid || global_ccc.actconf.rsrmgrs->len <= rmid) {
            LIXA_TRACE(("ax_unreg: rmid out of range\n"));
            THROW(OUT_OF_RANGE);
        }
        
        /* check the resource manager declared dynamic registration */
        if (TM_OK == xa_ret_cod) {
            act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, rmid);
            if (!(TMREGISTER &
                  lixa_iface_get_flags(&act_rsrmgr->lixa_iface))) {
                LIXA_TRACE(("ax_unreg: resource manager %d did not set "
                            "TMREGISTER flag (flags=0x%lx)\n", rmid,
                            lixa_iface_get_flags(&act_rsrmgr->lixa_iface)));
                xa_ret_cod = TMER_TMERR;
            }        
        }
        
        /* check the status of the resource manager */
        csr = &g_array_index(cs->rmstatus, struct client_status_rsrmgr_s,
                             rmid);
        if ((TM_OK == xa_ret_cod) && !csr->common.dynamic) {
            LIXA_TRACE(("ax_unreg: resource manager # %d is not using "
                        "dynamic registration\n", rmid));
            xa_ret_cod = TMER_INVAL;
        }

        if ((TM_OK == xa_ret_cod) && (XA_STATE_R1 != csr->common.xa_r_state)) {
            LIXA_TRACE(("ax_unreg: invalid XA r_state (%d)\n",
                        csr->common.xa_r_state));
            xa_ret_cod = TMER_PROTO;
        }

        if ((TM_OK == xa_ret_cod) &&
            (XA_STATE_D3 != csr->common.xa_td_state)) {
            LIXA_TRACE(("ax_unreg: invalid XA td_state (%d)\n",
                        csr->common.xa_td_state));
            xa_ret_cod = TMER_PROTO;
        }

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_UNREG;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;
        
        msg.body.unreg_8.ax_unreg_exec.rmid = rmid;
        msg.body.unreg_8.ax_unreg_exec.flags = flags;
        msg.body.unreg_8.ax_unreg_exec.rc = xa_ret_cod;
        msg.body.unreg_8.ax_unreg_exec.td_state = XA_STATE_D0;
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);
        
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);
        LIXA_TRACE(("ax_unreg: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_AX_UNREG_1,
                   client_status_get_crash_count(cs));
        
        /* new resource manager state */
        csr->common.xa_td_state = XA_STATE_D0;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_NOT_FOUND:
                ret_cod = TMER_PROTO;
                break;
            case COLL_GET_CS_ERROR:
                ret_cod = TMER_TMERR;
                break;
            case OUT_OF_RANGE:
                ret_cod = TMER_INVAL;
                break;
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
                ret_cod = TMER_TMERR;
                break;
            case NONE:
                ret_cod = xa_ret_cod;
                break;
            default:
                ret_cod = TMER_TMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("ax_unreg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
