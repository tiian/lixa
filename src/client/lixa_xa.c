/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include <lixa_crash.h>
#include <lixa_trace.h>
#include <lixa_errors.h>
#include <lixa_xid.h>
#include <lixa_tx_rc.h>
#include <lixa_xa.h>
#include <lixa_syslog.h>
#include <lixa_xml_msg_deserialize.h>
#include <lixa_xml_msg_serialize.h>
#include <lixa_xml_msg_trace.h>
#include <client_recovery.h>
#include <client_status.h>
#include <tx.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



int lixa_xa_close(client_status_t *cs, int *txrc)
{
    enum Exception { ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , XA_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_close\n"));
    TRY {
        struct lixa_msg_s msg;
        int fd;
        size_t buffer_size = 0;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        long xa_close_flags = TMNOFLAGS;

        /* it seems there's no reason to send info to the server about xa_close
           completion code; the message is sent only to free the slots
           reserved for the current thread of control */
        *txrc = TX_OK;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            int tmp_txrc = TX_OK;
            int rc;
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            rc = act_rsrmgr->xa_switch->xa_close_entry(
                act_rsrmgr->generic->xa_close_info, i, xa_close_flags);
            LIXA_TRACE(("lixa_xa_close: xa_close_entry('%s', %d, 0x%lx) = "
                        "%d\n", act_rsrmgr->generic->xa_close_info,
                        i, xa_close_flags, rc));
            switch (rc) {
                case XA_OK:
                    break;
                case XAER_RMERR:
                    tmp_txrc = TX_ERROR;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    tmp_txrc = TX_FAIL;
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXC019W,
                           (char *)act_rsrmgr->generic->name, i, rc);
                    LIXA_TRACE(("lixa_xa_close: resource manager # %d "
                                "returned unexpected return code: %d\n",
                                i, rc));
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);                    
            } /* switch (rc) */
            if (lixa_tx_rc_hierarchy(tmp_txrc) <
                lixa_tx_rc_hierarchy(*txrc))
                *txrc = tmp_txrc;
        }
                
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_CLOSE;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.close_8.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_close_8_rsrmgr_s),
            global_ccc.actconf.rsrmgrs->len);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct lixa_msg_body_close_8_rsrmgr_s record;
            record.rmid = i;
            g_array_append_val(msg.body.close_8.rsrmgrs, record);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        /* this object contains a lot of references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.close_8.rsrmgrs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_close: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }

        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_CLOSE_1,
                   client_status_get_crash_count(cs));
        
        if (TX_OK != *txrc)
            THROW(XA_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
                *txrc = TX_FAIL;
                break;
            case XA_ERROR:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_commit(client_status_t *cs, int *txrc, int one_phase_commit)
{
    enum Exception { TX_RC_ADD_ERROR1
                     , TX_RC_ADD_ERROR2
                     , INVALID_XA_RC
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct lixa_msg_s msg; 
    lixa_tx_rc_t ltr = LIXA_TX_RC_T_INIT;
    *txrc = TX_FAIL;
    msg.body.commit_8.xa_commit_execs = NULL;
    
    LIXA_TRACE(("lixa_xa_commit\n"));
    TRY {
        size_t buffer_size = 0;
        int fd;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        lixa_ser_xid_t ser_xid = "";

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_COMMIT;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.commit_8.conthr.finished = TRUE;
        msg.body.commit_8.xa_commit_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_commit_8_xa_commit_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        LIXA_TRACE(("lixa_xa_commit: one_phase_commit=%d\n",
                    one_phase_commit));

        /* prepare the object to compute *txrc */
        lixa_tx_rc_create(&ltr, TRUE, TRUE, global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_commit function */
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_commit_8_xa_commit_execs_s record;
            record.rmid = i;
            
            /* bypass resource managers returned XA_RDONLY at xa_prepare()
               level */
            if (csr->common.xa_s_state == XA_STATE_S0 &&
                csr->prepare_rc == XA_RDONLY) {
                LIXA_TRACE(("lixa_xa_commit: resource manager # %i "
                            "(xa_s_state=%d) returned %d (XA_RDONLY) at "
                            "xa_prepare() call, skipping...\n",
                            record.rmid, csr->common.xa_s_state,
                            csr->prepare_rc));
                if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(&ltr, XA_OK)))
                    THROW(TX_RC_ADD_ERROR1);
                continue;
            }
            
            /* bypass resource managers are not prepared */
            if ((one_phase_commit &&
                 XA_STATE_S2 != csr->common.xa_s_state) ||
                (!one_phase_commit &&
                 XA_STATE_S3 != csr->common.xa_s_state)) {
                    LIXA_TRACE(("lixa_xa_commit: rmid=%d, "
                                "one_phase_commit=%d, "
                                "xa_s_state=%d, bypassing...\n",
                                record.rmid, one_phase_commit,
                                csr->common.xa_s_state));
                    continue;
                }

            /* bypass resource managers have not dynamically registered and
               statically ended the transaction */
            if (csr->common.dynamic && csr->common.xa_s_state != XA_STATE_S2 &&
                csr->common.xa_s_state != XA_STATE_S3) {
                LIXA_TRACE(("lixa_xa_commit: resource manager # %i "
                            "has not yet dynamically registered and "
                            "statically ended/prepared, skipping...\n",
                            record.rmid));
                continue;
            }
            
            record.flags = one_phase_commit ? TMONEPHASE : TMNOFLAGS;
            record.rc = act_rsrmgr->xa_switch->xa_commit_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_commit: xa_commit_entry(xid, %d, 0x%lx) = "
                        "%d\n", record.rmid, record.flags, record.rc));
            if (XA_RETRY == record.rc) {
                /* try a second time */
                sleep(1); /* this is a critical choice... */
                LIXA_TRACE(("lixa_xa_commit: XA_RETRY, trying again...\n"));
                record.rc = act_rsrmgr->xa_switch->xa_commit_entry(
                    client_status_get_xid(cs), record.rmid, record.flags);
                LIXA_TRACE(("lixa_xa_commit: xa_commit_entry(xid, %d, 0x%lx) "
                            "= %d\n", record.rmid, record.flags, record.rc));
            }
            
            if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(&ltr, record.rc)))
                THROW(TX_RC_ADD_ERROR2);
            
            switch (record.rc) {
                case XA_HEURHAZ:
                case XA_HEURCOM:
                case XA_HEURRB:
                case XA_HEURMIX:
                    csr->common.xa_s_state = XA_STATE_S5;
                    msg.body.commit_8.conthr.finished = FALSE;
                    break;
                case XA_OK:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->common.xa_s_state = XA_STATE_S0;
                    if (!(TMONEPHASE & record.flags)) {
                        lixa_xid_serialize(
                            client_status_get_xid(cs), ser_xid);
                        syslog(LOG_WARNING, LIXA_SYSLOG_LXC017W,
                               (char *)act_rsrmgr->generic->name, record.rmid,
                               record.rc, NULL != ser_xid ? ser_xid : "");
                        LIXA_TRACE(("lixa_xa_commit: xa_commit returned "
                                    "XA_RB* (%d) for rmid=%d,xid='%s' but "
                                    "TMONEPHASE was not used\n",
                                    record.rc, record.rmid,
                                    NULL != ser_xid ? ser_xid : ""));
                        THROW(INVALID_XA_RC);
                    }
                    break;                    
                case XAER_ASYNC:
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case XAER_RMERR:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;
                case XA_RETRY:
                    if (TMONEPHASE & record.flags) {
                        /* transaction consistency is delegated to
                           Resource Manager behavior */
                        csr->common.xa_s_state = XA_STATE_S0;
                        lixa_xid_serialize(
                            client_status_get_xid(cs), ser_xid);
                        syslog(LOG_WARNING, LIXA_SYSLOG_LXC026W,
                               (char *)act_rsrmgr->generic->name, record.rmid,
                               record.rc, NULL != ser_xid ? ser_xid : "");
                        LIXA_TRACE(("lixa_xa_commit: xa_commit returned "
                                    "XA_RETRY (%d) for rmid=%d,xid='%s' and "
                                    "TMONEPHASE was used\n",
                                    record.rc, record.rmid,
                                    NULL != ser_xid ? ser_xid : ""));
                    }
                    break;
                case XAER_RMFAIL:
                    csr->common.xa_r_state = XA_STATE_R0;
                    break;
                case XAER_NOTA:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    break;
                default:
                    THROW(UNEXPECTED_XA_RC);
            }
            record.r_state = csr->common.xa_r_state;
            record.s_state = csr->common.xa_s_state;
            g_array_append_val(msg.body.commit_8.xa_commit_execs, record);
        } /* for (i=0; ...) */

        *txrc = lixa_tx_rc_get(&ltr);

        if (TX_MIXED == *txrc || TX_HAZARD == *txrc) {
            lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
            syslog(LOG_WARNING, LIXA_SYSLOG_LXC011W,
                   NULL != ser_xid ? ser_xid : "",
                   TX_MIXED == *txrc ? "TX_MIXED" : "TX_HAZARD");
        }
        
        switch (*txrc) {
            case TX_OK:
            case TX_OUTSIDE:
            case TX_ROLLBACK:
            case TX_COMMITTED:
            case TX_MIXED:
            case TX_HAZARD:
                break;
            default:
                msg.body.commit_8.conthr.finished = FALSE;
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        LIXA_TRACE(("lixa_xa_commit: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_COMMIT_1,
                   client_status_get_crash_count(cs));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case TX_RC_ADD_ERROR1:
            case TX_RC_ADD_ERROR2:
                break;
            case INVALID_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release memory */
        lixa_tx_rc_delete(&ltr);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        if (NULL != msg.body.commit_8.xa_commit_execs)
            g_array_free(msg.body.commit_8.xa_commit_execs, TRUE);
        
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_end(client_status_t *cs, int *txrc, int commit)
{
    enum Exception { MSG_SERIALIZE_ERROR1
                     , MSG_SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , XA_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    struct lixa_msg_s msg;
    msg.body.end_8.xa_end_execs = NULL;    
    
    LIXA_TRACE(("lixa_xa_end\n"));
    TRY {
        size_t buffer_size = 0;
        int fd;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        long xa_end_flags = TMNOFLAGS;
        int read_write_rsrmgr = 0;
        
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_END;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.end_8.conthr.commit = commit;        
        msg.body.end_8.xa_end_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_end_8_xa_end_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_open function */
        *txrc = TX_OK;
        xa_end_flags = TMSUCCESS;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            int tmp_txrc = TX_OK;
            lixa_ser_xid_t ser_xid = "";
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_end_8_xa_end_execs_s record;

            /* dynamic registered resource managers with unregistered state
               must be bypassed */
            if (csr->common.dynamic &&
                csr->common.xa_td_state != XA_STATE_D1) {
                LIXA_TRACE(("lixa_xa_end: resource manager # %u has not "
                            "dynamically registered itself, skipping...\n",
                            i));
                continue;
            }
            
            record.rmid = i;
            record.flags = xa_end_flags;
            record.rc = act_rsrmgr->xa_switch->xa_end_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_end: xa_end_entry(xid, %d, 0x%lx) = %d\n",
                        record.rmid, record.flags, record.rc));

            read_write_rsrmgr++;
            switch (record.rc) {
                case XA_NOMIGRATE:
                    tmp_txrc = TX_FAIL;
                    csr->common.xa_td_state = XA_STATE_T0;
                    lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXC015W,
                           (char *)act_rsrmgr->generic->name, record.rmid,
                           NULL != ser_xid ? ser_xid : "");
                    LIXA_TRACE(("lixa_xa_end: xa_end returned "
                                "XA_NOMIGRATE for rmid=%d,xid='%s' and this "
                                "should NOT happen!\n", record.rmid,
                                NULL != ser_xid ? ser_xid : ""));
                    break;
                case XA_OK:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    csr->common.xa_s_state = XA_STATE_S2;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    csr->common.xa_s_state = XA_STATE_S4;
                    xa_end_flags = TMFAIL;
                    read_write_rsrmgr--; /* read only transaction */
                    if (commit)
                        tmp_txrc = TX_ROLLBACK;
                    break;                    
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case XAER_RMERR:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    csr->common.xa_s_state = XA_STATE_S4;
                    xa_end_flags = TMFAIL;
                    if (commit)
                        tmp_txrc = TX_ROLLBACK;
                    break;
                case XAER_RMFAIL:
                    *txrc = TX_FAIL;
                    csr->common.xa_r_state = XA_STATE_R0;
                    xa_end_flags = TMFAIL;
                    tmp_txrc = TX_FAIL;
                    break;
                case XAER_NOTA:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    csr->common.xa_s_state = XA_STATE_S4;
                    xa_end_flags = TMFAIL;
                    if (commit)
                        tmp_txrc = TX_ROLLBACK;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    tmp_txrc = TX_FAIL;
                    break;
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.s_state = csr->common.xa_s_state;
            record.td_state = csr->common.xa_td_state;
            g_array_append_val(msg.body.end_8.xa_end_execs, record);
            
            if (lixa_tx_rc_hierarchy(tmp_txrc) <
                lixa_tx_rc_hierarchy(*txrc))
                *txrc = tmp_txrc;
        } /* for (i=0; ...) */
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer)-1, &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR1);
        /* release memory associated to the array */
        g_array_free(msg.body.end_8.xa_end_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_end: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_END_1,
                   client_status_get_crash_count(cs));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes))) {
            client_status_check_socket(cs, ret_cod);
            THROW(MSG_RETRIEVE_ERROR);
        }
        LIXA_TRACE(("lixa_xa_end: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_END_2,
                   client_status_get_crash_count(cs));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif
        /* check the answer from the server */
        if (LIXA_RC_OK != (ret_cod = msg.body.end_16.answer.rc))
            THROW(ERROR_FROM_SERVER);

        if (TX_OK != *txrc)
            THROW(XA_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR1:
            case MSG_SEND_ERROR:
                break;
                break;
            case MSG_RETRIEVE_ERROR:
            case MSG_DESERIALIZE_ERROR:
                break;
            case ERROR_FROM_SERVER:
                ret_cod += LIXA_RC_ERROR_FROM_SERVER_OFFSET;
                break;
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case XA_ERROR:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        
        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        if (NULL != msg.body.end_8.xa_end_execs)
            g_array_free(msg.body.end_8.xa_end_execs, TRUE);
        
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_forget(client_status_t *cs, int finished)
{
    enum Exception { INTERNAL_ERROR
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    lixa_ser_xid_t ser_xid = "";
    struct lixa_msg_s msg;

    msg.body.forget_8.xa_forget_execs = NULL;
    LIXA_TRACE(("lixa_xa_forget\n"));
    TRY {
        size_t buffer_size = 0;
        int fd;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_FORGET;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.forget_8.conthr.finished = finished;
        msg.body.forget_8.xa_forget_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_forget_8_xa_forget_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_forget_8_xa_forget_execs_s record;
            record.rmid = i;
            if (XA_STATE_S5 == csr->common.xa_s_state) {
                LIXA_TRACE(("lixa_xa_forget: resource manager # %d is in "
                            "'Heustically Completed' state, calling "
                            "xa_forget()...\n", record.rmid));
                record.flags = TMNOFLAGS;
                record.rc = act_rsrmgr->xa_switch->xa_forget_entry(
                    client_status_get_xid(cs), record.rmid, record.flags);
                LIXA_TRACE(("lixa_xa_forget: xa_forget_entry('%s', %d, 0x%lx) "
                            "= %d\n",
                            NULL != ser_xid ? ser_xid : "",
                            record.rmid, record.flags, record.rc));
                switch (record.rc) {
                    case XA_OK:
                        csr->common.xa_s_state = XA_STATE_S0;
                        break;
                    case XAER_ASYNC:
                        syslog(LOG_WARNING, LIXA_SYSLOG_LXC020W,
                               (char *)act_rsrmgr->generic->name, record.rmid,
                               NULL != ser_xid ? ser_xid : "");
                        LIXA_TRACE(("lixa_xa_forget: xa_forget returned "
                                    "XAER_ASYNC for rmid=%d,xid='%s' but "
                                    "TMASYNC flag was not set\n",
                                    record.rmid,
                                    NULL != ser_xid ? ser_xid : ""));
                        xa_rc = record.rc;
                        break;
                    case XAER_RMERR:
                        msg.body.forget_8.conthr.finished = FALSE;
                        syslog(LOG_NOTICE, LIXA_SYSLOG_LXC021N,
                               (char *)act_rsrmgr->generic->name, record.rmid,
                               NULL != ser_xid ? ser_xid : "");
                        LIXA_TRACE(("lixa_xa_forget: resource manager # %d "
                                    "is not able to forget this xid\n",
                                    record.rmid));
                        xa_rc = record.rc;
                        break;
                    case XAER_RMFAIL:
                        msg.body.forget_8.conthr.finished = FALSE;
                        LIXA_TRACE(("lixa_xa_forget: resource manager # %d "
                                    "returned %d and is unavailable\n",
                                    record.rmid));
                        xa_rc = record.rc;
                        break;
                    case XAER_NOTA:
                        csr->common.xa_s_state = XA_STATE_S0;
                        syslog(LOG_NOTICE, LIXA_SYSLOG_LXC022N,
                               (char *)act_rsrmgr->generic->name, record.rmid,
                               NULL != ser_xid ? ser_xid : "");
                        LIXA_TRACE(("lixa_xa_forget: resource manager # %d "
                                    "does not recognize as 'Heuristically "
                                    "Completed' the transaction...\n",
                                    record.rmid));
                        xa_rc = record.rc;
                        break;
                    case XAER_INVAL:
                    case XAER_PROTO:
                        xa_rc = record.rc;
                        break;
                    default:
                        THROW(INTERNAL_ERROR);
                }
            record.s_state = csr->common.xa_s_state;
            g_array_append_val(msg.body.forget_8.xa_forget_execs, record);
            } /* if (XA_STATE_S5 == csr->common.xa_s_state) */
        } /* for i */

        /* send the message only if there are records to send */
        if (0 < msg.body.forget_8.xa_forget_execs->len) {
            if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                                   &msg, buffer, sizeof(buffer),
                                   &buffer_size)))
                THROW(MSG_SERIALIZE_ERROR);
            LIXA_TRACE(("lixa_xa_forget: sending " SIZE_T_FORMAT
                        " bytes to the server for step 8\n", buffer_size));
            if (LIXA_RC_OK != (ret_cod =
                               lixa_msg_send(fd, buffer, buffer_size))) {
                if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                    client_status_set_sockfd(cs, LIXA_NULL_FD);
                THROW(MSG_SEND_ERROR);
            }

            LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_FORGET_1,
                       client_status_get_crash_count(cs));
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
                break;
            case NONE:
                switch (xa_rc) {
                    case XA_OK:
                    case XAER_RMERR:
                    case XAER_NOTA:
                        ret_cod = LIXA_RC_OK;
                        break;
                    case XAER_ASYNC:
                        ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                        break;
                    case XAER_RMFAIL:
                        ret_cod = LIXA_RC_XA_ERROR;
                        break;
                    default:
                        ret_cod = LIXA_RC_INTERNAL_ERROR;
                        break;
                }
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        if (NULL != msg.body.forget_8.xa_forget_execs)
            g_array_free(msg.body.forget_8.xa_forget_execs, TRUE);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_forget/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_open(client_status_t *cs, int *txrc, int next_txstate, int mmode)
{
    enum Exception { OBJ_CORRUPTED
                     , MSG_SERIALIZE_ERROR1
                     , MSG_SEND_ERROR1
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , MSG_SERIALIZE_ERROR2
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SEND_ERROR2
                     , XA_ERROR
                     , CLIENT_RECOVERY_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct lixa_msg_s msg;
    
    msg.body.open_24.xa_open_execs = NULL;
    LIXA_TRACE(("lixa_xa_open\n"));
    TRY {
        struct lixa_msg_body_open_8_client_s client;
        int fd;
        size_t buffer_size = 0;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        int recovery_pending = FALSE;

        /* check the resouce managers status is OK */
        if (cs->rmstatus->len == 0) {
            /* popolate the array... */
            for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
                struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                    global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
                int dynamic = act_rsrmgr->xa_switch->flags & TMREGISTER;
                struct client_status_rsrmgr_s csr;
                client_status_rsrmgr_init(&csr, dynamic);
                g_array_append_val(cs->rmstatus, csr);
            }
        } else if (cs->rmstatus->len == global_ccc.actconf.rsrmgrs->len) {
            /* reset the array values */
            for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
                struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                    global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
                int dynamic = act_rsrmgr->xa_switch->flags & TMREGISTER;
                struct client_status_rsrmgr_s *csr = &g_array_index(
                    cs->rmstatus, struct client_status_rsrmgr_s, i);
                client_status_rsrmgr_init(csr, dynamic);
            }            
        } else {
            LIXA_TRACE(("lixa_xa_open: cs->rmstatus->len = %u, "
                        "global_ccc.actconf.rsrmgrs->len = %u\n",
                        cs->rmstatus->len,
                        global_ccc.actconf.rsrmgrs->len));
            THROW(OBJ_CORRUPTED);
        }

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);
        LIXA_TRACE(("lixa_xa_open: fd = %d\n", fd));
        
        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_OPEN;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;
        
        client.job = (xmlChar *)lixa_job_get_raw(global_ccc.job);
        strncpy(client.config_digest,
                global_ccc.config_digest, sizeof(md5_digest_hex_t));
        client.config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
        client.maint = mmode;
        msg.body.open_8.client = client;
        
        msg.body.open_8.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_open_8_rsrmgr_s),
            global_ccc.actconf.rsrmgrs->len);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_open_8_rsrmgr_s record;
            record.rmid = i;
            record.dynamic = act_rsrmgr->xa_switch->flags & TMREGISTER ?
                1 : 0;
            record.name = act_rsrmgr->generic->name;
                record.xa_name = (xmlChar *)act_rsrmgr->xa_switch->name;
                g_array_append_val(msg.body.open_8.rsrmgrs, record);
        }
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer)-1,
                               &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR1);
        
        /* this object contains a lot of references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.open_8.rsrmgrs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes ('%s') to the server for step 8 (socket fd %d)\n",
                    buffer_size, buffer, fd));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR1);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_OPEN_1,
                   client_status_get_crash_count(cs));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes))) {
            client_status_check_socket(cs, ret_cod);
            THROW(MSG_RETRIEVE_ERROR);
        }
        LIXA_TRACE(("lixa_xa_open: received %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_OPEN_2,
                   client_status_get_crash_count(cs));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif
        /* check the answer from the server */        
        if (LIXA_RC_RECOVERY_PENDING_TX  == (
                ret_cod = msg.body.open_16.answer.rc)) {
            recovery_pending = TRUE;
            LIXA_TRACE(("lixa_xa_open: the server replied RECOVERY "
                        "PENDING condition\n"));
        } else if (LIXA_RC_OK != ret_cod)
            THROW(ERROR_FROM_SERVER);
        
        /* prepare the next message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_OPEN;
        msg.header.pvs.step = 24;
        msg.body.open_24.conthr.txstate = next_txstate;
        msg.body.open_24.xa_open_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_open_24_xa_open_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_open function */
        *txrc = TX_OK;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_open_24_xa_open_execs_s record;
            long xa_open_flags = TMNOFLAGS;
            int tmp_txrc = TX_OK;

            record.xa_info = (xmlChar *)act_rsrmgr->generic->xa_open_info;
            record.rmid = i;
            record.flags = xa_open_flags;
            record.rc = act_rsrmgr->xa_switch->xa_open_entry(
                (char *)record.xa_info, record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_open: xa_open_entry('%s', %d, 0x%lx) = %d\n",
                        (char *)record.xa_info, record.rmid, record.flags,
                        record.rc));

            /* compute TX_*; the implemented behaviour is the most restrictive;
               see RFNF 2907143 https://sourceforge.net/tracker/?func=detail&aid=2907143&group_id=257602&atid=1231748 */
            switch (record.rc) {
                case XA_OK:
                    csr->common.xa_r_state = XA_STATE_R1;
                    break;
                case XAER_RMERR:
                    tmp_txrc = TX_ERROR;
                    csr->common.xa_r_state = XA_STATE_R1;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    tmp_txrc = TX_FAIL;
                    csr->common.xa_r_state = XA_STATE_R1;
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.r_state = csr->common.xa_r_state;
            g_array_append_val(msg.body.open_24.xa_open_execs, record);
            
            if (lixa_tx_rc_hierarchy(tmp_txrc) <
                lixa_tx_rc_hierarchy(*txrc))
                *txrc = tmp_txrc;
        } /* for (i=0; ...) */

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR2);
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes to the server for step 24\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR2);
        }

        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_OPEN_3,
                   client_status_get_crash_count(cs));
        
        if (TX_OK != *txrc) {
            int txrc2, rc2;
            LIXA_TRACE(("lixa_xa_open: performing xa_close on all the "
                        "resource managers because tx_open is not TX_OK\n"));
            rc2 = lixa_xa_close(cs, &txrc2);
            LIXA_TRACE(("lixa_xa_open/lixa_xa_close: rc=%d, txrx=%d\n",
                        rc2, txrc2));
            THROW(XA_ERROR);
        }
        
        /* manage recovery pending phase (see doc/seq_diagr.txt) */
        if (recovery_pending &&
            LIXA_RC_OK != (ret_cod = client_recovery(cs, &client))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod) {
                /* the server probably crashed */
                syslog(LOG_NOTICE, LIXA_SYSLOG_LXC028N);
            }
            *txrc = TX_FAIL;
            THROW(CLIENT_RECOVERY_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case MSG_SERIALIZE_ERROR1:
            case MSG_SEND_ERROR1:
                break;
            case MSG_RETRIEVE_ERROR:
            case MSG_DESERIALIZE_ERROR:
                break;
            case ERROR_FROM_SERVER:
                ret_cod += LIXA_RC_ERROR_FROM_SERVER_OFFSET;
                break;
            case MSG_SERIALIZE_ERROR2:
                break;
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SEND_ERROR2:
                break;
            case XA_ERROR:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case CLIENT_RECOVERY_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        
        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        if (NULL != msg.body.open_24.xa_open_execs)
            g_array_free(msg.body.open_24.xa_open_execs, TRUE);
        
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_prepare(client_status_t *cs, int *txrc, int *commit)
{
    enum Exception { ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct lixa_msg_s msg; 
    
    msg.body.prepare_8.xa_prepare_execs = NULL;
    LIXA_TRACE(("lixa_xa_prepare\n"));
    TRY {
        size_t buffer_size = 0;
        int fd, break_prepare;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_PREPARE;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        *commit = TRUE;
        
        msg.body.prepare_8.xa_prepare_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_prepare_8_xa_prepare_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_prepare function */
        *txrc = TX_OK;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_prepare_8_xa_prepare_execs_s record;
            int tmp_txrc = TX_OK;

            /* reset the value because some R.M. will not be xa_prepared */
            csr->prepare_rc = XA_OK;
            
            /* dynamic registered resource managers with unregistered state
               must be bypassed */
            if (csr->common.dynamic && csr->common.xa_s_state != XA_STATE_S2) {
                LIXA_TRACE(("lixa_xa_prepare: resource manager # %u has not "
                            "dynamically registered & ended its "
                            "partecipation, skipping...\n", i));
                continue;
            }
            
            record.rmid = i;
            record.flags = TMNOFLAGS;
            record.rc = csr->prepare_rc =
                act_rsrmgr->xa_switch->xa_prepare_entry(
                    client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_prepare: xa_prepare_entry(xid, %d, 0x%lx) = "
                        "%d\n", record.rmid, record.flags, record.rc));

            break_prepare = TRUE;
            switch (record.rc) {
                case XA_OK:
                    csr->common.xa_s_state = XA_STATE_S3;
                    break_prepare = FALSE;
                    break;
                case XA_RDONLY:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break_prepare = FALSE;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->common.xa_s_state = XA_STATE_S0;
                    tmp_txrc = TX_ROLLBACK;
                    break;                    
                case XAER_NOTA:
                    csr->common.xa_s_state = XA_STATE_S0;
                    tmp_txrc = TX_ROLLBACK;
                    break;
                case XAER_RMERR:
                    csr->common.xa_s_state = XA_STATE_S2;
                    tmp_txrc = TX_ROLLBACK;
                    break;
                case XAER_RMFAIL:
                    csr->common.xa_r_state = XA_STATE_R0;
                    tmp_txrc = TX_FAIL;
                    break;
                case XAER_INVAL:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    tmp_txrc = TX_FAIL;
                    break;
                case XAER_PROTO:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    tmp_txrc = TX_ROLLBACK;
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.s_state = csr->common.xa_s_state;
            record.td_state = csr->common.xa_td_state;
            g_array_append_val(msg.body.prepare_8.xa_prepare_execs, record);

            if (lixa_tx_rc_hierarchy(tmp_txrc) <
                lixa_tx_rc_hierarchy(*txrc))
                *txrc = tmp_txrc;
            
            if (break_prepare) {
                /* interrupt first phase commit, we must rollback the global
                   transaction */
                *commit = FALSE;
                break;
            }
        } /* for (i=0; ...) */
        
        msg.body.prepare_8.conthr.commit = *commit;
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        LIXA_TRACE(("lixa_xa_prepare: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }

        LIXA_CRASH(LIXA_CRASH_POINT_PREPARE_1,
                   client_status_get_crash_count(cs));
        
        /* memory clean-up */
        g_array_free(msg.body.prepare_8.xa_prepare_execs, TRUE);
        msg.body.prepare_8.xa_prepare_execs = NULL;
        
        /* wait server answer */
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes))) {
            client_status_check_socket(cs, ret_cod);
            THROW(MSG_RETRIEVE_ERROR);
        }
        LIXA_TRACE(("lixa_xa_prepare: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif
        /* check the answer from the server */
        if (LIXA_RC_OK != (ret_cod = msg.body.prepare_16.answer.rc))
            THROW(ERROR_FROM_SERVER);

        LIXA_CRASH(LIXA_CRASH_POINT_PREPARE_2,
                   client_status_get_crash_count(cs));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
            case MSG_RETRIEVE_ERROR:
            case MSG_DESERIALIZE_ERROR:
                break;
            case ERROR_FROM_SERVER:
                ret_cod += LIXA_RC_ERROR_FROM_SERVER_OFFSET;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        if (NULL != msg.body.prepare_8.xa_prepare_execs)
            g_array_free(msg.body.prepare_8.xa_prepare_execs, TRUE);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_rollback(client_status_t *cs, int *txrc, int tx_commit)
{
    enum Exception { TX_RC_ADD_ERROR1
                     , TX_RC_ADD_ERROR2
                     , TX_RC_ADD_ERROR3
                     , TX_RC_ADD_ERROR4
                     , TX_RC_ADD_ERROR5
                     , TX_RC_ADD_ERROR6
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SERIALIZE_ERROR
                     , MSG_SEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct lixa_msg_s msg; 
    lixa_tx_rc_t ltr = LIXA_TX_RC_T_INIT;
    *txrc = TX_FAIL;
    msg.body.rollback_8.xa_rollback_execs = NULL;
    
    LIXA_TRACE(("lixa_xa_rollback\n"));
    TRY {
        size_t buffer_size = 0;
        int fd;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        lixa_ser_xid_t ser_xid = "";

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_ROLLBACK;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.rollback_8.conthr.finished = TRUE;
        msg.body.rollback_8.xa_rollback_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_rollback_8_xa_rollback_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* prepare the object to compute *txrc */
        lixa_tx_rc_create(&ltr, tx_commit, FALSE,
                          global_ccc.actconf.rsrmgrs->len);

        /* loop on all the resource managers and call xa_rollback function */
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_rollback_8_xa_rollback_execs_s record;
            record.rmid = i;

            /* bypass resource managers returned XA_RDONLY at xa_prepare()
               level */
            if (csr->common.xa_s_state == XA_STATE_S0 &&
                csr->prepare_rc == XA_RDONLY) {
                LIXA_TRACE(("lixa_xa_rollback: resource manager # %i "
                            "(xa_s_state=%d) returned %d (XA_RDONLY) at "
                            "xa_prepare() call, skipping...\n",
                            record.rmid, csr->common.xa_s_state,
                            csr->prepare_rc));
                if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(&ltr, XA_OK)))
                    THROW(TX_RC_ADD_ERROR1);
                continue;
            }
            
            /* bypass resource managers returned XAER_RMFAIL at
               xa_prepare() level */
            if (csr->common.xa_r_state == XA_STATE_R0 &&
                csr->prepare_rc == XAER_RMFAIL) {
                LIXA_TRACE(("lixa_xa_rollback: resource manager # %i "
                            "(xa_r_state=%d) returned %d (XAER_RMFAIL) at "
                            "xa_prepare() call, skipping...\n",
                            record.rmid, csr->common.xa_r_state,
                            csr->prepare_rc));
                if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(
                                       &ltr, csr->prepare_rc)))
                    THROW(TX_RC_ADD_ERROR2);
                continue;
            }
            
            /* bypass resource managers rolled back at xa_prepare() level */
            if (csr->common.xa_s_state == XA_STATE_S0 &&
                csr->prepare_rc >= XA_RBBASE && csr->prepare_rc <= XA_RBEND) {
                LIXA_TRACE(("lixa_xa_rollback: resource manager # %i "
                            "(xa_s_state=%d) returned %d (XA_RB*) at "
                            "xa_prepare() call, skipping...\n",
                            record.rmid, csr->common.xa_s_state,
                            csr->prepare_rc));
                if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(
                                       &ltr, csr->prepare_rc)))
                    THROW(TX_RC_ADD_ERROR3);
                continue;
            }
            
            /* bypass resource managers returned XAER_NOTA at
               xa_prepare() level */
            if (csr->common.xa_s_state == XA_STATE_S0 &&
                csr->prepare_rc == XAER_NOTA) {
                LIXA_TRACE(("lixa_xa_rollback: resource manager # %i "
                            "(xa_s_state=%d) returned %d (XAER_NOTA) at "
                            "xa_prepare() call, skipping...\n",
                            record.rmid, csr->common.xa_s_state,
                            csr->prepare_rc));
                lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
                syslog(LOG_WARNING, LIXA_SYSLOG_LXC023W,
                       (char *)act_rsrmgr->generic->name, record.rmid,
                       NULL != ser_xid ? ser_xid : "");
                if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(
                                       &ltr, csr->prepare_rc)))
                    THROW(TX_RC_ADD_ERROR4);
                continue;
            }
            
            /* bypass resource managers have not dynamically registered and
               statically ended the transaction */
            if (csr->common.dynamic && csr->common.xa_s_state != XA_STATE_S2 &&
                csr->common.xa_s_state != XA_STATE_S3 &&
                csr->common.xa_s_state != XA_STATE_S4) {
                LIXA_TRACE(("lixa_xa_rollback: resource manager # %i "
                            "(xa_s_state=%d) "
                            "has not yet dynamically registered and/or "
                            "statically ended/prepared, skipping...\n",
                            record.rmid, csr->common.xa_s_state));
                if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(
                                       &ltr, csr->prepare_rc)))
                    THROW(TX_RC_ADD_ERROR5);
                continue;
            }

            record.flags = TMNOFLAGS;
            record.rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_rollback: xa_rollback_entry(xid, %d, 0x%lx) "
                        "= %d\n", record.rmid, record.flags, record.rc));
            if (XA_RETRY == record.rc) {
                sleep(1); /* this is a critical choice */
                LIXA_TRACE(("lixa_xa_rollback: XA_RETRY, trying again..."));
                record.rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                    client_status_get_xid(cs), record.rmid, record.flags);
                LIXA_TRACE(("lixa_xa_rollback: xa_rollback_entry("
                            "xid, %d, 0x%lx) = %d\n", record.rmid,
                            record.flags, record.rc));
            }

            /* force a different return code if xa_prepare failed; see
               TX (Transaction Demarcation) Specification page 68, note 3 */
            if (tx_commit &&
                XA_OK != record.rc && XA_HEURRB != record.rc &&
                XAER_RMFAIL != record.rc && XA_HEURCOM != record.rc &&
                XA_HEURHAZ != record. rc && XA_HEURMIX != record.rc &&
                (XA_RBBASE > record.rc || XA_RBEND < record.rc) &&
                (XAER_RMERR == csr->prepare_rc ||
                 XAER_PROTO == csr->prepare_rc)) {
                LIXA_TRACE(("lixa_xa_rollback: tx_commit=%d, record.rc=%d, "
                            "csr->prepare_rc=%d, forcing LIXA_XAER_HAZARD "
                            "rollback\n", tx_commit, record.rc,
                            csr->prepare_rc));
                lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
                syslog(LOG_WARNING, LIXA_SYSLOG_LXC016W,
                       (char *)act_rsrmgr->generic->name, record.rmid,
                       csr->prepare_rc, record.rc,
                       NULL != ser_xid ? ser_xid : "");
                /* force the return code of xa_rollback() to a different one */
                record.rc = LIXA_XAER_HAZARD;
            }
            
            if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(&ltr, record.rc)))
                THROW(TX_RC_ADD_ERROR6);
            
            switch (record.rc) {
                case XA_HEURHAZ:
                case XA_HEURCOM:
                case XA_HEURRB:
                case XA_HEURMIX:
                    csr->common.xa_s_state = XA_STATE_S5;
                    msg.body.rollback_8.conthr.finished = FALSE;
                    break;
                case XA_OK:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;                    
                case XAER_ASYNC:
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case LIXA_XAER_HAZARD: /* keeping the same state changed by
                                          xa_prepare() */
                    break;
                case XAER_RMERR:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;
                case XA_RETRY: /* state does not change, this will become a
                                  recovery pending transaction */
                    break;
                case XAER_RMFAIL:
                    csr->common.xa_r_state = XA_STATE_R0;
                    break;
                case XAER_NOTA:
                    csr->common.xa_s_state = XA_STATE_S0;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    csr->common.xa_td_state =
                        csr->common.dynamic ? XA_STATE_D0 : XA_STATE_T0;
                    break;
                default:
                    THROW(UNEXPECTED_XA_RC);
            }
            record.r_state = csr->common.xa_r_state;
            record.s_state = csr->common.xa_s_state;
            g_array_append_val(msg.body.rollback_8.xa_rollback_execs, record);
        } /* for (i=0; ...) */
        
        *txrc = lixa_tx_rc_get(&ltr);

        if (TX_MIXED == *txrc || TX_HAZARD == *txrc) {
            lixa_xid_serialize(client_status_get_xid(cs), ser_xid);
            syslog(LOG_WARNING, LIXA_SYSLOG_LXC012W,
                   NULL != ser_xid ? ser_xid : "",
                   TX_MIXED == *txrc ? "TX_MIXED" : "TX_HAZARD");
        }
        
        switch (*txrc) {
            case TX_OK:
            case TX_OUTSIDE:
            case TX_ROLLBACK:
            case TX_COMMITTED:
            case TX_MIXED:
            case TX_HAZARD:
                break;
            default:
                msg.body.rollback_8.conthr.finished = FALSE;
        }
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        LIXA_TRACE(("lixa_xa_rollback: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_ROLLBACK_1,
                   client_status_get_crash_count(cs));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case TX_RC_ADD_ERROR1:
            case TX_RC_ADD_ERROR2:
            case TX_RC_ADD_ERROR3:
            case TX_RC_ADD_ERROR4:
            case TX_RC_ADD_ERROR5:
            case TX_RC_ADD_ERROR6:
                break;
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SERIALIZE_ERROR:
            case MSG_SEND_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release memory */
        lixa_tx_rc_delete(&ltr);
        
        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        if (NULL != msg.body.rollback_8.xa_rollback_execs)
            g_array_free(msg.body.rollback_8.xa_rollback_execs, TRUE);
        
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_start(client_status_t *cs, int *txrc, XID *xid, int txstate,
                  int next_txstate, int *dupid_or_proto)
{
    enum Exception { MSG_SERIALIZE_ERROR1
                     , MSG_SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , MSG_SERIALIZE_ERROR2
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SEND_ERROR2
                     , XA_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct lixa_msg_s msg; 
    
    LIXA_TRACE(("lixa_xa_start\n"));
    TRY {
        size_t buffer_size = 0;
        guint i;
        int fd;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_START;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        memcpy(&msg.body.start_8.conthr.xid, xid, sizeof(XID));
        msg.body.start_8.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_start_8_rsrmgr_s),
            global_ccc.actconf.rsrmgrs->len);
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_open_8_rsrmgr_s record;
            /* if resource manager supports dynamic registration, xa_start
               must not be performed */
            if (act_rsrmgr->xa_switch->flags & TMREGISTER) {
                LIXA_TRACE(("lixa_xa_start: resource manager # %d registers "
                            "dynamically, skipping...\n", i));
                continue;
            }
            record.rmid = i;
            g_array_append_val(msg.body.start_8.rsrmgrs, record);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer)-1, &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR1);

        /* only the GArray needs to be released to avoid memory leaks */
        g_array_free(msg.body.start_8.rsrmgrs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_start: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR);
        }
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_START_1,
                   client_status_get_crash_count(cs));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes))) {
            client_status_check_socket(cs, ret_cod);
            THROW(MSG_RETRIEVE_ERROR);
        }
        LIXA_TRACE(("lixa_xa_start: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_START_2,
                   client_status_get_crash_count(cs));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif
        /* check the answer from the server */
        if (LIXA_RC_OK != (ret_cod = msg.body.start_16.answer.rc))
            THROW(ERROR_FROM_SERVER);

        /* prepare the next message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_START;
        msg.header.pvs.step = 24;
        msg.body.start_24.xa_start_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_start_24_xa_start_execs_s),
            global_ccc.actconf.rsrmgrs->len);

        /* loop on all the resource managers and call xa_start function */
        *txrc = TX_OK;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus, struct client_status_rsrmgr_s, i);
            struct lixa_msg_body_start_24_xa_start_execs_s record;
            long xa_start_flags = TMNOFLAGS;
            int rc;
            int tmp_txrc = TX_OK;
            lixa_ser_xid_t ser_xid = "";

            /* if resource manager supports dynamic registration, xa_start
               must not be performed */
            if (act_rsrmgr->xa_switch->flags & TMREGISTER) {
                LIXA_TRACE(("lixa_xa_start: resource manager # %d registers "
                            "dynamically, skipping...\n", i));
                continue;
            }
            
            record.rmid = i;
            record.flags = xa_start_flags;
            record.rc = rc = act_rsrmgr->xa_switch->xa_start_entry(
                xid, record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_start: xa_start_entry(xid, %d, 0x%lx) = %d\n",
                        record.rmid, record.flags, record.rc));

            switch (record.rc) {
                case XA_OK:
                    csr->common.xa_td_state = XA_STATE_T1;
                    csr->common.xa_s_state = XA_STATE_S1;
                    break;
                case XA_RETRY:
                    LIXA_TRACE(("lixa_xa_start: the resource manager returned "
                                "XA_RETRY, but the transaction manager does "
                                "not use TMNOWAIT flag; this is a resource "
                                "manager wrong behavior!\n"));
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXC013W,
                           (char *)act_rsrmgr->generic->name, record.rmid);
                    tmp_txrc = TX_ERROR;
                    csr->common.xa_td_state = XA_STATE_T0;
                    break;
                case XAER_RMERR:
                    tmp_txrc = TX_ERROR;
                    csr->common.xa_td_state = XA_STATE_T0;
                    break;
                case XAER_DUPID:
                    *dupid_or_proto = TRUE;
                    tmp_txrc = TX_ERROR;
                    csr->common.xa_td_state = XA_STATE_T0;
                    lixa_xid_serialize(xid, ser_xid);
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXC009W,
                           (char *)act_rsrmgr->generic->name, record.rmid,
                           NULL != ser_xid ? ser_xid : "");
                    LIXA_TRACE(("lixa_xa_start: xa_start returned XAER_DUPID "
                                "for rmid=%d,xid='%s' and this should NOT "
                                "happen!\n", record.rmid, ser_xid));
                    break;
                case XAER_OUTSIDE:
                    tmp_txrc = TX_OUTSIDE;
                    break;
                case XAER_INVAL:
                    tmp_txrc = TX_FAIL;
                    csr->common.xa_td_state = XA_STATE_T0;
                    break;
                case XAER_PROTO:
                    *dupid_or_proto = TRUE;
                    tmp_txrc = TX_ERROR;
                    csr->common.xa_td_state = XA_STATE_T0;
                    lixa_xid_serialize(xid, ser_xid);
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXC010W,
                           (char *)act_rsrmgr->generic->name, record.rmid,
                           ser_xid);
                    LIXA_TRACE(("lixa_xa_start: xa_start returned XAER_PROTO "
                                "for rmid=%d,xid='%s' and this should NOT "
                                "happen!\n", record.rmid, ser_xid));
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXC014W,
                           (char *)act_rsrmgr->generic->name, record.rmid,
                           record.rc);
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.td_state = csr->common.xa_td_state;
            record.s_state = csr->common.xa_s_state;
            g_array_append_val(msg.body.start_24.xa_start_execs, record);

            if (lixa_tx_rc_hierarchy(tmp_txrc) <
                lixa_tx_rc_hierarchy(*txrc))
                *txrc = tmp_txrc;
        } /* for (i=0; ...) */

        /* txstate will be moved to next state only if xa_start succeded */
        if (TX_OK == *txrc)
            msg.body.start_24.conthr.txstate = next_txstate;
        else
            msg.body.start_24.conthr.txstate = txstate;
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR2);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.start_24.xa_start_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_start: sending " SIZE_T_FORMAT
                    " bytes to the server for step 24\n", buffer_size));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR2);
        }

        LIXA_CRASH(LIXA_CRASH_POINT_LIXA_XA_START_3,
                   client_status_get_crash_count(cs));
        
        if (TX_OK != *txrc)
            THROW(XA_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR1:
            case MSG_SEND_ERROR:
            case MSG_RETRIEVE_ERROR:
            case MSG_DESERIALIZE_ERROR:
                break;
            case ERROR_FROM_SERVER:
                ret_cod += LIXA_RC_ERROR_FROM_SERVER_OFFSET;
                break;
            case MSG_SERIALIZE_ERROR2:
                break;
            case ASYNC_NOT_IMPLEMENTED:
                ret_cod = LIXA_RC_ASYNC_NOT_IMPLEMENTED;
                break;
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MSG_SEND_ERROR2:
                break;
            case XA_ERROR:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* memory recovery */
        if (NULL != msg.body.start_24.xa_start_execs) {
            g_array_free(msg.body.start_24.xa_start_execs, TRUE);
            msg.body.start_24.xa_start_execs = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

