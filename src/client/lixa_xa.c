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



#ifdef HAVE_STRING_H
#include <string.h>
#endif



#include <lixa_crash.h>
#include <lixa_trace.h>
#include <lixa_errors.h>
#include <lixa_tx_rc.h>
#include <lixa_xa.h>
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
                     , SEND_ERROR
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
                    if (*txrc == TX_OK)
                        *txrc = TX_ERROR;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);                    
            } /* switch (rc) */
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
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);

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
                *txrc = TX_FAIL;
                break;
            case SEND_ERROR:
                *txrc = TX_ERROR;
                ret_cod = LIXA_RC_SEND_ERROR;
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
    enum Exception { TX_RC_ADD_ERROR
                     , INVALID_XA_RC
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , MSG_SERIALIZE_ERROR
                     , SEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    lixa_tx_rc_t ltr = LIXA_TX_RC_T_INIT;
    *txrc = TX_FAIL;
    
    LIXA_TRACE(("lixa_xa_commit\n"));
    TRY {
        struct lixa_msg_s msg; 
        size_t buffer_size = 0;
        int fd;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_COMMIT;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.commit_8.xa_commit_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_commit_8_xa_commit_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        LIXA_TRACE(("lixa_xa_commit: one_phase_commit = %d\n",
                    one_phase_commit));

        /* prepare the object to compute *txrc */
        lixa_tx_rc_create(&ltr, TRUE, TRUE, global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_commit function */
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct common_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstates, struct common_status_rsrmgr_s, i);
            struct lixa_msg_body_commit_8_xa_commit_execs_s record;

            record.rmid = i;
            
            /* bypass resource managers are not prepared */
            if ((one_phase_commit && XA_STATE_S2 != csr->xa_s_state) ||
                (!one_phase_commit && XA_STATE_S3 != csr->xa_s_state)) {
                    LIXA_TRACE(("lixa_xa_commit: rmid = %d, "
                                "one_phase_commit = %d, "
                                "xa_s_state = %d, bypassing...\n",
                                record.rmid, one_phase_commit,
                                csr->xa_s_state));
                    continue;
                }
                    
            record.flags = one_phase_commit ? TMONEPHASE : TMNOFLAGS;
            record.rc = act_rsrmgr->xa_switch->xa_commit_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_commit: xa_commit_entry(xid, %d, 0x%lx) = "
                        "%d\n", record.rmid, record.flags, record.rc));
            if (XA_RETRY == record.rc) {
                /* try a second time */
                LIXA_TRACE(("lixa_xa_commit: XA_RETRY, try again..."));
                record.rc = act_rsrmgr->xa_switch->xa_commit_entry(
                    client_status_get_xid(cs), record.rmid, record.flags);
                LIXA_TRACE(("lixa_xa_commit: xa_commit_entry(xid, %d, 0x%lx) "
                            "= %d\n", record.rmid, record.flags, record.rc));
            }
            
            if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(&ltr, record.rc)))
                THROW(TX_RC_ADD_ERROR);
            
            switch (record.rc) {
                case XA_HEURHAZ:
                case XA_HEURCOM:
                case XA_HEURRB:
                case XA_HEURMIX:
                    csr->xa_s_state = XA_STATE_S5;
                    break;
                case XA_OK:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->xa_s_state = XA_STATE_S0;
                    if (TMONEPHASE != record.flags)
                        THROW(INVALID_XA_RC);
                    break;                    
                case XAER_ASYNC:
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case XAER_RMERR:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XAER_RMFAIL:
                    csr->xa_r_state = XA_STATE_R0;
                    break;
                case XAER_NOTA:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    csr->xa_t_state = XA_STATE_T0;
                    break;
                default:
                    THROW(UNEXPECTED_XA_RC);
            }
            record.r_state = csr->xa_r_state;
            record.s_state = csr->xa_s_state;
            g_array_append_val(msg.body.commit_8.xa_commit_execs, record);
        } /* for (i=0; ...) */

        *txrc = lixa_tx_rc_get(&ltr);
        switch (*txrc) {
            case TX_OK:
            case TX_OUTSIDE:
            case TX_ROLLBACK:
            case TX_COMMITTED:
                msg.body.commit_8.conthr.finished = TRUE;
                break;
            default:
                msg.body.commit_8.conthr.finished = FALSE;
        }
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.commit_8.xa_commit_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_commit: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case TX_RC_ADD_ERROR:
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
        /* release memory */
        lixa_tx_rc_delete(&ltr);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_end(client_status_t *cs, int *txrc, int commit)
{
    enum Exception { MSG_SERIALIZE_ERROR1
                     , SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , MSG_SERIALIZE_ERROR2
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , SEND_ERROR2
                     , XA_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_end\n"));
    TRY {
        struct lixa_msg_s msg; 
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
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer)-1, &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR1);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_end: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes)))
            THROW(MSG_RETRIEVE_ERROR);
        LIXA_TRACE(("lixa_xa_end: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif
        /* check the answer from the server */
        if (LIXA_RC_OK != (ret_cod = msg.body.end_16.answer.rc))
            THROW(ERROR_FROM_SERVER);

        /* prepare the next message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_END;
        msg.header.pvs.step = 24;
        msg.body.end_24.xa_end_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_end_24_xa_end_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_open function */
        *txrc = TX_OK;
        xa_end_flags = TMSUCCESS;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct common_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstates, struct common_status_rsrmgr_s, i);
            struct lixa_msg_body_end_24_xa_end_execs_s record;

            record.rmid = i;
            record.flags = xa_end_flags;
            record.rc = act_rsrmgr->xa_switch->xa_end_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_end: xa_end_entry(xid, %d, 0x%lx) = %d\n",
                        record.rmid, record.flags, record.rc));

            read_write_rsrmgr++;
            switch (record.rc) {
                case XA_OK:
                    csr->xa_t_state = XA_STATE_T0;
                    csr->xa_s_state = XA_STATE_S2;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->xa_t_state = XA_STATE_T0;
                    csr->xa_s_state = XA_STATE_S4;
                    xa_end_flags = TMFAIL;
                    read_write_rsrmgr--; /* read only transaction */
                    break;                    
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case XAER_RMERR:
                    /* @@@ this behavior comes from page 65 of
                       "The TX Specification"; from state table of XA protocol
                       it seems not clear what should be done when there's a
                       problem in dissociation
                       there might be a bug in this place... */
                    csr->xa_t_state = XA_STATE_T0;
                    csr->xa_s_state = XA_STATE_S4;
                    xa_end_flags = TMFAIL;
                    break;
                case XAER_RMFAIL:
                    *txrc = TX_FAIL;
                    csr->xa_r_state = XA_STATE_R0;
                    xa_end_flags = TMFAIL;
                    break;
                case XAER_NOTA:
                    /* @@@ this behavior comes from page 65 of
                       "The TX Specification"; from state table of XA protocol
                       it seems not clear what should be done when there's a
                       problem in dissociation
                       there might be a bug in this place... */
                    csr->xa_t_state = XA_STATE_T0;
                    csr->xa_s_state = XA_STATE_S4;
                    xa_end_flags = TMFAIL;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    csr->xa_t_state = XA_STATE_T0;
                    break;
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.s_state = csr->xa_s_state;
            record.t_state = csr->xa_t_state;
            g_array_append_val(msg.body.end_24.xa_end_execs, record);
        } /* for (i=0; ...) */
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR2);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.end_24.xa_end_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_end: sending " SIZE_T_FORMAT
                    " bytes to the server for step 24\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR2);

        if (TX_OK != *txrc)
            THROW(XA_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR1:
                break;
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
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
            case SEND_ERROR2:
                ret_cod = LIXA_RC_SEND_ERROR;
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
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_open(client_status_t *cs, int *txrc, int next_txstate)
{
    enum Exception { OBJ_CORRUPTED
                     , MSG_SERIALIZE_ERROR1
                     , SEND_ERROR1
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , MSG_SERIALIZE_ERROR2
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , SEND_ERROR2
                     , XA_ERROR
                     , CLIENT_RECOVERY_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_open\n"));
    TRY {
        struct lixa_msg_body_open_8_client_s client;
        struct lixa_msg_s msg;
        int fd;
        size_t buffer_size = 0;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        int recovery_pending = FALSE;

        /* check the resouce managers status is OK */
        if (cs->rmstates->len == 0) {
            /* popolate the array... */
            for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
                struct common_status_rsrmgr_s csr;
                common_status_rsrmgr_init(&csr);
                g_array_append_val(cs->rmstates, csr);
            }
        } else if (cs->rmstates->len == global_ccc.actconf.rsrmgrs->len) {
            /* reset the array values */
            for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
                struct common_status_rsrmgr_s *csr = &g_array_index(
                    cs->rmstates, struct common_status_rsrmgr_s, i);
                common_status_rsrmgr_init(csr);
            }            
        } else {
            LIXA_TRACE(("lixa_xa_open: cs->rmstates->len = %u, "
                        "global_ccc.actconf.rsrmgrs->len = %u\n",
                        cs->rmstates->len, global_ccc.actconf.rsrmgrs->len));
            THROW(OBJ_CORRUPTED);
        }
        
        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_OPEN;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        client.job = (xmlChar *)lixa_job_get_raw(global_ccc.job);
        strncpy(client.config_digest,
                global_ccc.config_digest, sizeof(md5_digest_hex_t));
        client.config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
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
            record.name = act_rsrmgr->generic->name;
            record.xa_name = (xmlChar *)act_rsrmgr->xa_switch->name;
            g_array_append_val(msg.body.open_8.rsrmgrs, record);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer)-1, &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR1);

        /* this object contains a lot of references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.open_8.rsrmgrs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes ('%s') to the server for step 8\n",
                    buffer_size, buffer));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR1);

        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes)))
            THROW(MSG_RETRIEVE_ERROR);
        LIXA_TRACE(("lixa_xa_open: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
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
            struct common_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstates, struct common_status_rsrmgr_s, i);
            struct lixa_msg_body_open_24_xa_open_execs_s record;
            long xa_open_flags = TMNOFLAGS;

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
                    csr->xa_r_state = XA_STATE_R1;
                    break;
                case XAER_RMERR:
                    if (*txrc == TX_OK)
                        *txrc = TX_ERROR;
                    csr->xa_r_state = XA_STATE_R1;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    csr->xa_r_state = XA_STATE_R1;
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.r_state = csr->xa_r_state;
            g_array_append_val(msg.body.open_24.xa_open_execs, record);
        } /* for (i=0; ...) */
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR2);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.open_24.xa_open_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_open: sending " SIZE_T_FORMAT
                    " bytes to the server for step 24\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR2);

        if (TX_OK != *txrc)
            THROW(XA_ERROR);

        /* manage recovery pending phase (see doc/seq_diagr.txt) */
        if (recovery_pending &&
            LIXA_RC_OK != (ret_cod = client_recovery(cs, &client)))
            THROW(CLIENT_RECOVERY_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case MSG_SERIALIZE_ERROR1:
                break;
            case SEND_ERROR1:
                ret_cod = LIXA_RC_SEND_ERROR;
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
            case SEND_ERROR2:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case XA_ERROR:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case CLIENT_RECOVERY_ERROR:
                /* @@@ no way - based on the standard - to report this type
                   of error to the application program: open point for future
                   investigation */
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
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
                     , SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_prepare\n"));
    TRY {
        struct lixa_msg_s msg; 
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
            struct common_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstates, struct common_status_rsrmgr_s, i);
            struct lixa_msg_body_prepare_8_xa_prepare_execs_s record;

            record.rmid = i;
            record.flags = TMNOFLAGS;
            record.rc = act_rsrmgr->xa_switch->xa_prepare_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_prepare: xa_prepare_entry(xid, %d, 0x%lx) = "
                        "%d\n", record.rmid, record.flags, record.rc));

            break_prepare = TRUE;
            switch (record.rc) {
                case XA_OK:
                    csr->xa_s_state = XA_STATE_S3;
                    break_prepare = FALSE;
                    break;
                case XA_RDONLY:
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->xa_s_state = XA_STATE_S0;
                    break_prepare = FALSE;
                    break;                    
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case XAER_RMERR:
                    csr->xa_s_state = XA_STATE_S2;
                    break;
                case XAER_RMFAIL:
                    *txrc = TX_FAIL;
                    csr->xa_r_state = XA_STATE_R0;
                    break;
                case XAER_NOTA:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    csr->xa_t_state = XA_STATE_T0;
                    break;
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.s_state = csr->xa_s_state;
            record.t_state = csr->xa_t_state;
            g_array_append_val(msg.body.prepare_8.xa_prepare_execs, record);

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

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.prepare_8.xa_prepare_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_prepare: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);

        /* wait server answer */
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes)))
            THROW(MSG_RETRIEVE_ERROR);
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

        LIXA_CRASH(LIXA_CRASH_POINT_PREPARE_1,
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
                break;
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
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
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_rollback(client_status_t *cs, int *txrc, int tx_commit)
{
    enum Exception { TX_RC_ADD_ERROR
                    , ASYNC_NOT_IMPLEMENTED
                    , UNEXPECTED_XA_RC
                    , MSG_SERIALIZE_ERROR
                    , SEND_ERROR
                    , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    lixa_tx_rc_t ltr = LIXA_TX_RC_T_INIT;
    *txrc = TX_FAIL;
    
    LIXA_TRACE(("lixa_xa_rollback\n"));
    TRY {
        struct lixa_msg_s msg; 
        size_t buffer_size = 0;
        int fd;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_ROLLBACK;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

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
            struct common_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstates, struct common_status_rsrmgr_s, i);
            struct lixa_msg_body_rollback_8_xa_rollback_execs_s record;

            record.rmid = i;            
            record.flags = TMNOFLAGS;
            record.rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                client_status_get_xid(cs), record.rmid, record.flags);
            LIXA_TRACE(("lixa_xa_rollback: xa_rollback_entry(xid, %d, 0x%lx) "
                        "= %d\n", record.rmid, record.flags, record.rc));
            if (XA_RETRY == record.rc) {
                LIXA_TRACE(("lixa_xa_rollback: XA_RETRY, trying again..."));
                record.rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                    client_status_get_xid(cs), record.rmid, record.flags);
                LIXA_TRACE(("lixa_xa_rollback: xa_rollback_entry("
                            "xid, %d, 0x%lx) = %d\n", record.rmid,
                            record.flags, record.rc));
            }
            
            if (LIXA_RC_OK != (ret_cod = lixa_tx_rc_add(&ltr, record.rc)))
                THROW(TX_RC_ADD_ERROR);
            
            switch (record.rc) {
                case XA_HEURHAZ:
                    csr->xa_s_state = XA_STATE_S5;
                    break;
                case XA_HEURCOM:
                    csr->xa_s_state = XA_STATE_S5;
                    break;
                case XA_HEURRB:
                    csr->xa_s_state = XA_STATE_S5;
                    break;
                case XA_HEURMIX:
                    csr->xa_s_state = XA_STATE_S5;
                    break;
                case XA_OK:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                    csr->xa_s_state = XA_STATE_S0;
                    break;                    
                case XAER_ASYNC:
                    THROW(ASYNC_NOT_IMPLEMENTED);
                case XAER_RMERR:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XAER_RMFAIL:
                    csr->xa_r_state = XA_STATE_R0;
                    break;
                case XAER_NOTA:
                    csr->xa_s_state = XA_STATE_S0;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    csr->xa_t_state = XA_STATE_T0;
                    break;
                default:
                    THROW(UNEXPECTED_XA_RC);
            }
            record.r_state = csr->xa_r_state;
            record.s_state = csr->xa_s_state;
            g_array_append_val(msg.body.rollback_8.xa_rollback_execs, record);
        } /* for (i=0; ...) */
        
        *txrc = lixa_tx_rc_get(&ltr);
        switch (*txrc) {
            case TX_OK:
            case TX_OUTSIDE:
            case TX_ROLLBACK:
            case TX_COMMITTED:
                msg.body.commit_8.conthr.finished = TRUE;
                break;
            default:
                msg.body.commit_8.conthr.finished = FALSE;
        }
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        /* this object contains references to external stuff and
           cannot be freed using standard lixa_msg_free; we are freeing the
           array to avoid memory leaks */
        g_array_free(msg.body.rollback_8.xa_rollback_execs, TRUE);
        memset(&msg, 0, sizeof(msg));
        
        LIXA_TRACE(("lixa_xa_rollback: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);
        
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
        /* release memory */
        lixa_tx_rc_delete(&ltr);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_xa_start(client_status_t *cs, int *txrc, XID *xid, int next_txstate)
{
    enum Exception { MSG_SERIALIZE_ERROR1
                     , SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ERROR_FROM_SERVER
                     , MSG_SERIALIZE_ERROR2
                     , ASYNC_NOT_IMPLEMENTED
                     , UNEXPECTED_XA_RC
                     , SEND_ERROR2
                     , XA_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_start\n"));
    TRY {
        struct lixa_msg_s msg; 
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
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes)))
            THROW(MSG_RETRIEVE_ERROR);
        LIXA_TRACE(("lixa_xa_start: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
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
        msg.body.start_24.conthr.txstate = next_txstate;
        msg.body.start_24.xa_start_execs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_start_24_xa_start_execs_s),
            global_ccc.actconf.rsrmgrs->len);
        
        /* loop on all the resource managers and call xa_start function */
        *txrc = TX_OK;
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct common_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstates, struct common_status_rsrmgr_s, i);
            struct lixa_msg_body_start_24_xa_start_execs_s record;
            long xa_start_flags = TMNOFLAGS;
            int rc;

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
                    csr->xa_t_state = XA_STATE_T1;
                    break;
                case XAER_RMERR:
                    if (*txrc == TX_OK)
                        *txrc = TX_ERROR;
                    csr->xa_t_state = XA_STATE_T0;
                    break;
                case XAER_INVAL:
                case XAER_PROTO:
                    *txrc = TX_FAIL;
                    csr->xa_t_state = XA_STATE_T0;
                    break;
                case XAER_ASYNC:
                    *txrc = TX_FAIL;
                    THROW(ASYNC_NOT_IMPLEMENTED);
                default:
                    *txrc = TX_FAIL;
                    THROW(UNEXPECTED_XA_RC);
            }
            record.t_state = csr->xa_t_state;
            g_array_append_val(msg.body.start_24.xa_start_execs, record);
        } /* for (i=0; ...) */
        
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
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR2);

        if (TX_OK != *txrc)
            THROW(XA_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MSG_SERIALIZE_ERROR1:
                break;
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
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
            case SEND_ERROR2:
                ret_cod = LIXA_RC_SEND_ERROR;
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
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xa_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

