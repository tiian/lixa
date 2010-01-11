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



#ifdef HAVE_STRING_H
#include <string.h>
#endif



#include <lixa_trace.h>
#include <lixa_errors.h>
#include <lixa_xa.h>
#include <lixa_xml_msg_deserialize.h>
#include <lixa_xml_msg_serialize.h>
#include <lixa_xml_msg_trace.h>
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



int lixa_xa_end(client_status_t *cs, int *txrc, int commit, int *rwrm)
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
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(fd, buffer, buffer_size,
                                                       &read_bytes)))
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
        *rwrm = read_write_rsrmgr;
        
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
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_xa_open\n"));
    TRY {
        struct lixa_msg_s msg;
        int fd;
        size_t buffer_size = 0;
        guint i;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;

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

        msg.body.open_8.client.profile = (xmlChar *)global_ccc.profile;
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

        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(fd, buffer, buffer_size,
                                                       &read_bytes)))
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
        if (LIXA_RC_OK != (ret_cod = msg.body.open_16.answer.rc))
            THROW(ERROR_FROM_SERVER);

        /* prepare the next message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_OPEN;
        msg.header.pvs.step = 24;
        msg.body.open_24.conthr.state = next_txstate;
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
            int rc;

            record.xa_info = (xmlChar *)act_rsrmgr->generic->xa_open_info;
            record.rmid = i;
            record.flags = xa_open_flags;
            record.rc = rc = act_rsrmgr->xa_switch->xa_open_entry(
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
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case MSG_SERIALIZE_ERROR1:
                break;
            case MSG_RETRIEVE_ERROR:
                break;
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
        int fd;
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

            switch (record.rc) {
                case XA_OK:
                    csr->xa_s_state = XA_STATE_S3;
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

            if (XA_OK != record.rc) {
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
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(fd, buffer, buffer_size,
                                                       &read_bytes)))
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
                            "dynamically, skipped...\n", i));
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
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(fd, buffer, buffer_size,
                                                       &read_bytes)))
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
        msg.body.start_24.conthr.state = next_txstate;
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
                            "dynamically, skipped...\n", i));
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

