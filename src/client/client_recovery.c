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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
 */
#include <config.h>



#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include <lixa_errors.h>
#include <lixa_xid.h>
/*
#include <lixa_common_status.h>
*/
#include <lixa_xml_msg_deserialize.h>
#include <lixa_xml_msg_serialize.h>
#include <lixa_xml_msg_trace.h>
#include <lixa_syslog.h>
#include <client_recovery.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_RECOVERY



int client_recovery(client_status_t *cs,
                    const struct lixa_msg_body_open_8_client_s *client)
{
    enum Exception { XML_STRDUP_ERROR
                     , MSG_SERIALIZE_ERROR1
                     , MSG_SEND_ERROR1
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , SERIALIZE_ERROR
                     , ABORTED_RECOVERY
                     , ANALYZE_ERROR
                     , COMMIT_ERROR
                     , ROLLBACK_ERROR
                     , MSG_SERIALIZE_ERROR2
                     , MSG_SEND_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct lixa_msg_s rqst, rpl, updt;
    
    LIXA_TRACE(("client_recovery\n"));
    TRY {
        int fd;
        size_t buffer_size = 0;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        int commit;
        lixa_ser_xid_t ser_xid;

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        while (TRUE) { /* exit only if the server tells: no available tx */
            /* initialize messages */
            lixa_msg_init(&rqst);
            lixa_msg_init(&rpl);
            lixa_msg_init(&updt);
            /* build the message */
            rqst.header.level = LIXA_MSG_LEVEL;
            rqst.header.pvs.verb = LIXA_MSG_VERB_QRCVR;
            rqst.header.pvs.step = LIXA_MSG_STEP_INCR;

            if (NULL == (rqst.body.qrcvr_8.client.job =
                         xmlStrdup(client->job)))
                THROW(XML_STRDUP_ERROR);
            strncpy(rqst.body.qrcvr_8.client.config_digest,
                    client->config_digest,
                    sizeof(md5_digest_hex_t));
            rqst.body.qrcvr_8.client.config_digest[MD5_DIGEST_LENGTH * 2] =
                '\0';
        
            if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                                   &rqst, buffer, sizeof(buffer)-1,
                                   &buffer_size)))
                THROW(MSG_SERIALIZE_ERROR1);

            LIXA_TRACE(("client_recovery: sending " SIZE_T_FORMAT
                        " bytes ('%s') to the server for step %d\n",
                        buffer_size, buffer, rqst.header.pvs.step));
            if (LIXA_RC_OK != (ret_cod = lixa_msg_send(
                                   fd, buffer, buffer_size))) {
                if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                    client_status_set_sockfd(cs, LIXA_NULL_FD);
                THROW(MSG_SEND_ERROR1);
            }

            LIXA_CRASH(LIXA_CRASH_POINT_CLIENT_RECOVERY_1,
                       client_status_get_crash_count(cs));
        
            if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                                   fd, buffer, sizeof(buffer)-1,
                                   &read_bytes))) {
                client_status_check_socket(cs, ret_cod);
                THROW(MSG_RETRIEVE_ERROR);
            }
            LIXA_TRACE(("client_recovery: receiving %d"
                        " bytes from the server |%*.*s|\n",
                        read_bytes, read_bytes, read_bytes, buffer));
            
            LIXA_CRASH(LIXA_CRASH_POINT_CLIENT_RECOVERY_2,
                       client_status_get_crash_count(cs));
        
            if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                                   buffer, read_bytes, &rpl)))
                THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
            lixa_msg_trace(&rpl);
#endif
            
            /* check exit condition */
            if (LIXA_RC_OBJ_NOT_FOUND == rpl.body.qrcvr_16.answer.rc) {
                LIXA_TRACE(("client_recovery: the server answered "
                            "LIXA_RC_OBJ_NOT_FOUND; there are no more "
                            "transactions to recover\n"));
                break;
            }
            
            if (!lixa_xid_serialize(
                    &rpl.body.qrcvr_16.client.state.xid, ser_xid))
                THROW(SERIALIZE_ERROR);
        
            /* check config digest */
            if (strncmp(rqst.body.qrcvr_8.client.config_digest,
                        rpl.body.qrcvr_16.client.config_digest,
                        sizeof(md5_digest_hex_t))) {    
                LIXA_TRACE(("client_recovery: current config digest ('%s') "
                            "does not match server stored config digest "
                            "('%s'); this transaction can NOT be recovered "
                            "by this client\n",
                            rpl.body.qrcvr_16.client.config_digest,
                            rqst.body.qrcvr_8.client.config_digest));
                syslog(LOG_ERR, LIXA_SYSLOG_LXC001E, ser_xid,
                       rpl.body.qrcvr_16.client.config_digest,
                       rqst.body.qrcvr_8.client.config_digest);
                THROW(ABORTED_RECOVERY);
            }

            /* check the answer is arrived from the server */
            if (LIXA_RC_OK != (ret_cod = client_recovery_analyze(
                                   cs, &rpl, &commit)))
                THROW(ANALYZE_ERROR);
            LIXA_TRACE(("client_recovery: transaction '%s' must be %s\n",
                        ser_xid, commit ? "committed" : "rolled back"));

            /* build the message */
            updt.header.level = LIXA_MSG_LEVEL;
            updt.header.pvs.verb = LIXA_MSG_VERB_QRCVR;
            updt.header.pvs.step = 3*LIXA_MSG_STEP_INCR;
            updt.body.qrcvr_24.recovery.commit = commit;
            
            if (commit) {
                if (LIXA_RC_OK != (ret_cod = client_recovery_commit(
                                       cs, &rpl, &updt)))
                    THROW(COMMIT_ERROR);
            } else {
                if (LIXA_RC_OK != (ret_cod = client_recovery_rollback(
                                       cs, &rpl, &updt)))
                    THROW(ROLLBACK_ERROR);
            }
            if (updt.body.qrcvr_24.recovery.failed)
                syslog(LOG_WARNING, LIXA_SYSLOG_LXC005W, ser_xid);
            
            if (LIXA_RC_OK != (
                    ret_cod = lixa_msg_serialize(
                        &updt, buffer, sizeof(buffer)-1, &buffer_size)))
                THROW(MSG_SERIALIZE_ERROR2);
            
            LIXA_TRACE(("client_recovery: sending " SIZE_T_FORMAT
                        " bytes ('%s') to the server for step %d\n",
                        buffer_size, buffer, updt.header.pvs.step));
            if (LIXA_RC_OK != (ret_cod =
                               lixa_msg_send(fd, buffer, buffer_size))) {
                if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                    client_status_set_sockfd(cs, LIXA_NULL_FD);
                THROW(MSG_SEND_ERROR2);
            }
                
            LIXA_CRASH(LIXA_CRASH_POINT_CLIENT_RECOVERY_3,
                       client_status_get_crash_count(cs));

            /* release messages */
            lixa_msg_free(&rqst);
            lixa_msg_free(&rpl);
            lixa_msg_free(&updt);
        } /* while (TRUE) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XML_STRDUP_ERROR:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
                break;
            case MSG_SERIALIZE_ERROR1:
            case MSG_SEND_ERROR1:
                break;
            case MSG_RETRIEVE_ERROR:
            case MSG_DESERIALIZE_ERROR:
                break;
            case SERIALIZE_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case ABORTED_RECOVERY:
                ret_cod = LIXA_RC_ABORTED_RECOVERY;
                break;
            case ANALYZE_ERROR:
            case COMMIT_ERROR:
            case ROLLBACK_ERROR:
                break;
            case MSG_SERIALIZE_ERROR2:
            case MSG_SEND_ERROR2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release messages */
        lixa_msg_free(&rqst);
        lixa_msg_free(&rpl);
        lixa_msg_free(&updt);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_recovery_analyze(const client_status_t *cs,
                            struct lixa_msg_s *rpl,
                            int *commit)
{
    enum Exception { DIFFERENT_NUMBER
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_recovery_analyze\n"));
    TRY {
        guint i;
        
        *commit = FALSE;
        /* intention of the client (transaction manager) */
        if (rpl->body.qrcvr_16.client.state.will_commit) {
            int /* all_prepared = TRUE, */ any_prepared = FALSE;
            int only_one = TRUE;
            LIXA_TRACE(("client_recovery_analyze: the TX was committing\n"));
            *commit = TRUE;
            /* check the status of the resource managers */
            for (i=0; i<rpl->body.qrcvr_16.rsrmgrs->len; ++i) {
                struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                    &g_array_index(rpl->body.qrcvr_16.rsrmgrs,
                                   struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
                LIXA_TRACE(("client_recovery_analyze: rmid=%d, r_state=%d, "
                            "s_state=%d, td_state=%d\n",
                            rsrmgr->rmid, rsrmgr->r_state, rsrmgr->s_state,
                            rsrmgr->td_state));
                if (rsrmgr->s_state == XA_STATE_S3)
                    any_prepared = TRUE;
            }
            if (rpl->body.qrcvr_16.rsrmgrs->len > 1)
                only_one = FALSE;
            *commit = any_prepared || only_one;
        }
        /* check resource managers match */
        if (rpl->body.qrcvr_16.rsrmgrs->len !=
            global_ccc.actconf.rsrmgrs->len) {
            LIXA_TRACE(("client_recovery_analyze: the number of resource "
                        "managers does not match (%ud,%ud)\n",
                        rpl->body.qrcvr_16.rsrmgrs->len,
                        global_ccc.actconf.rsrmgrs->len));
            THROW(DIFFERENT_NUMBER);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case DIFFERENT_NUMBER:
                ret_cod = LIXA_RC_RECOVERY_INFO_MISMATCH;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_analyze/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_recovery_commit(const client_status_t *cs,
                           struct lixa_msg_s *rpl,
                           struct lixa_msg_s *updt)
{
    enum Exception { SERIALIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    
    LIXA_TRACE(("client_recovery_commit\n"));
    TRY {
        guint i;
        int failed = FALSE;
        lixa_ser_xid_t ser_xid;
        
        if (!lixa_xid_serialize(
                &rpl->body.qrcvr_16.client.state.xid, ser_xid))
            THROW(SERIALIZE_ERROR);
        LIXA_TRACE(("client_recovery_commit: committing transaction '%s'\n",
                    ser_xid));
        
        updt->body.qrcvr_24.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_qrcvr_24_rsrmgr_s),
            rpl->body.qrcvr_16.rsrmgrs->len);
        /* check the status of the resource managers */
        for (i=0; i<rpl->body.qrcvr_16.rsrmgrs->len; ++i) {
            int rc;
            struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                &g_array_index(rpl->body.qrcvr_16.rsrmgrs,
                               struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
            struct act_rsrmgr_config_s *act_rsrmgr =
                &g_array_index(global_ccc.actconf.rsrmgrs,
                               struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_qrcvr_24_rsrmgr_s record;

            if (XA_STATE_S2 != rsrmgr->s_state &&
                XA_STATE_S3 != rsrmgr->s_state) {
                LIXA_TRACE(("client_recovery_commit: resource manager "
                            "rmid=%d does not need recovery (s_state=%d)\n",
                            rsrmgr->rmid, rsrmgr->s_state));
                continue;
            }
            
            LIXA_TRACE(("client_recovery_commit: xa_commit for rmid=%d, "
                        "name='%s', xa_name='%s'...\n",
                        rsrmgr->rmid, (char *)act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            rc = act_rsrmgr->xa_switch->xa_commit_entry(
                &rpl->body.qrcvr_16.client.state.xid, i,
                rpl->body.qrcvr_16.rsrmgrs->len == 1 ? TMONEPHASE : TMNOFLAGS);
            LIXA_TRACE(("client_recovery_commit: rc=%d\n", rc));
            switch (rc) {
                case XA_OK:
                    break;
                case XA_RDONLY:
                    syslog(LOG_NOTICE, LIXA_SYSLOG_LXC006N,
                           act_rsrmgr->generic->name, rc, ser_xid);
                    break;
                case XAER_NOTA:
                    syslog(LOG_INFO, LIXA_SYSLOG_LXC008I,
                           act_rsrmgr->generic->name, ser_xid);
                    break;
                default:
                    syslog(LOG_CRIT, LIXA_SYSLOG_LXC003C,
                           act_rsrmgr->generic->name, rc, ser_xid);
                    failed = TRUE;
            }
            /* prepare record for server update */
            record.rmid = rsrmgr->rmid;
            record.rc = rc;
            g_array_append_val(updt->body.qrcvr_24.rsrmgrs, record);
        }
        updt->body.qrcvr_24.recovery.failed = failed;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERIALIZE_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



    
int client_recovery_rollback(const client_status_t *cs,
                             struct lixa_msg_s *rpl,
                             struct lixa_msg_s *updt)
{
    enum Exception { SERIALIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_recovery_rollback\n"));
    TRY {
        guint i;
        int failed = FALSE;
        lixa_ser_xid_t ser_xid;
        
        if (!lixa_xid_serialize(
                &rpl->body.qrcvr_16.client.state.xid, ser_xid))
            THROW(SERIALIZE_ERROR);
        LIXA_TRACE(("client_recovery_rollback: rolling back "
                    "transaction '%s'\n", ser_xid));
        
        updt->body.qrcvr_24.rsrmgrs = g_array_sized_new(
            FALSE, FALSE,
            sizeof(struct lixa_msg_body_qrcvr_24_rsrmgr_s),
            rpl->body.qrcvr_16.rsrmgrs->len);
        /* check the status of the resource managers */
        for (i=0; i<rpl->body.qrcvr_16.rsrmgrs->len; ++i) {
            int rc;
            struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                &g_array_index(rpl->body.qrcvr_16.rsrmgrs,
                               struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
            struct act_rsrmgr_config_s *act_rsrmgr =
                &g_array_index(global_ccc.actconf.rsrmgrs,
                               struct act_rsrmgr_config_s, i);
            struct lixa_msg_body_qrcvr_24_rsrmgr_s record;

            /*
            if (XA_STATE_S0 == rsrmgr->s_state &&
                LIXA_MSG_VERB_END == rsrmgr->next_verb) {
                LIXA_TRACE(("client_recovery_rollback: resource manager "
                            "rmid=%d needs recovery (s_state=%d, "
                            "next_state=%d) because xa_prepare could be "
                            "previously issued\n",
                            rsrmgr->rmid, rsrmgr->s_state,
                            rsrmgr->next_verb));
                            } else */
            if (XA_STATE_S2 != rsrmgr->s_state &&
                       XA_STATE_S3 != rsrmgr->s_state &&
                       XA_STATE_S4 != rsrmgr->s_state) {
                LIXA_TRACE(("client_recovery_rollback: resource manager "
                            "rmid=%d does not need recovery (s_state=%d)\n",
                            rsrmgr->rmid, rsrmgr->s_state));
                continue;
            }

            LIXA_TRACE(("client_recovery_rollback: xa_rollback for rmid=%d, "
                        "name='%s', xa_name='%s'...\n",
                        rsrmgr->rmid, (char *)act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                &rpl->body.qrcvr_16.client.state.xid, i,
                rpl->body.qrcvr_16.rsrmgrs->len == 1 ? TMONEPHASE : TMNOFLAGS);
            LIXA_TRACE(("client_recovery_rollback: rc=%d\n", rc));
            switch (rc) {
                case XA_OK:
                    break;
                case XA_RDONLY:
                    syslog(LOG_NOTICE, LIXA_SYSLOG_LXC007N,
                           act_rsrmgr->xa_switch->name, rc, ser_xid);
                    break;
                case XAER_NOTA:
                    syslog(LOG_INFO, LIXA_SYSLOG_LXC018I,
                           act_rsrmgr->generic->name, ser_xid);
                    break;
                default:
                    syslog(LOG_CRIT, LIXA_SYSLOG_LXC004C,
                           act_rsrmgr->xa_switch->name, rc, ser_xid);
                    failed = TRUE;
            }
            /* prepare record for server update */
            record.rmid = rsrmgr->rmid;
            record.rc = rc;
            g_array_append_val(updt->body.qrcvr_24.rsrmgrs, record);
        }
        updt->body.qrcvr_24.recovery.failed = failed;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SERIALIZE_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_recovery_scan(const client_status_t *cs, GTree *crt,
                         int bbqc, int bfic, int utf)
{
    enum Exception { RECOVER_ERROR1
                     , RECOVER_ERROR2
                     , G_ARRAY_NEW
                     , MALLOC_ERROR
                     , RECOVER_ERROR3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_recovery_scan\n"));
    TRY {
        guint i;
        int xa_rc;
        lixa_ser_xid_t ser_xid;
        
        /* scan all the resource managers associated to the current profile */
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            XID xid_array[100];
            int found, first = TRUE;
            int count = sizeof(xid_array)/sizeof(XID);
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            LIXA_TRACE(("client_recovery_scan: rmid=%u, "
                        "lixa_name='%s', xa_name='%s'\n", i,
                        act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            do {
                int j;
                long flags = first ? TMSTARTRSCAN : TMNOFLAGS;
                found = act_rsrmgr->xa_switch->xa_recover_entry(
                    xid_array, count, (int)i, flags);
                first = FALSE;
                LIXA_TRACE(("client_recovery_scan: rmid=%u, found=%d\n",
                            i, found));
                if (found < 0) {
                    syslog(LOG_ERR, LIXA_SYSLOG_LXC024E,
                           act_rsrmgr->xa_switch->name, i, found, flags,
                           count);
                    THROW(RECOVER_ERROR1);
                }
                if (found > count) {
                    syslog(LOG_ERR, LIXA_SYSLOG_LXC025C,
                           act_rsrmgr->xa_switch->name, i, found, flags,
                           count);
                    THROW(RECOVER_ERROR2);
                }
                for (j=0; j<found; ++j) {
                    XID *xid;
                    GArray *node;
#ifdef _TRACE
                    if (lixa_xid_serialize(xid_array+j, ser_xid)) {
                        LIXA_TRACE(("client_recovery_scan: rmid=%u returned "
                                    "xid '%s'\n", i, ser_xid));
                    }
#endif
                    /* check XID format id */
                    if (!bfic && LIXA_XID_FORMAT_ID != xid_array[j].formatID) {
                        LIXA_TRACE(("client_recovery_scan: this transaction "
                                    "has format id 0x%lx instead of 0x%lx "
                                    "and will be discarded\n",
                                    xid_array[j].formatID,
                                    LIXA_XID_FORMAT_ID));
                        continue;
                    }
                    /* check XID branch qualifier */
                    if (!bbqc && !lixa_xid_bqual_is_global(xid_array + j)) {
                        LIXA_TRACE(("client_recovery_scan: the branch "
                                    "qualifier of this transaction does not "
                                    "match current global branch qualifier "
                                    "and will be discarded\n"));
                        continue;
                    }
                    /* look for the xid */
                    if (NULL == (node = (GArray *)g_tree_lookup(
                                     crt, xid_array+j))) {
                        LIXA_TRACE(("client_recovery_scan: creating "
                                    "a new node for this xid\n", ser_xid));
                        /* initialize the array */
                        if (NULL == (node = g_array_new(
                                         FALSE, FALSE, sizeof(int))))
                            THROW(G_ARRAY_NEW);
                        /* create a new xid object */
                        if (NULL == (xid = malloc(sizeof(XID))))
                            THROW(MALLOC_ERROR);
                        memcpy(xid, xid_array+j, sizeof(XID));
                        /* insert the node in the tree */
                        g_tree_insert(crt, xid, node);
                    }
                    /* add the id to the array */
                    g_array_append_val(node, i);
                } /* for (j=0; j<found; ++j) */
            } while (found == count);
            if (utf) {
                /* stop the scan; Oracle XE 10.2 does not like this call,
                   while it is accepted by DB2 Express-C 9.7; it's an optional
                   flag on lixar command line */
                xa_rc = act_rsrmgr->xa_switch->xa_recover_entry(
                    xid_array, 0, (int)i, TMENDRSCAN);
                LIXA_TRACE(("client_recovery_scan: rmid=%u, flag=0x%lx "
                            "(TMENDRSCAN), xa_rc=%d\n", i, TMENDRSCAN, xa_rc));
                if (XA_OK != xa_rc) {
                    THROW(RECOVER_ERROR3);
                }
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RECOVER_ERROR1:
            case RECOVER_ERROR2:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case G_ARRAY_NEW:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;                
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case RECOVER_ERROR3:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_scan/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_recovery_report(const client_status_t *cs, GTree *crt)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_recovery_report\n"));
    TRY {
        guint i;

        printf("\nResource manager list:\n");
        for (i=0; i<global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            printf("rmid=%u, lixa_name='%s', xa_name='%s'\n", i,
                   act_rsrmgr->generic->name, act_rsrmgr->xa_switch->name);
        }

        if (g_tree_nnodes(crt)) {
            printf("\nPrepared and in-doubt transaction list:\n");
            g_tree_foreach(crt, client_recovery_report_foreach,
                           (gpointer *)stdout);
        } else
            printf("\nThere are no prepared and in-doubt transactions to "
                   "list.\n");
        printf("\n");
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_report/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


gboolean client_recovery_report_foreach(gpointer key, gpointer value,
                                         gpointer data)
{
    /* key points to a xid object, value points to an array of resource manager ids, data points to the output stream */
    XID *xid = (XID *)key;
    GArray *rsrmgrs = (GArray *)value;
    FILE *stream = (FILE *)data;
    lixa_ser_xid_t ser_xid;
    guint i;

    if (!lixa_xid_serialize(xid, ser_xid)) {
        LIXA_TRACE(("client_recovery_report_foreach: xid serialization "
                    "error\n"));
        return TRUE;
    }
    fprintf(stream, "xid='%s': ", ser_xid);
    for (i=0; i<rsrmgrs->len; ++i) {
        int *p = &g_array_index(rsrmgrs, int, i);
        fprintf(stream, "rmid=%d ", *p);
    }
    fprintf(stream, "\n");
    
    return FALSE;
}



int clnt_rcvr_xid_compare(gconstpointer a, gconstpointer b, gpointer foo) {
    return lixa_xid_compare((const XID *)a, (const XID *)b);
}



void clnt_rcvr_array_free(gpointer data) {
    g_array_free((GArray *)data, TRUE);
}



int client_recovery_cold_commit(const client_status_t *cs,
                                XID *xid,
                                const GArray *rsrmgrs)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_recovery_cold_commit\n"));
    TRY {
        guint i;

        for (i=0; i<rsrmgrs->len; ++i) {
            int xa_rc;
            const long flags = TMNOFLAGS;
            int *rmid = &g_array_index(rsrmgrs, int, i);
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s,
                *rmid);
            LIXA_TRACE(("client_recovery_cold_commit: rmid=%d, "
                        "lixa_name='%s', xa_name='%s'\n", *rmid,
                        act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            xa_rc = act_rsrmgr->xa_switch->xa_commit_entry(
                xid, *rmid, flags);
            LIXA_TRACE(("client_recovery_cold_commit: "
                        "xa_commit_entry(xid, %d, 0x%lx) = %d\n",
                        *rmid, flags, xa_rc));
            printf("xa_commit --> rmid=%d, lixa_name='%s', xa_name='%s', "
                   "rc=%d\n", *rmid, act_rsrmgr->generic->name,
                   act_rsrmgr->xa_switch->name, xa_rc);
            if (XA_HEURCOM == xa_rc || XA_HEURRB == xa_rc ||
                XA_HEURMIX == xa_rc || XA_HEURHAZ == xa_rc) {
                lixa_ser_xid_t ser_xid = "";
                lixa_xid_serialize(xid, ser_xid);
                LIXA_TRACE(("client_recovery_cold_commit: the resource "
                            "manager returned heuristic completion, calling "
                            "xa_forget...\n"));
                syslog(LOG_CRIT, LIXA_SYSLOG_LXR004W, xa_rc, ser_xid);
                xa_rc = act_rsrmgr->xa_switch->xa_forget_entry(
                    xid, *rmid, flags);
                LIXA_TRACE(("client_recovery_cold_commit: "
                            "xa_forget_entry('%s', %d, 0x%lx) = %d\n",
                            ser_xid, *rmid, flags, xa_rc));
                printf("xa_forget --> rmid=%d, lixa_name='%s', xa_name='%s', "
                       "rc=%d\n", *rmid, act_rsrmgr->generic->name,
                       act_rsrmgr->xa_switch->name, xa_rc);
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_cold_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_recovery_cold_rollback(const client_status_t *cs,
                                  XID *xid,
                                  const GArray *rsrmgrs)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_recovery_cold_rollback\n"));
    TRY {
        guint i;
        
        for (i=0; i<rsrmgrs->len; ++i) {
            int xa_rc;
            const long flags = TMNOFLAGS;
            int *rmid = &g_array_index(rsrmgrs, int, i);
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s,
                *rmid);
            LIXA_TRACE(("client_recovery_cold_rollback: rmid=%d, "
                        "lixa_name='%s', xa_name='%s'\n", *rmid,
                        act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            xa_rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                xid, *rmid, flags);
            LIXA_TRACE(("client_recovery_cold_rollback: "
                        "xa_rollback_entry(xid, %d, 0x%lx) = %d\n",
                        *rmid, flags, xa_rc));
            printf("xa_rollback --> rmid=%d, lixa_name='%s', xa_name='%s', "
                   "rc=%d\n", *rmid, act_rsrmgr->generic->name,
                   act_rsrmgr->xa_switch->name, xa_rc);
            if (XA_HEURCOM == xa_rc || XA_HEURRB == xa_rc ||
                XA_HEURMIX == xa_rc || XA_HEURHAZ == xa_rc) {
                lixa_ser_xid_t ser_xid = "";
                lixa_xid_serialize(xid, ser_xid);
                LIXA_TRACE(("client_recovery_cold_rollback: the resource "
                            "manager returned heuristic completion, calling "
                            "xa_forget...\n"));
                syslog(LOG_CRIT, LIXA_SYSLOG_LXR005W, xa_rc, ser_xid);
                xa_rc = act_rsrmgr->xa_switch->xa_forget_entry(
                    xid, *rmid, flags);
                LIXA_TRACE(("client_recovery_cold_rollback: "
                            "xa_forget_entry('%s', %d, 0x%lx) = %d\n",
                            ser_xid, *rmid, flags, xa_rc));
                printf("xa_forget --> rmid=%d, lixa_name='%s', xa_name='%s', "
                       "rc=%d\n", *rmid, act_rsrmgr->generic->name,
                       act_rsrmgr->xa_switch->name, xa_rc);
            }
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_cold_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

