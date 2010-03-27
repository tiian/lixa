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



#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include <lixa_errors.h>
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
                     , MSG_SERIALIZE_ERROR
                     , SEND_ERROR
                     , MSG_RETRIEVE_ERROR
                     , MSG_DESERIALIZE_ERROR
                     , ABORTED_RECOVERY
                     , ANALYZE_ERROR
                     , COMMIT_ERROR
                     , ROLLBACK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    char *ser_xid = NULL;
    
    LIXA_TRACE(("client_recovery\n"));
    TRY {
        int fd;
        struct lixa_msg_s rqst, rpl;
        size_t buffer_size = 0;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;
        int commit;

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* build the message */
        rqst.header.level = LIXA_MSG_LEVEL;
        rqst.header.pvs.verb = LIXA_MSG_VERB_QRCVR;
        rqst.header.pvs.step = LIXA_MSG_STEP_INCR;

        if (NULL == (rqst.body.qrcvr_8.client.job = xmlStrdup(client->job)))
            THROW(XML_STRDUP_ERROR);
        strncpy(rqst.body.qrcvr_8.client.config_digest, client->config_digest,
                sizeof(md5_digest_hex_t));
        rqst.body.qrcvr_8.client.config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &rqst, buffer, sizeof(buffer)-1, &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        LIXA_TRACE(("client_recovery: sending " SIZE_T_FORMAT
                    " bytes ('%s') to the server for step 8\n",
                    buffer_size, buffer));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);

        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer)-1, &read_bytes)))
            THROW(MSG_RETRIEVE_ERROR);
        LIXA_TRACE(("client_recovery: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &rpl)))
            THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&rpl);
#endif

        ser_xid = xid_serialize(&rpl.body.qrcvr_16.client.state.xid);
        
        /* check config digest */
        if (strncmp(rqst.body.qrcvr_8.client.config_digest,
                    rpl.body.qrcvr_16.client.config_digest,
                    sizeof(md5_digest_hex_t))) {    
            LIXA_TRACE(("client_recovery: current config digest ('%s') does "
                        "not match server stored config digest ('%s'); "
                        "this transaction can NOT be recovered by this "
                        "client\n",
                        rpl.body.qrcvr_16.client.config_digest,
                        rqst.body.qrcvr_8.client.config_digest));
            syslog(LOG_ERR, LIXA_SYSLOG_LXC001E, ser_xid ? ser_xid : "",
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

        if (commit) {
            if (LIXA_RC_OK != (ret_cod = client_recovery_commit(cs, &rpl)))
                THROW(COMMIT_ERROR);
        } else {
            if (LIXA_RC_OK != (ret_cod = client_recovery_rollback(cs, &rpl)))
                THROW(ROLLBACK_ERROR);
        }
            
        /* @@@ query the resource managers with xa_recover ... */
        exit(1);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XML_STRDUP_ERROR:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
                break;
            case MSG_SERIALIZE_ERROR:
                break;
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case MSG_RETRIEVE_ERROR:
            case MSG_DESERIALIZE_ERROR:
                break;
            case ABORTED_RECOVERY:
                ret_cod = LIXA_RC_ABORTED_RECOVERY;
                break;
            case ANALYZE_ERROR:
            case COMMIT_ERROR:
            case ROLLBACK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (ser_xid) free(ser_xid);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_recovery_analyze(const client_status_t *cs,
                            const struct lixa_msg_s *rpl,
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
            int all_prepared = TRUE;
            int only_one = TRUE;
            LIXA_TRACE(("client_recovery_analyze: the TX was committing\n"));
            *commit = TRUE;
            /* check the status of the resource managers */
            for (i=0; i<rpl->body.qrcvr_16.rsrmgrs->len; ++i) {
                struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                    &g_array_index(rpl->body.qrcvr_16.rsrmgrs,
                                   struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
                LIXA_TRACE(("client_recovery_analyze: rmid=%d, r_state=%d, "
                            "s_state=%d, t_state=%d\n",
                            rsrmgr->rmid, rsrmgr->r_state, rsrmgr->s_state,
                            rsrmgr->t_state));
                if (rsrmgr->t_state != XA_STATE_S3)
                    all_prepared = FALSE;
            }
            if (rpl->body.qrcvr_16.rsrmgrs->len > 1)
                only_one = FALSE;
            *commit = all_prepared || only_one;
        }
        /* check resource managers match */
        if (rpl->body.qrcvr_16.rsrmgrs->len !=
            global_ccc.actconf.rsrmgrs->len) {
            LIXA_TRACE(("client_recovery_analyze: the number of resource "
                        "managers do not match (%ud,%ud)\n",
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
                           const struct lixa_msg_s *rpl)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    char *ser_xid = NULL;
    
    LIXA_TRACE(("client_recovery_commit\n"));
    TRY {
        guint i;
        
        ser_xid = xid_serialize(&rpl->body.qrcvr_16.client.state.xid);
        LIXA_TRACE(("client_recovery_commit: committing transaction '%s'\n",
                    ser_xid));
        
        /* check the status of the resource managers */
        for (i=0; i<rpl->body.qrcvr_16.rsrmgrs->len; ++i) {
            int rc;
            struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                &g_array_index(rpl->body.qrcvr_16.rsrmgrs,
                               struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
            struct act_rsrmgr_config_s *act_rsrmgr =
                &g_array_index(global_ccc.actconf.rsrmgrs,
                               struct act_rsrmgr_config_s, i);
            LIXA_TRACE(("client_recovery_commit: xa_commit for rmid=%d, "
                        "name='%s', xa_name='%s'...\n",
                        rsrmgr->rmid, (char *)act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            rc = act_rsrmgr->xa_switch->xa_commit_entry(
                &rpl->body.qrcvr_16.client.state.xid, i,
                rpl->body.qrcvr_16.rsrmgrs->len == 1 ? TMONEPHASE : TMNOFLAGS);
            LIXA_TRACE(("client_recovery_commit: rc=%d\n", rc));
            if (rc)
                syslog(LOG_CRIT, LIXA_SYSLOG_LXC003C,
                       act_rsrmgr->xa_switch->name, rc, ser_xid); 
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
        if (ser_xid) free(ser_xid);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



    
int client_recovery_rollback(const client_status_t *cs,
                             const struct lixa_msg_s *rpl)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    char *ser_xid = NULL;
    
    LIXA_TRACE(("client_recovery_rollback\n"));
    TRY {
        guint i;
        
        ser_xid = xid_serialize(&rpl->body.qrcvr_16.client.state.xid);
        LIXA_TRACE(("client_recovery_rollback: rolling back "
                    "transaction '%s'\n", ser_xid));
        
        /* check the status of the resource managers */
        for (i=0; i<rpl->body.qrcvr_16.rsrmgrs->len; ++i) {
            int rc;
            struct lixa_msg_body_qrcvr_16_rsrmgr_s *rsrmgr =
                &g_array_index(rpl->body.qrcvr_16.rsrmgrs,
                               struct lixa_msg_body_qrcvr_16_rsrmgr_s, i);
            struct act_rsrmgr_config_s *act_rsrmgr =
                &g_array_index(global_ccc.actconf.rsrmgrs,
                               struct act_rsrmgr_config_s, i);
            LIXA_TRACE(("client_recovery_rollback: xa_rollback for rmid=%d, "
                        "name='%s', xa_name='%s'...\n",
                        rsrmgr->rmid, (char *)act_rsrmgr->generic->name,
                        act_rsrmgr->xa_switch->name));
            rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                &rpl->body.qrcvr_16.client.state.xid, i,
                rpl->body.qrcvr_16.rsrmgrs->len == 1 ? TMONEPHASE : TMNOFLAGS);
            LIXA_TRACE(("client_recovery_rollback: rc=%d\n", rc));
            if (rc)
                syslog(LOG_CRIT, LIXA_SYSLOG_LXC004C,
                       act_rsrmgr->xa_switch->name, rc, ser_xid); 
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
        if (ser_xid) free(ser_xid);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_recovery_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



