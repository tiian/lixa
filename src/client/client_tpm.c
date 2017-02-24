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

#include <lixa_errors.h>
#include <lixa_xid.h>
#include <lixa_xml_msg_deserialize.h>
#include <lixa_xml_msg_serialize.h>
#include <lixa_xml_msg_trace.h>
#include <lixa_syslog.h>
#include <src/common/lixa_xml_msg.h>

#include "client_tpm.h"

/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_CLIENT_TPM

int client_tpm_trans(client_status_t *cs, GTree *xidt, int maint)
{
    enum Exception
    {
        XML_STRDUP_ERROR,
        MSG_SERIALIZE_ERROR1,
        MSG_SEND_ERROR1,
        MSG_RETRIEVE_ERROR,
        MSG_DESERIALIZE_ERROR,
        SERIALIZE_ERROR,
        ANALYZE_ERROR,
        MSG_SERIALIZE_ERROR2,
        MSG_SEND_ERROR2,
        NO_TRANSACTIONS,
        XID_DESERIALIZE_ERROR,
        G_ARRAY_NEW,
        MALLOC_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct lixa_msg_s msg;

    LIXA_TRACE(("client_tpm_trans\n"));
    TRY {
        int fd;
        size_t buffer_size = 0;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        ssize_t read_bytes;

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);

        /* initialize messages */
        lixa_msg_init(&msg);

        /* build the message */
        struct lixa_msg_body_trans_8_client_s client;
        client.job = (xmlChar *) lixa_job_get_raw(global_ccc.job);
        strncpy(client.config_digest,
                global_ccc.config_digest, sizeof(md5_digest_hex_t));
        client.config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
        client.maint = maint;

        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_TRANS;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        if (NULL ==
            (msg.body.trans_8.client.job = xmlStrdup(client.job))) THROW(
                XML_STRDUP_ERROR);
        strncpy(msg.body.trans_8.client.config_digest, client.config_digest,
                sizeof(md5_digest_hex_t));
        msg.body.trans_8.client.config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
        msg.body.trans_8.client.maint = client.maint;

        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer) - 1,
                               &buffer_size))) THROW(MSG_SERIALIZE_ERROR1);

        LIXA_TRACE(("client_tpm_trans: sending "
                    SIZE_T_FORMAT
                    " bytes ('%s') to the server for step %d\n",
                    buffer_size, buffer, msg.header.pvs.step));
        if (LIXA_RC_OK != (ret_cod = lixa_msg_send(
                               fd, buffer, buffer_size))) {
            if (LIXA_RC_CONNECTION_CLOSED == ret_cod)
                client_status_set_sockfd(cs, LIXA_NULL_FD);
            THROW(MSG_SEND_ERROR1);
        }

        if (LIXA_RC_OK != (ret_cod = lixa_msg_retrieve(
                               fd, buffer, sizeof(buffer) - 1,
                               &read_bytes))) {
            client_status_check_socket(cs, ret_cod);
            THROW(MSG_RETRIEVE_ERROR);
        }
        LIXA_TRACE(("client_tpm_trans: receiving %d"
                    " bytes from the server |%*.*s|\n",
                    read_bytes, read_bytes, read_bytes, buffer));

        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buffer, read_bytes, &msg))) THROW(MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        lixa_msg_trace(&msg);
#endif

        /* check exit condition */
        if (LIXA_RC_OBJ_NOT_FOUND == msg.body.trans_16.answer.rc) {
            LIXA_TRACE(("client_tpm_trans: the server answered "
                        "LIXA_RC_OBJ_NOT_FOUND; there are no more "
                        "transactions\n"));
            THROW(NO_TRANSACTIONS);
        }

        gpointer *node;

        guint i;
        for (i = 0; i < msg.body.trans_16.transactions->len; i++) {
            struct lixa_msg_body_trans_16_transaction_s *trans;
            trans = &g_array_index(
                msg.body.trans_16.transactions,
                struct lixa_msg_body_trans_16_transaction_s, i);

            XID xid;
            if (!lixa_xid_deserialize(&xid, trans->xid)) THROW(
                XID_DESERIALIZE_ERROR);

            /* look for the gtrid */
            char *gtrid = lixa_xid_get_gtrid_ascii(&xid);
            if (NULL == (node = g_tree_lookup(xidt, gtrid))) {
                LIXA_TRACE(("client_tpm_trans: creating "
                            "a new node for gtrid='%s'\n", gtrid));
                /* initialize the array */
                GArray *sxid = NULL;
                if (NULL == (sxid = g_array_new(FALSE, FALSE,
                                                sizeof(char *)))) THROW(
                                                    G_ARRAY_NEW);

                /* insert the node in the tree */
                char *key = NULL;
                if (NULL ==
                    (key = (char *) malloc(LIXA_XID_GTRID_ASCII_LENGTH))) THROW(
                        MALLOC_ERROR);
                memcpy(key, gtrid, LIXA_XID_GTRID_ASCII_LENGTH);
                /* insert the new element in the tree */
                g_tree_insert(xidt, key, sxid);
                node = (gpointer *) sxid;
            }

            /* add the serialized xid to the array */
            char *sxid = malloc(LIXA_XID_SERIALIZE_LENGTH);
            memcpy(sxid, trans->xid, LIXA_XID_SERIALIZE_LENGTH);
            LIXA_TRACE(("client_tpm_trans: adding sxid='%s'\n", sxid));
            g_array_append_val((GArray *) node, sxid);
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case G_ARRAY_NEW:
                    ret_cod = LIXA_RC_G_RETURNED_NULL;
                    break;
                case MALLOC_ERROR:
                    ret_cod = LIXA_RC_MALLOC_ERROR;
                    break;
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
                case ANALYZE_ERROR:
                    break;
                case MSG_SERIALIZE_ERROR2:
                case MSG_SEND_ERROR2:
                    break;
                case NO_TRANSACTIONS:
                    ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                    break;
                case XID_DESERIALIZE_ERROR:
                    ret_cod = LIXA_RC_MALFORMED_XID;
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
            /* release messages */
            lixa_msg_free(&msg);
        } /* TRY-CATCH */
    LIXA_TRACE(
        ("client_tpm_trans/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

int client_tpm_report(const client_status_t *cs, GTree *xidt)
{
    enum Exception
    {
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_tpm_report\n"));
    TRY {
        guint i;

        printf("\nResource manager list:\n");
        for (i = 0; i < global_ccc.actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            printf("rmid=%u, lixa_name='%s', xa_name='%s'\n", i,
                   act_rsrmgr->generic->name, act_rsrmgr->xa_switch->name);
        }

        if (g_tree_nnodes(xidt)) {
            printf("\nTransaction list:\n");
            g_tree_foreach(xidt, client_tpm_report_foreach,
                           (gpointer *) stdout);
        } else
            printf("\nThere are no transactions.\n");
        printf("\n");

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("client_tpm_report/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

int tpm_gtrid_compare(gconstpointer a, gconstpointer b, gpointer user_data)
{
    const char *gtrida = (const char *) a;
    const char *gtridb = (const char *) b;
    LIXA_TRACE(
        ("tpm_gtrid_compare: gtrida='%s', gtridb='%s'\n", gtrida, gtridb));
    return strcmp(gtrida, gtridb);
}

void tpm_gtrid_value_destroy(gpointer data)
{
    g_array_free((GArray *) data, TRUE);
}

gboolean client_tpm_report_foreach(gpointer key, gpointer value, gpointer data)
{
    /* key points to a gtrid, value points to an array of serialized xids, data points to the output stream */
    char *gtrid = (char *) key;
    GArray *sxid = (GArray *) value;
    FILE *stream = (FILE *) data;
    guint i;

    fprintf(stream, "gtrid='%s':\n", gtrid);
    for (i = 0; i < sxid->len; i++) {
        char *p = g_array_index(sxid, char*, i);
        fprintf(stream, "\tsxid='%s'\n", p);
    }
    fprintf(stream, "\n");

    return FALSE;
}

gboolean client_tpm_value_foreach(gpointer key, gpointer value, gpointer data)
{
    GArray *sxid = (GArray *) value;
    GArray *xida = (GArray *) data;
    guint i;

    for (i = 0; i < sxid->len; i++) {
        char *p = g_array_index(sxid, char*, i);
        char *xid = malloc(LIXA_XID_SERIALIZE_LENGTH);
        memcpy(xid, p, LIXA_XID_SERIALIZE_LENGTH);
        g_array_append_val(xida, xid);
    }

    return FALSE;
}

gboolean client_tpm_unique_value_foreach(gpointer key, gpointer value, gpointer data)
{
    GArray *sxid = (GArray *) value;
    GArray *xida = (GArray *) data;
    guint i;

    for (i = 0; i < sxid->len; i++) {
        char *p = g_array_index(sxid, char*, i);

        gboolean add = TRUE;
        guint j;
        for (j = 0; j < xida->len; j++) {
            char *currxid = g_array_index(xida, char*, j);
            if (strcmp(currxid, p) == 0) {
                add = FALSE;
                break;
            }
        }

        if (!add)
            continue;

        char *xid = malloc(LIXA_XID_SERIALIZE_LENGTH);
        memcpy(xid, p, LIXA_XID_SERIALIZE_LENGTH);
        g_array_append_val(xida, xid);
    }

    return FALSE;
}
