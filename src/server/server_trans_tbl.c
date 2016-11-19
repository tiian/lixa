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

#include <lixa_errors.h>
#include <lixa_crash.h>
#include <lixa_trace.h>
#include "server_trans_tbl.h"

/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_SERVER_TPM

int server_trans_tbl_new(server_trans_tbl_t *stt, guint tsid_array_size)
{
    enum Exception
    {
        OBJ_NOT_INITIALIZED, G_MUTEX_NEW_ERROR, G_TREE_NEW_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_new\n"));
    TRY {
        if (NULL != stt->mutex || NULL != stt->records) THROW(
            OBJ_NOT_INITIALIZED);
        if (NULL == (stt->mutex = g_mutex_new())) THROW(G_MUTEX_NEW_ERROR);
        if (NULL == (stt->records = g_tree_new_full(
            server_trans_tbl_comp, NULL,
            free, server_trans_tbl_value_destroy))) THROW(G_TREE_NEW_ERROR);
        stt->tsid_array_size = tsid_array_size;

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case OBJ_NOT_INITIALIZED:
                ret_cod = LIXA_RC_OBJ_NOT_INITIALIZED;
                break;
            case G_MUTEX_NEW_ERROR:
            case G_TREE_NEW_ERROR:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_trans_tbl_new/excp=%d/"
        "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

int server_trans_tbl_insert(server_trans_tbl_t *stt,
                            const struct server_trans_tbl_rec_s *sttr)
{
    enum Exception
    {
        OBJ_CORRUPTED,
        OUT_OF_RANGE,
        G_ARRAY_SIZED_NEW_ERROR,
        MALLOC_ERROR,
        XID_SERIALIZE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_insert\n"));
    TRY {
        gpointer *node;
        GQueue *queue;

        lixa_ser_xid_t ser_xid;
        if (!lixa_xid_serialize(&(sttr->xid), ser_xid)) THROW(
            XID_SERIALIZE_ERROR);

        LIXA_TRACE(
            ("server_trans_tbl_insert: gtrid='%s', xid='%s', tsid=%u, block_id="
            UINT32_T_FORMAT
            "\n", sttr->gtrid, ser_xid,
                sttr->tsid, sttr->block_id));

        if (NULL == stt->mutex || NULL == stt->records) THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(stt->mutex);

        /* check tsid is not out of range */
        if (sttr->tsid == 0 || sttr->tsid >= stt->tsid_array_size) THROW(
            OUT_OF_RANGE);

        /* look for gtrid */
        if (NULL == (node = g_tree_lookup(stt->records, sttr->gtrid))) {
            /* create a new array */
            GArray *tsid = NULL;
            guint i;
            char *key = NULL;

            if (NULL == (tsid = g_array_sized_new(
                FALSE, FALSE, sizeof(GQueue),
                stt->tsid_array_size))) THROW(G_ARRAY_SIZED_NEW_ERROR);
            /* prepare the array: all the elements are initialized with an empty queue object */
            for (i = 0; i < stt->tsid_array_size; ++i) {
                GQueue q;
                g_queue_init(&q);
                g_array_append_val(tsid, q);
            }
            if (NULL == (key = malloc(sizeof(sttr->gtrid)))) THROW(
                MALLOC_ERROR);
            memcpy(key, sttr->gtrid, sizeof(sttr->gtrid));
            /* insert the new element in the tree */
            g_tree_insert(stt->records, key, tsid);
            node = (gpointer *) tsid;
        }

        /* retrieve the queue associated to the thread status id tsid and push the XID */
        queue = &g_array_index((GArray *) node, GQueue, sttr->tsid);
        g_queue_push_tail(queue, (gpointer *) &(sttr->xid));

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case G_ARRAY_SIZED_NEW_ERROR:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_mutex_unlock(stt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_trans_tbl_insert/excp=%d/"
        "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

void server_trans_tbl_value_destroy(gpointer data)
{
    GArray *tsid;
    GQueue *queue;
    gint i;

    LIXA_TRACE(("server_trans_tbl_value_destroy: data=%p\n", data));
    tsid = (GArray *) data;
    for (i = 0; i < tsid->len; ++i) {
        queue = &g_array_index(tsid, GQueue, i);
        while (!g_queue_is_empty(queue)) {
            g_queue_pop_tail(queue);
        }
    }
    g_array_free(tsid, TRUE);
}

int server_trans_tbl_comp(gconstpointer a, gconstpointer b, gpointer user_data)
{
    const char *gtrida = (const char *) a;
    const char *gtridb = (const char *) b;
    LIXA_TRACE(
        ("server_trans_tbl_comp: gtrida='%s', gtridb='%s'\n", gtrida, gtridb));
    return strcmp(gtrida, gtridb);
}

int server_trans_tbl_delete(server_trans_tbl_t *stt)
{
    enum Exception
    {
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_delete\n"));
    TRY {
        if (NULL != stt->mutex) {
            g_mutex_free(stt->mutex);
            stt->mutex = NULL;
        }

        g_tree_destroy(stt->records);
        stt->records = NULL;

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
    LIXA_TRACE(("server_trans_tbl_delete/excp=%d/"
        "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

int server_trans_tbl_query_xid(server_trans_tbl_t *stt,
                               const struct server_trans_tbl_rec_s *sttr,
                               struct server_trans_tbl_rec_s *out,
                               guint *out_array_size)
{
    enum Exception
    {
        OBJ_CORRUPTED,
        OUT_OF_RANGE,
        OBJ_NOT_FOUND1,
        OBJ_NOT_FOUND2,
        REALLOC_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_query_xid\n"));
    TRY {
        gpointer *node;
        GQueue *queue;

        LIXA_TRACE(
            ("server_trans_tbl_query_xid: query is gtrid='%s', tsid=%u\n",
                sttr->gtrid, sttr->tsid));

        if (NULL == stt->mutex || NULL == stt->records) THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(stt->mutex);

        /* check tsid is not out of range */
        if (sttr->tsid == 0 || sttr->tsid >= stt->tsid_array_size) THROW(
            OUT_OF_RANGE);

        /* look for gtrid */
        if (NULL == (node = g_tree_lookup(stt->records, sttr->gtrid))) {
            /* no transactions for this global transaction identifier */
            THROW(OBJ_NOT_FOUND1);
        }

        /* check the size of the queue pointed by tsid */
        gboolean hastransactions = FALSE;
        guint i, last = 0;

        for (i = 1; i < stt->tsid_array_size; ++i) {
            GQueue *q = &g_array_index((GArray *) node, GQueue, i);
            if (!g_queue_is_empty(q)) {
                hastransactions = TRUE;

                /* add all entries from queue to out record */
                if (NULL == (out = realloc(
                    out, (last + q->length) *
                         sizeof(struct server_trans_tbl_rec_s)))) THROW(
                    REALLOC_ERROR);

                guint x;
                for (x = 0; x < q->length; x++) {
                    XID *xid = g_queue_peek_nth(q, x);
                    struct server_trans_tbl_rec_s outr;
                    outr.xid = *xid;

                    out[last] = outr;
                    last++;
                }
            } /* if (!g_queue_is_empty(q)) */
        } /* for i */

        if (!hastransactions) {
            THROW(OBJ_NOT_FOUND2);
        }

        *out_array_size = last;

        LIXA_TRACE(("server_trans_tbl_query_xid: result is transactions="
                       UINT32_T_FORMAT
                       "\n", *out_array_size));

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case OBJ_NOT_FOUND1:
            case OBJ_NOT_FOUND2:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_mutex_unlock(stt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_trans_tbl_query_xid/excp=%d/"
        "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return 0;
}
