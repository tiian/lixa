/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif



#include "lixa_errors.h"
#include "lixa_crash.h"
#include "lixa_trace.h"
#include "server_trans_tbl.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_SERVER_TPM



int server_trans_tbl_init(server_trans_tbl_t *stt, guint tsid_array_size)
{
    enum Exception { G_TREE_NEW_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_init\n"));
    TRY {
        memset(stt, 0, sizeof(server_trans_tbl_t));
        g_mutex_init(&stt->mutex);
        if (NULL == (stt->records = g_tree_new_full(
                         server_trans_tbl_comp, NULL,
                         free, server_trans_tbl_rec1_destroy)))
            THROW(G_TREE_NEW_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
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
    LIXA_TRACE(("server_trans_tbl_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_trans_tbl_insert(server_trans_tbl_t *stt,
                            const struct server_trans_tbl_qry_s *sttq)
{
    enum Exception { OBJ_CORRUPTED,
                     OUT_OF_RANGE,
                     G_TRY_MALLOC_ERROR1,
                     G_PTR_ARRAY_NEW_ERROR,
                     STRDUP_ERROR,
                     G_TRY_MALLOC_ERROR2,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_insert\n"));
    TRY {
        gpointer node = NULL;
        struct server_trans_tbl_rec1_s *sttr1 = NULL;
        struct server_trans_tbl_rec2_s *sttr2 = NULL;

        LIXA_TRACE(("server_trans_tbl_insert: gtrid='%s', xid='%s', "
                    "tsid=%u\n", sttq->gtrid, sttq->xid, sttq->tsid));

        if (NULL == stt->records)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(&stt->mutex);

        /* check tsid is not out of range */
        if (sttq->tsid == 0)
            THROW(OUT_OF_RANGE);

        /* look for gtrid */
        if (NULL == (node = g_tree_lookup(stt->records, sttq->gtrid))) {
            char *key = NULL;
            /* allocate a new record for this global transaction id */
            if (NULL == (sttr1 = (struct server_trans_tbl_rec1_s *)
                         g_try_malloc0(
                             sizeof(struct server_trans_tbl_rec1_s))))
                THROW(G_TRY_MALLOC_ERROR1);
            sttr1->tsid = sttq->tsid;
            if (NULL == (sttr1->branches = g_ptr_array_new_with_free_func(
                             g_free)))
                THROW(G_PTR_ARRAY_NEW_ERROR);
            /* duplicate gtrid to insert a new node in the tree */
            if (NULL == (key = strdup(sttq->gtrid)))
                THROW(STRDUP_ERROR);
            /* insert the new element in the tree */
            g_tree_insert(stt->records, (gpointer)key, sttr1);
            node = (gpointer *)sttr1;
        }
        /* create a new second level record */
        /* allocate a new record for this branch */
        if (NULL == (sttr2 = (struct server_trans_tbl_rec2_s *)
                         g_try_malloc0(
                             sizeof(struct server_trans_tbl_rec2_s))))
            THROW(G_TRY_MALLOC_ERROR2);

        sttr2->block_id = sttq->block_id;
        memcpy(sttr2->xid, sttq->xid, sizeof(lixa_ser_xid_t));
        sttr1 = (struct server_trans_tbl_rec1_s *)node;
        g_ptr_array_add(sttr1->branches, sttr2);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case G_TRY_MALLOC_ERROR1:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case G_PTR_ARRAY_NEW_ERROR:
                ret_cod = LIXA_RC_G_PTR_ARRAY_NEW_ERROR;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
            case G_TRY_MALLOC_ERROR2:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_mutex_unlock(&stt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_trans_tbl_insert/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



void server_trans_tbl_rec1_destroy(gpointer data)
{
    struct server_trans_tbl_rec1_s *sttr1;

    LIXA_TRACE(("server_trans_tbl_rec1_destroy: data=%p\n", data));
    if (NULL != data) {
        sttr1 = (struct server_trans_tbl_rec1_s *)data;
        if (NULL != sttr1->branches)
            g_ptr_array_free(sttr1->branches, TRUE);
        g_free(sttr1);
    }
}



int server_trans_tbl_comp(gconstpointer a, gconstpointer b, gpointer user_data)
{
    const char *gtrida = (const char *) a;
    const char *gtridb = (const char *) b;
    LIXA_TRACE(("server_trans_tbl_comp: gtrida='%s', gtridb='%s'\n",
                gtrida, gtridb));
    return strcmp(gtrida, gtridb);
}



int server_trans_tbl_clear(server_trans_tbl_t *stt)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_clear\n"));
    TRY {
        g_mutex_clear(&stt->mutex);

        g_tree_destroy(stt->records);
        stt->records = NULL;

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
    LIXA_TRACE(("server_trans_tbl_clear/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



gboolean server_trans_tbl_traverse(gpointer key, gpointer value, gpointer data)
{
    char *gtrid = (char *)key;
    struct server_trans_tbl_rec1_s *sttr1 =
        (struct server_trans_tbl_rec1_s *)value;
    GArray *result = (GArray *)data;
    struct server_trans_tbl_qry_s record;
    guint i;

    for (i=0; i<sttr1->branches->len; ++i) {
        struct server_trans_tbl_rec2_s *sttr2 =
            g_ptr_array_index(sttr1->branches, i);
        if (NULL == (record.gtrid = strdup(gtrid))) {
            LIXA_TRACE(("server_trans_tbl_traverse: error while calling "
                        "strdup\n"));
        }
        record.tsid = sttr1->tsid;
        record.block_id = sttr2->block_id;
        memcpy(record.xid, sttr2->xid, sizeof(record.xid));
        g_array_append_val(result, record);
    } /* for (i=0; i<sttr->branches.len; ++i) */
    
    return FALSE;
}



int server_trans_tbl_query_xid(server_trans_tbl_t *stt,
                               const struct server_trans_tbl_qry_s *sttq,
                               server_trans_tbl_qry_arr_t *result, int maint)
{
    enum Exception { OBJ_CORRUPTED,
                     NULL_OBJECT,
                     OUT_OF_RANGE,
                     OBJ_NOT_FOUND1,
                     STRDUP_ERROR,
                     OBJ_NOT_FOUND2,
                     REALLOC_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_query_xid\n"));
    TRY {
        LIXA_TRACE(("server_trans_tbl_query_xid: query is gtrid='%s', "
                    "tsid=%u\n", sttq->gtrid, sttq->tsid));

        if (NULL == stt->records)
            THROW(OBJ_CORRUPTED);
        if (NULL == result)
            THROW(NULL_OBJECT);

        /* lock mutex */
        g_mutex_lock(&stt->mutex);

        /* check tsid is not out of range */
        if (sttq->tsid == 0)
            THROW(OUT_OF_RANGE);

        if (maint) {
            g_tree_foreach(stt->records, server_trans_tbl_traverse, result);
        } else {
            guint i;
            struct server_trans_tbl_rec1_s *sttr1 = NULL;
            struct server_trans_tbl_qry_s record;
            /* look for gtrid */
                if (NULL == (sttr1 = (struct server_trans_tbl_rec1_s *)
                             g_tree_lookup(stt->records, sttq->gtrid))) {
                /* no transactions for this global transaction identifier */
                THROW(OBJ_NOT_FOUND1);
            }

            for (i=0; i<sttr1->branches->len; ++i) {
                struct server_trans_tbl_rec2_s *sttr2 =
                    g_ptr_array_index(sttr1->branches, i);
                if (NULL == (record.gtrid = strdup(sttq->gtrid)))
                    THROW(STRDUP_ERROR);
                record.tsid = sttr1->tsid;
                record.block_id = sttr2->block_id;
                memcpy(record.xid, sttr2->xid, sizeof(record.xid));
                g_array_append_val(result, record);
            } /* for (i=0; i<sttr->branches.len; ++i) */
        } /* if (maint) */

        if (0 == result->len)
            THROW(OBJ_NOT_FOUND2);
        LIXA_TRACE(("server_trans_tbl_query_xid: result is "
                    "transactions = %u\n", result->len));

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case OBJ_NOT_FOUND1:
            case OBJ_NOT_FOUND2:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
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
        g_mutex_unlock(&stt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_trans_tbl_query_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int server_trans_tbl_remove(server_trans_tbl_t *stt,
                            const struct server_trans_tbl_qry_s *sttq)
{
    enum Exception { OBJ_CORRUPTED,
                     OUT_OF_RANGE,
                     NOT_FOUND_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_trans_tbl_remove\n"));
    TRY {
        LIXA_TRACE(("server_trans_tbl_remove: gtrid='%s', xid='%s', tsid=%u\n",
                    sttq->gtrid, sttq->xid, sttq->tsid));

        if (NULL == stt->records)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(&stt->mutex);

        /* check tsid is not out of range */
        if (sttq->tsid == 0)
            THROW(OUT_OF_RANGE);

        /* remove */
        if (!g_tree_remove(stt->records, sttq->gtrid)) {
            LIXA_TRACE(("server_trans_tbl_remove: not found! "
                        "(gtrid='%s', xid='%s')\n", sttq->gtrid, sttq->xid));
            THROW(NOT_FOUND_ERROR);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case NOT_FOUND_ERROR:
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_mutex_unlock(&stt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_trans_tbl_remove/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



server_trans_tbl_qry_arr_t *server_trans_tbl_qry_arr_new(void)
{
    server_trans_tbl_qry_arr_t *arr = NULL;
    if (NULL == (arr = g_array_new(FALSE, FALSE,
                                   sizeof(struct server_trans_tbl_qry_s)))) {
        LIXA_TRACE(("server_trans_tbl_qry_arr_new: g_array_new returned "
                    "NULL\n"));
    } else {
        g_array_set_clear_func(arr, server_trans_tbl_qry_destroy);
    }
    return arr;
}



void server_trans_tbl_qry_arr_delete(server_trans_tbl_qry_arr_t *sttqa)
{
    if (NULL != sttqa)
        g_array_free(sttqa, TRUE);
}



void server_trans_tbl_qry_destroy(gpointer data)
{
    struct server_trans_tbl_qry_s *qry = (struct server_trans_tbl_qry_s *)data;
    if (NULL != qry && NULL != qry->gtrid) {
        free(qry->gtrid);
        qry->gtrid = NULL;
    }
}
