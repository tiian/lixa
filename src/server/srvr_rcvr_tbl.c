/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "srvr_rcvr_tbl.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



int srvr_rcrv_tbl_key1_job_comp(gconstpointer a, gconstpointer b,
                                gpointer user_data)
{
    const char *ja = lixa_job_get_raw((const lixa_job_t *) a);
    const char *jb = lixa_job_get_raw((const lixa_job_t *) b);
    LIXA_TRACE(("srvr_rcrv_tbl_key1_job_comp: ja='%*.*s', jb='%*.*s'\n",
                sizeof(lixa_job_t) - 1, sizeof(lixa_job_t) - 1, ja,
                sizeof(lixa_job_t) - 1, sizeof(lixa_job_t) - 1, jb));
    return strncmp(ja, jb, sizeof(lixa_job_t) - 1);
}



void srvr_rcvr_tbl_value1_destroy(gpointer data)
{
    GArray *lvl2_tsid;
    GQueue *queue;
    gint i;

    LIXA_TRACE(("srvr_rcvr_tbl_value1_destroy: data=%p\n", data));
    lvl2_tsid = (GArray *) data;
    for (i = 0; i < lvl2_tsid->len; ++i) {
        queue = &g_array_index(lvl2_tsid, GQueue, i);
        while (!g_queue_is_empty(queue)) {
            g_queue_pop_tail(queue);
        }
    }
    g_array_free(lvl2_tsid, TRUE);
}



int srvr_rcvr_tbl_init(srvr_rcvr_tbl_t *srt, guint tsid_array_size)
{
    enum Exception { G_TREE_NEW_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("srvr_rcvr_tbl_init\n"));
    TRY {
        memset(srt, 0, sizeof(srvr_rcvr_tbl_t));
        g_mutex_init(&srt->mutex);
        if (NULL == (srt->lvl1_job = g_tree_new_full(
                         srvr_rcrv_tbl_key1_job_comp, NULL,
                         free, srvr_rcvr_tbl_value1_destroy)))
            THROW(G_TREE_NEW_ERROR);
        srt->lvl2_tsid_array_size = tsid_array_size;

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
    LIXA_TRACE(("srvr_rcvr_tbl_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int srvr_rcvr_tbl_clear(srvr_rcvr_tbl_t *srt)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("srvr_rcvr_tbl_clear\n"));
    TRY {
        g_mutex_clear(&srt->mutex);

        g_tree_destroy(srt->lvl1_job);
        srt->lvl1_job = NULL;

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
    LIXA_TRACE(("srvr_rcvr_tbl_clear/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



gboolean srvr_rcvr_tbl_trace_trav(gpointer key, gpointer value,
                                  gpointer data)
{
    LIXA_TRACE(("srvr_rcvr_tbl_trace_trav: key=%p, value=%p\n", key, value));
    LIXA_TRACE(("srvr_rcvr_tbl_trace_trav: job='%s'\n",
                lixa_job_get_raw((lixa_job_t *) key)));
    return FALSE;
}



void srvr_rcvr_tbl_trace(srvr_rcvr_tbl_t *srt)
{
    LIXA_TRACE(("srvr_rcvr_tbl_trace: srt=%p, srt->mutex=%p, "
                "srt->lvl1_job=%p, srt->lvl2_tsid_array_size=%u\n",
                srt, srt->mutex, srt->lvl1_job, srt->lvl2_tsid_array_size));
    g_tree_foreach(srt->lvl1_job, srvr_rcvr_tbl_trace_trav, NULL);
}



int srvr_rcvr_tbl_insert(srvr_rcvr_tbl_t *srt,
                         const struct srvr_rcvr_tbl_rec_s *srtr)
{
    enum Exception { OBJ_CORRUPTED
                     , OUT_OF_RANGE
                     , G_ARRAY_SIZED_NEW_ERROR
                     , MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("srvr_rcvr_tbl_insert\n"));
    TRY {
        gpointer *node;
        GQueue *queue;

        LIXA_TRACE(("srvr_rcvr_tbl_insert: job='%s', tsid=%u, block_id="
                    UINT32_T_FORMAT
                    "\n", lixa_job_get_raw(srtr->job),
                    srtr->tsid, srtr->block_id));

        if (NULL == srt->lvl1_job)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(&srt->mutex);

        /* check tsid is not out of range */
        if (srtr->tsid == 0 || srtr->tsid >= srt->lvl2_tsid_array_size)
            THROW(OUT_OF_RANGE);

        /* look for key1_job */
        if (NULL == (node = g_tree_lookup(srt->lvl1_job, srtr->job))) {
            /* create a new array for level 2 */
            GArray *lvl2_tsid = NULL;
            guint i;
            lixa_job_t *key = NULL;

            if (NULL == (lvl2_tsid = g_array_sized_new(
                             FALSE, FALSE, sizeof(GQueue),
                             srt->lvl2_tsid_array_size)))
                THROW(G_ARRAY_SIZED_NEW_ERROR);
            /* prepare the array: all the elements are initialized with an
               empty queue object */
            for (i = 0; i < srt->lvl2_tsid_array_size; ++i) {
                GQueue q;
                g_queue_init(&q);
                g_array_append_val(lvl2_tsid, q);
            }
            if (NULL == (key = malloc(sizeof(lixa_job_t))))
                THROW(MALLOC_ERROR);
            memcpy(key, srtr->job, sizeof(lixa_job_t));
            /* insert the new element in the tree */
            g_tree_insert(srt->lvl1_job, key, lvl2_tsid);
            node = (gpointer *) lvl2_tsid;
        }

        /* retrieve the queue associated to the thread status id tsid and push
           the block_id */
        queue = &g_array_index((GArray *) node, GQueue, srtr->tsid);
        g_queue_push_tail(queue, GUINT_TO_POINTER(srtr->block_id));

        THROW(NONE);
    } CATCH {
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_mutex_unlock(&srt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("srvr_rcvr_tbl_insert/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int srvr_rcvr_tbl_get_block(srvr_rcvr_tbl_t *srt,
                            const struct srvr_rcvr_tbl_rec_s *srtr,
                            struct srvr_rcvr_tbl_rec_s *out,
                            int browse)
{
    enum Exception { OBJ_CORRUPTED,
                     OUT_OF_RANGE,
                     OBJ_NOT_FOUND1,
                     BYPASSED_OPERATION,
                     OBJ_NOT_FOUND2,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("srvr_rcvr_tbl_get_block\n"));
    TRY {
        gpointer *node;
        GQueue *queue;

        LIXA_TRACE(("srvr_rcvr_tbl_get_block: query is job='%s', tsid=%u\n",
                    lixa_job_get_raw(srtr->job), srtr->tsid));

        if (NULL == srt->lvl1_job)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(&srt->mutex);

        /* check tsid is not out of range */
        if (srtr->tsid == 0 || srtr->tsid >= srt->lvl2_tsid_array_size)
            THROW(OUT_OF_RANGE);

        /* reset output record */
        memset(out->job, 0, sizeof(lixa_job_t));
        out->tsid = 0;
        out->block_id = 0;

        /* look for key1_job */
        if (NULL == (node = g_tree_lookup(srt->lvl1_job, srtr->job))) {
            /* no transactions for this job */
            THROW(OBJ_NOT_FOUND1);
        }

        /* check the size of the queue pointed by tsid */
        queue = &g_array_index((GArray *) node, GQueue, srtr->tsid);
        if (g_queue_is_empty(queue)) {
            guint i;
            /* this queue is empty, check other queues... */
            for (i = 1; i < srt->lvl2_tsid_array_size; ++i) {
                GQueue *q = &g_array_index((GArray *) node, GQueue, i);
                if (!g_queue_is_empty(q)) {
                    memcpy(out->job, srtr->job, sizeof(lixa_job_t));
                    out->tsid = i;
                    LIXA_TRACE(("srvr_rcvr_tbl_get_block: answer is job='%s', "
                                "tsid=%u\n",
                                lixa_job_get_raw(out->job), out->tsid));
                    THROW(BYPASSED_OPERATION);
                } /* if (!g_queue_is_empty(q)) */
            } /* for i */
            THROW(OBJ_NOT_FOUND2);
        }

        /* there's a transaction */
        memcpy(out->job, srtr->job, sizeof(lixa_job_t));
        out->tsid = srtr->tsid;
        out->block_id = browse ?
            GPOINTER_TO_UINT(g_queue_peek_head(queue)) :
            GPOINTER_TO_UINT(g_queue_pop_head(queue));
        LIXA_TRACE(("srvr_rcvr_tbl_get_block: answer is job='%s', "
                    "tsid=%u, block_id=" UINT32_T_FORMAT "\n",
                    lixa_job_get_raw(
                        out->job), out->tsid, out->block_id));

        THROW(NONE);
    } CATCH {
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
            case BYPASSED_OPERATION:
                ret_cod = LIXA_RC_BYPASSED_OPERATION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_mutex_unlock(&srt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("srvr_rcvr_tbl_get_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



gboolean srvr_rcvr_tbl_get_array_trav(gpointer key, gpointer value,
                                      gpointer data)
{
    GQueue *queue;
    guint queue_length, i;
    
    struct srvr_rcvr_tbl_get_array_data_s *data_ref =
        (struct srvr_rcvr_tbl_get_array_data_s *)data;

    if (NULL == data_ref) {
        LIXA_TRACE(("srvr_rcvr_tbl_get_array_trav: data_ref is NULL, this "
                    "is an internal error\n"));
        return TRUE;
    }
    
    LIXA_TRACE(("srvr_rcvr_tbl_get_array_trav: key=%p, value=%p, tsid=%u\n",
                key, value, data_ref->tsid));
    /* a node of the tree is an array, the tsid-th position contains the
       queue of block_id to retrieve */
    queue = &g_array_index((GArray *) value, GQueue, data_ref->tsid);
    queue_length = g_queue_get_length(queue);
    LIXA_TRACE(("srvr_rcvr_tbl_get_array_trav: there are %u block_id to "
                "pick-up from this node\n", queue_length));
    for (i=0; i<queue_length; ++i) {
        uint32_t block_id = GPOINTER_TO_UINT(g_queue_peek_nth(queue, i));
        LIXA_TRACE(("srvr_rcvr_tbl_get_array_trav: adding block_id "
                    UINT32_T_FORMAT " to array\n", block_id));
        g_array_append_val(data_ref->array, block_id);
    } /* for (i=0; i<queue_length; ++i) */
    
    return FALSE;
}



GArray *srvr_rcvr_tbl_get_array(srvr_rcvr_tbl_t *srt, guint tsid)
{
    enum Exception { G_ARRAY_NEW
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct srvr_rcvr_tbl_get_array_data_s data;
    
    LIXA_TRACE(("srvr_rcvr_tbl_get_array: srt=%p, tsid=%u\n", srt, tsid));
    TRY {
        /* traverse the tree to collect the block_id related to tsid */
        data.tsid = tsid;
        data.array = NULL;
        /* create a new array to return */
        if (NULL == (data.array = g_array_new(FALSE, FALSE, sizeof(uint32_t))))
            THROW(G_ARRAY_NEW);
        /* lock mutex to guarantee struct stability */
        g_mutex_lock(&srt->mutex);
        /* search the block_id and put in data.array */
        g_tree_foreach(srt->lvl1_job, srvr_rcvr_tbl_get_array_trav,
                       (gpointer)&data);
        /* unlock the mutex */
        g_mutex_unlock(&srt->mutex);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_ARRAY_NEW:
                ret_cod = LIXA_RC_G_ARRAY_NEW_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("srvr_rcvr_tbl_get_array/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return data.array;
}

