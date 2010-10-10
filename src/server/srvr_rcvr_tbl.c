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



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <srvr_rcvr_tbl.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



int srvr_rcrv_tbl_key1_job_comp(gconstpointer a, gconstpointer b,
                                gpointer user_data)
{
    const char *ja = lixa_job_get_raw((const lixa_job_t *)a);
    const char *jb = lixa_job_get_raw((const lixa_job_t *)b);
    LIXA_TRACE(("srvr_rcrv_tbl_key1_job_comp: ja='%*.*s', jb='%*.*s'\n",
                sizeof(lixa_job_t)-1, sizeof(lixa_job_t)-1, ja,
                sizeof(lixa_job_t)-1, sizeof(lixa_job_t)-1, jb));
    return strncmp(ja, jb, sizeof(lixa_job_t) - 1);
}



int srvr_rcvr_tbl_new(srvr_rcvr_tbl_t *srt, guint tsid_array_size)
{
    enum Exception { OBJ_NOT_INITIALIZED
                     , G_MUTEX_NEW_ERROR
                     , G_TREE_NEW_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("srvr_rcvr_tbl_new\n"));
    TRY {
        if (NULL != srt->mutex || NULL != srt->lvl1_job)
            THROW(OBJ_NOT_INITIALIZED);
        if (NULL == (srt->mutex = g_mutex_new()))
            THROW(G_MUTEX_NEW_ERROR);
        if (NULL == (srt->lvl1_job = g_tree_new_full(
                         srvr_rcrv_tbl_key1_job_comp, NULL,
                         free, NULL)))
            THROW(G_TREE_NEW_ERROR);
        srt->lvl2_tsid_array_size = tsid_array_size;
        
        THROW(NONE);
    } CATCH {
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
    LIXA_TRACE(("srvr_rcvr_tbl_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int srvr_rcvr_tbl_delete(srvr_rcvr_tbl_t *srt)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("srvr_rcvr_tbl_delete\n"));
    TRY {
        /* @@@ must be implemented and called from shutdown procedure to
           avoid not significative memory leak detection */
        if (NULL != srt->mutex) {
            g_mutex_free(srt->mutex);
            srt->mutex = NULL;
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
    LIXA_TRACE(("srvr_rcvr_tbl_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



gboolean srvr_rcvr_tbl_trace_trav(gpointer key, gpointer value,
                                  gpointer data)
{
    LIXA_TRACE(("srvr_rcvr_tbl_trace_trav: key=%p, value=%p\n", key, value));
    LIXA_TRACE(("srvr_rcvr_tbl_trace_trav: job='%s'\n",
                lixa_job_get_raw((lixa_job_t *)key)));
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
                    UINT32_T_FORMAT"\n", lixa_job_get_raw(srtr->job),
                    srtr->tsid, srtr->block_id));
        
        if (NULL == srt->mutex || NULL == srt->lvl1_job)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(srt->mutex);

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
            node = (gpointer *)lvl2_tsid;
        }

        /* retrieve the queue associated to the thread status id tsid and push
           the block_id */
        queue = &g_array_index((GArray *)node, GQueue, srtr->tsid);
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
        g_mutex_unlock(srt->mutex);
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
    enum Exception { OBJ_CORRUPTED
                     , OUT_OF_RANGE
                     , OBJ_NOT_FOUND1
                     , BYPASSED_OPERATION
                     , OBJ_NOT_FOUND2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("srvr_rcvr_tbl_get_block\n"));
    TRY {
        gpointer *node;
        GQueue *queue;
        
        LIXA_TRACE(("srvr_rcvr_tbl_get_block: query is job='%s', tsid=%u\n",
                    lixa_job_get_raw(srtr->job), srtr->tsid));

        if (NULL == srt->mutex || NULL == srt->lvl1_job)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(srt->mutex);

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
        queue = &g_array_index((GArray *)node, GQueue, srtr->tsid);
        if (g_queue_is_empty(queue)) {
            guint i;
            /* this queue is empty, check other queues... */
            for (i = 1; i < srt->lvl2_tsid_array_size; ++i) {
                GQueue *q = &g_array_index((GArray *)node, GQueue, i);
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
                    lixa_job_get_raw(out->job), out->tsid, out->block_id));
        
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
        g_mutex_unlock(srt->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("srvr_rcvr_tbl_get_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



