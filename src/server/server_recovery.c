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



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_recovery.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_RECOVERY



int srvr_rcrv_tbl_key1_job_comp(gconstpointer a, gconstpointer b)
{
    const char *ja = (const char*)lixa_job_get_raw((const lixa_job_t *)a);
    const char *jb = (const char*)lixa_job_get_raw((const lixa_job_t *)b);
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
        if (NULL == (srt->lvl1_job = g_tree_new(srvr_rcrv_tbl_key1_job_comp)))
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



int srvr_rcvr_tbl_insert(srvr_rcvr_tbl_t *srt,
                         const struct srvr_rcvr_tbl_rec_s *srtr)
{
    enum Exception { OBJ_CORRUPTED
                     , G_PTR_ARRAY_NEW_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("srvr_rcvr_tbl_insert\n"));
    TRY {
        gpointer *node;
        
        if (NULL == srt->mutex || NULL == srt->lvl1_job)
            THROW(OBJ_CORRUPTED);

        /* lock mutex */
        g_mutex_lock(srt->mutex);
        
        /* look for key1_job */
        if (NULL == (node = g_tree_lookup(srt->lvl1_job, srtr->job))) {
            /* create a new array for level 2 */
            GPtrArray *lvl2_tsid = NULL;
            if (NULL == (lvl2_tsid = g_ptr_array_sized_new(
                             srt->lvl2_tsid_array_size)))
                THROW(G_PTR_ARRAY_NEW_ERROR);
            /* insert the new element in the tree */
            g_tree_insert(srt->lvl1_job, srtr->job, lvl2_tsid);
            node = (gpointer *)lvl2_tsid;
        }

        /* @@@ restart from here */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case G_PTR_ARRAY_NEW_ERROR:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
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

