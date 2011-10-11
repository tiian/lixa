/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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

/*
 * This file contains utilities used by LIXA provided switch files (MySQL,
 * PostgreSQL, ...)
 */

#include <config.h>



#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif



#include <lixa_trace.h>
#include <lixa_sw.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



GStaticMutex lixa_sw_status_mutex = G_STATIC_MUTEX_INIT;
GHashTable  *lixa_sw_status = NULL;



void lixa_sw_status_destroy(gpointer data)
{
    lixa_sw_status_t *lps = (lixa_sw_status_t *)data;
    LIXA_TRACE(("lixa_sw_status_destroy: %p\n", data));
    if (NULL != lps) {
        g_array_free(lps->rm, TRUE);
        free(lps);
    }
}



struct lixa_sw_status_rm_s *lixa_sw_status_rm_get(int rmid)
{
    guint i;
    pthread_t key = pthread_self();
    lixa_sw_status_t *lps = NULL;
    struct lixa_sw_status_rm_s *lpsr = NULL;
    
    if (NULL == lixa_sw_status) {
        LIXA_TRACE(("lixa_sw_status_rm_get: lixa_sw_status is NULL\n"));
        return NULL;
    }   
    if (NULL == (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                     lixa_sw_status, (gconstpointer)key))) {
        LIXA_TRACE(("lixa_sw_status_rm_get: thread " PTHREAD_T_FORMAT
                    "not registered\n"));
        return NULL;
    }
    /* look for rmid */
    for (i=0; i<lps->rm->len; ++i) {
        lpsr = &g_array_index(lps->rm, struct lixa_sw_status_rm_s, i);
        if (lpsr->rmid == rmid)
            break;
    }    
    if (i == lps->rm->len) {
        LIXA_TRACE(("lixa_sw_status_rm_get: rmid %d is not registered\n",
                    rmid));
        return NULL;
    }
    return lpsr;
}



gpointer lixa_sw_get_conn_by_rmid(int rmid)
{
    gpointer conn = NULL;
    
    /* lock the mutex */
    g_static_mutex_lock(&lixa_sw_status_mutex);

    if (NULL != lixa_sw_status) {
        pthread_t key = pthread_self();
        lixa_sw_status_t *lps = NULL;
        if (NULL != (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                         lixa_sw_status, (gconstpointer)key))) {
            if (0 < lps->rm->len) {
                guint i;
                for (i=0; i<lps->rm->len; ++i) {
                    struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                        lps->rm, struct lixa_sw_status_rm_s, i);
                    if (lpsr->rmid == rmid) {
                        conn = lpsr->conn;
                        break;
                    }
                }
                if (NULL == conn)
                    LIXA_TRACE(("lixa_sw_get_conn_by_rmid: no connection "
                                "found for rmid=%d\n", rmid));
            } else {
                LIXA_TRACE(("lixa_sw_get_conn_by_rmid: no connection "
                            "found to PostgreSQL databases\n"));
            }
        } else {
            LIXA_TRACE(("lixa_sw_get_conn_by_rmid: thread not registered\n"));
        }
    } else {
        LIXA_TRACE(("lixa_sw_get_conn_by_rmid: status is NULL\n"));
    }
    
    /* unlock the mutex */
    g_static_mutex_unlock(&lixa_sw_status_mutex);

    return conn;
}



gpointer lixa_sw_get_conn_by_pos(int pos)
{
    gpointer conn = NULL;
    
    /* lock the mutex */
    g_static_mutex_lock(&lixa_sw_status_mutex);

    if (NULL != lixa_sw_status) {
        pthread_t key = pthread_self();
        lixa_sw_status_t *lps = NULL;
        if (NULL != (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                         lixa_sw_status, (gconstpointer)key))) {
            if (pos < lps->rm->len) {
                struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                    lps->rm, struct lixa_sw_status_rm_s, pos);
                conn = lpsr->conn;
            } else {
                LIXA_TRACE(("lixa_sw_get_conn_by_pos: %d exceeds %d, "
                            "the last position\n", pos, lps->rm->len));
            }
        } else {
            LIXA_TRACE(("lixa_sw_get_conn_by_pos: thread not registered\n"));
        }
    } else {
        LIXA_TRACE(("lixa_sw_get_conn_by_pos: status is NULL\n"));
    }
    
    /* unlock the mutex */
    g_static_mutex_unlock(&lixa_sw_status_mutex);

    return conn;
}



