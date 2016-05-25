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
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA_SWITCH



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



gpointer lixa_sw_get_conn_by_rmid(int rmid, int rm_type)
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
                    if (rmid == lpsr->rmid) {
                        if (rm_type != lpsr->rm_type) {
                            LIXA_TRACE(("lixa_sw_get_conn_by_rmid: rmid %d "
                                        "is of type %d while caller is "
                                        "expecting type %d\n",
                                        rmid, lpsr->rm_type, rm_type));
                        } else 
                            conn = lpsr->conn;
                        break;
                    }
                }
                if (NULL == conn)
                    LIXA_TRACE(("lixa_sw_get_conn_by_rmid: no connection "
                                "found for rmid=%d\n", rmid));
            } else {
                LIXA_TRACE(("lixa_sw_get_conn_by_rmid: no connection "
                            "found to PostgreSQL/MySQL databases\n"));
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



gpointer lixa_sw_get_conn_by_pos(int pos, int rm_type)
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
                int j=0;
                for (i=0; i<lps->rm->len; ++i) {
                    struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                        lps->rm, struct lixa_sw_status_rm_s, i);
                    LIXA_TRACE(("lixa_sw_get_conn_by_pos: i=%d, rm_type=%d\n",
                                i, lpsr->rm_type));
                    if (lpsr->rm_type == rm_type) {
                        if (j == pos) {
                            conn = lpsr->conn;
                            break;
                        } else
                            j++;
                    }
                }
            }
            if (NULL == conn) {
                LIXA_TRACE(("lixa_sw_get_conn_by_pos: %d-th resource manager "
                            "of type %d not found\n", pos, rm_type));
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



int lixa_sw_is_managed_conn(const gpointer conn, int rm_type)
{
    int is_managed = FALSE;
    
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
                    LIXA_TRACE(("lixa_sw_is_managed_conn: i=%d, rm_type=%d\n",
                                i, lpsr->rm_type));
                    if (lpsr->rm_type == rm_type)
                        if (conn == lpsr->conn) {
                            is_managed = TRUE;
                            break;
                        }
                }
            }
            if (!is_managed) {
                LIXA_TRACE(("lixa_sw_is_managed_conn: %p conn for resource "
                            "manager of type %d not found\n", conn, rm_type));
            }
        } else {
            LIXA_TRACE(("lixa_sw_is_managed_conn: thread not registered\n"));
        }
    } else {
        LIXA_TRACE(("lixa_sw_is_managed_conn: status is NULL\n"));
    }
    
    /* unlock the mutex */
    g_static_mutex_unlock(&lixa_sw_status_mutex);

    LIXA_TRACE(("lixa_sw_is_managed_conn: return value is %d\n", is_managed));
    return is_managed;
}



