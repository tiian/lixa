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



/**
 * This mutex is used to protect the status structure when the library is
 * used in a multithread environment
 */
GStaticMutex lixa_sw_status_mutex = G_STATIC_MUTEX_INIT;
/**
 * The status is saved in a hash table: there is an element for every
 * thread
 */
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



