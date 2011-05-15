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
#include <config.h>



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_GMODULE_H
# include <gmodule.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif



/* XA standard header */
#include <xa.h>
/* PostgreSQL front-end */
#include <libpq-fe.h>
/* PostgreSQL XA wrapper provided by LIXA (private header) */
#include <liblixapq.h>
/* PostgreSQL XA wrapper provided by LIXA (public header) */
#include <lixapq.h>
/* LIXA standard trace library: it could be removed if "TRACE" calls would
 * be removed from this source */
#include <lixa_trace.h>
/* LIXA convenience macros: it could be removed if "TRY/CATCH" statement
 * would be removed from this source */
#include <lixa_defines.h>


/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



struct xa_switch_t xapqls = {
    "PostgreSQL[LIXA]",
    TMNOFLAGS,
    0,
    lixa_pq_open,
    lixa_pq_close,
    lixa_pq_start,
    lixa_pq_end,
    lixa_pq_rollback,
    lixa_pq_prepare,
    lixa_pq_commit,
    lixa_pq_recover,
    lixa_pq_forget,
    lixa_pq_complete
};



/**
 * This mutex is used to protect the status structure when the library is
 * used in a multithread environment
 */
GStaticMutex lixa_pq_status_mutex = G_STATIC_MUTEX_INIT;
/**
 * The status is saved in a hash table: there is an element for every
 * thread
 */
GHashTable  *lixa_pq_status = NULL;



const gchar *g_module_check_init(GModule *module)
{
    LIXA_TRACE(("lixa_pq/g_module_check_init: initializing module\n"));
    return NULL;
}



void g_module_unload(GModule *module)
{
    LIXA_TRACE(("lixa_pq/g_module_unload: releasing local storage\n"));
    /* lock the status mutex */
    g_static_mutex_lock(&lixa_pq_status_mutex);

    g_hash_table_destroy(lixa_pq_status);
    lixa_pq_status = NULL;
    
    /* unlock the status mutex */
    g_static_mutex_unlock(&lixa_pq_status_mutex);
}



void lixa_pq_status_destroy(gpointer data)
{
    lixa_pq_status_t *lps = (lixa_pq_status_t *)data;
    LIXA_TRACE(("lixa_pq_status_destroy: %p\n", data));
    if (NULL != lps) {
        g_array_free(lps->rm, TRUE);
        g_free(lps);
    }
}



int lixa_pq_open(char *xa_info, int rmid, long flags)
{
    enum Exception { ASYNC_NOT_SUPPORTED
                     , G_HASH_TABLE_NEW_FULL_ERROR
                     , G_MALLOC_ERROR
                     , PQ_CONNECTDB_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;

    PGconn           *conn = NULL;
    lixa_pq_status_t *lps = NULL;
    
    LIXA_TRACE(("lixa_pq_open: xa_info='%s', rmid=%d, flags=%ld\n",
                xa_info, rmid, flags));
    TRY {
        pthread_t key = pthread_self();
        guint i;
        
        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_open: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == lixa_pq_status) {
            /* the status structure must be initialized from scratch */
            if (NULL == (lixa_pq_status = g_hash_table_new_full(
                             g_direct_hash, g_direct_equal, NULL,
                             lixa_pq_status_destroy))) {
                LIXA_TRACE(("lixa_pq_open/g_hash_table_new_full: unable to "
                            "allocate hash table status\n"));
                THROW(G_HASH_TABLE_NEW_FULL_ERROR);
            }
        }

        /* look for this thread status */
        if (NULL == (lps = (lixa_pq_status_t *)g_hash_table_lookup(
                         lixa_pq_status, (gconstpointer)key))) {
            LIXA_TRACE(("lixa_pq_open: status for thread " PTHREAD_T_FORMAT
                        " not found, allocating it...\n", key));
            if (NULL == (lps = (lixa_pq_status_t *)g_malloc(
                             sizeof(lixa_pq_status_t)))) {
                LIXA_TRACE(("lixa_pq_open: unable to allocate %u bytes\n",
                            sizeof(lixa_pq_status_t)));
                THROW(G_MALLOC_ERROR);
            }
            lixa_pq_status_init(lps);
        }

        /* check if rmid is already connected */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_pq_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_pq_status_rm_s, i);
            if (lpsr->rmid == rmid) {
                conn = lpsr->conn;
                break;
            }
        }

        if (NULL == conn) {
            /* create a new connection */
            struct lixa_pq_status_rm_s lpsr = { 0, NULL };
            conn = PQconnectdb(xa_info);
            if (CONNECTION_OK != PQstatus(conn)) {
                LIXA_TRACE(("lixa_pq_open: error while connecting to the "
                            "database: %s", PQerrorMessage(conn)));
                PQfinish(conn);
                THROW(PQ_CONNECTDB_ERROR);
            }
            /* save the connection for this thread/rmid */
            lpsr.rmid = rmid;
            lpsr.conn = conn;
            g_array_append_val(lps->rm, lpsr);
            g_hash_table_insert(lixa_pq_status, (gpointer)key, (gpointer)lps);
        }

        /* Change state to "Initialized" */
        lps->state.R = 1;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ASYNC_NOT_SUPPORTED:
                xa_rc = XAER_ASYNC;
                break;
            case G_HASH_TABLE_NEW_FULL_ERROR:
            case G_MALLOC_ERROR:
            case PQ_CONNECTDB_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        if (NONE != excp && NULL != lps)
            g_free(lps);
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_open/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_close(char *xa_info, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_start(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_end(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_rollback(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_prepare(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_commit(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_recover(XID *xids, long count, int rmid, long flags)
{
    return 0;
}



int lixa_pq_forget(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_complete(int *handle, int *retval, int rmid, long flags)
{
    /* asynchronous mode is not supported by this wrapper */
    return XAER_INVAL;
}



PGconn *lixa_pq_get_conn(void)
{
    PGconn *conn = NULL;
    
    /* lock the mutex */
    g_static_mutex_lock(&lixa_pq_status_mutex);

    if (NULL != lixa_pq_status) {
        pthread_t key = pthread_self();
        lixa_pq_status_t *lps = NULL;
        if (NULL != (lps = (lixa_pq_status_t *)g_hash_table_lookup(
                         lixa_pq_status, (gconstpointer)key))) {
            if (0 < lps->rm->len) {
                struct lixa_pq_status_rm_s *lpsr = &g_array_index(
                    lps->rm, struct lixa_pq_status_rm_s, 0);
                conn = lpsr->conn;
            } else {
                LIXA_TRACE(("lixa_pq_get_conn: no connection found\n"));
            }
        } else {
            LIXA_TRACE(("lixa_pq_get_conn: thread not registered\n"));
        }
    } else {
        LIXA_TRACE(("lixa_pq_get_conn: status is NULL\n"));
    }
    
    /* unlock the mutex */
    g_static_mutex_unlock(&lixa_pq_status_mutex);

    return conn;
}
