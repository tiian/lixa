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



#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
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
/* LIXA utilities: it contains XID serialization/deserialization utilities */
#include <lixa_utils.h>



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
        free(lps);
    }
}



struct lixa_pq_status_rm_s *lixa_pq_status_rm_get(int rmid)
{
    guint i;
    pthread_t key = pthread_self();
    lixa_pq_status_t *lps = NULL;
    struct lixa_pq_status_rm_s *lpsr = NULL;
    
    if (NULL == lixa_pq_status) {
        LIXA_TRACE(("lixa_pq_status_rm_get: lixa_pq_status is NULL\n"));
        return NULL;
    }   
    if (NULL == (lps = (lixa_pq_status_t *)g_hash_table_lookup(
                     lixa_pq_status, (gconstpointer)key))) {
        LIXA_TRACE(("lixa_pq_status_rm_get: thread " PTHREAD_T_FORMAT
                    "not registered\n"));
        return NULL;
    }
    /* look for rmid */
    for (i=0; i<lps->rm->len; ++i) {
        lpsr = &g_array_index(lps->rm, struct lixa_pq_status_rm_s, i);
        if (lpsr->rmid == rmid)
            break;
    }    
    if (i == lps->rm->len) {
        LIXA_TRACE(("lixa_pq_status_rm_get: rmid %d is not registered\n",
                    rmid));
        return NULL;
    }
    return lpsr;
}



int lixa_pq_open(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , G_HASH_TABLE_NEW_FULL_ERROR
                     , MALLOC_ERROR
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
        const long valid_flags = TMASYNC|TMNOFLAGS;
        
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }
        
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
            if (NULL == (lps = (lixa_pq_status_t *)malloc(
                             sizeof(lixa_pq_status_t)))) {
                LIXA_TRACE(("lixa_pq_open: unable to allocate %u bytes\n",
                            sizeof(lixa_pq_status_t)));
                THROW(MALLOC_ERROR);
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
            struct lixa_pq_status_rm_s lpsr;
            lixa_pq_status_rm_init(&lpsr);
            conn = PQconnectdb(xa_info);
            if (CONNECTION_OK != PQstatus(conn)) {
                LIXA_TRACE(("lixa_pq_open: error while connecting to the "
                            "database: %s", PQerrorMessage(conn)));
                PQfinish(conn);
                THROW(PQ_CONNECTDB_ERROR);
            }
            /* save the connection for this thread/rmid */
            lpsr.rmid = rmid;
            lpsr.state.R = 1;
            lpsr.conn = conn;
            g_array_append_val(lps->rm, lpsr);
            g_hash_table_insert(lixa_pq_status, (gpointer)key, (gpointer)lps);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
                xa_rc = XAER_INVAL;
                break;
            case ASYNC_NOT_SUPPORTED:
                xa_rc = XAER_ASYNC;
                break;
            case G_HASH_TABLE_NEW_FULL_ERROR:
            case MALLOC_ERROR:
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
            free(lps);
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_open/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_close(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , NOTHING_TO_DO1
                     , NOTHING_TO_DO2
                     , PROTOCOL_ERROR
                     , NOTHING_TO_DO3
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_close\n"));
    TRY {
        guint             i;
        pthread_t         key = pthread_self();
        lixa_pq_status_t *lps = NULL;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }

        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_close: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == lixa_pq_status) {
            /* the status structure does not exist, this is a dummy xa_close */
            THROW(NOTHING_TO_DO1);
        }
            
        if (NULL == (lps = (lixa_pq_status_t *)g_hash_table_lookup(
                         lixa_pq_status, (gconstpointer)key))) {
            /* the status structure does not contain a record for this
               thread, this is a dummy xa_close */
            THROW(NOTHING_TO_DO2);
        }

        /* check the state of the resource manager */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_pq_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_pq_status_rm_s, i);
            if (lpsr->rmid == rmid) {
                /* state found, checking protocol errors */
                if (lpsr->state.T == 1 ||
                    lpsr->state.S == 1) {
                    LIXA_TRACE(("lixa_pq_close: state is T%d, S%d and "
                                "xa_close() cannot be called\n",
                                lpsr->state.T, lpsr->state.S));
                    THROW(PROTOCOL_ERROR);
                }
                /* state found, checking "Un-initialized" state */
                if (lpsr->state.R == 0)
                    THROW(NOTHING_TO_DO3);
                /* closing database connection if any */
                if (NULL != lpsr->conn) {
                    PQfinish(lpsr->conn);
                    lpsr->conn = NULL;
                }
                lpsr->state.R = 0;
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
                xa_rc = XAER_INVAL;
                break;
            case ASYNC_NOT_SUPPORTED:
                xa_rc = XAER_ASYNC;
                break;
            case NOTHING_TO_DO1:
            case NOTHING_TO_DO2:
                xa_rc = XA_OK;
                break;
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NOTHING_TO_DO3:
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_close/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_start(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , BEGIN_ERROR
                     , NONE} excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_start\n"));
    TRY {
        struct lixa_pq_status_rm_s *lpsr = NULL;
        const long valid_flags = TMJOIN|TMRESUME|TMNOWAIT|TMASYNC|TMNOFLAGS;
        lixa_ser_xid_t lsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_end: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOFLAGS != flags) {
            LIXA_TRACE(("lixa_pq_start: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == (lpsr = lixa_pq_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 ||
            (lpsr->state.S != 0 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_pq_start: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_start: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_ser_xid_serialize(lsx, xid)) {
            LIXA_TRACE(("lixa_pq_start: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }
        LIXA_TRACE(("lixa_pq_start: starting XID '%s'\n", lsx));

        /* saving xid */
        lpsr->xid = *xid;
        /* starting transaction */
        res = PQexec(lpsr->conn, "BEGIN");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_start: error while executing BEGIN command "
                        "(%s)\n", PQerrorMessage(lpsr->conn)));
            THROW(BEGIN_ERROR);
        }
        PQclear(res);
        res = NULL;

        lpsr->state.T = 1;
        lpsr->state.S = 1;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR1:
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case BEGIN_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_start/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_end(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , ROLLBACK_ONLY
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_end\n"));
    TRY {
        struct lixa_pq_status_rm_s *lpsr = NULL;
        const long valid_flags = TMSUSPEND|TMMIGRATE|TMSUCCESS|TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_end: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if ((TMSUSPEND|TMMIGRATE|TMASYNC) & flags) {
            LIXA_TRACE(("lixa_pq_end: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == (lpsr = lixa_pq_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 1 ||
            (lpsr->state.S != 1 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_pq_end: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_end: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_ser_xid_serialize(lsx, xid)) {
            LIXA_TRACE(("lixa_pq_end: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_ser_xid_serialize(lsx2, &(lpsr->xid));
            LIXA_TRACE(("lixa_pq_end: ending XID '%s' is not the same "
                        "of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_end: ending XID '%s'\n", lsx));

        /* check if the TM marked as failed the transaction */
        if (TMFAIL & flags) {
            LIXA_TRACE(("lixa_pq_end: TMFAIL detected, entering 'Rollback "
                        "Only' state\n"));
            lpsr->state.S = 4;
            lpsr->state.T = 0;
            THROW(ROLLBACK_ONLY);
        } else {
            lpsr->state.S = 2;
            lpsr->state.T = 0;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR1:
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case ROLLBACK_ONLY:
                xa_rc = XA_RBROLLBACK;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_end/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_rollback(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , ROLLBACK_ERROR1
                     , ROLLBACK_ERROR2
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_rollback\n"));
    TRY {
        struct lixa_pq_status_rm_s *lpsr = NULL;
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_rollback: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_rollback: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == (lpsr = lixa_pq_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 ||
            (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4)) {
            LIXA_TRACE(("lixa_pq_rollback: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_rollback: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_ser_xid_serialize(lsx, xid)) {
            LIXA_TRACE(("lixa_pq_rollback: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_ser_xid_serialize(lsx2, &(lpsr->xid));
            LIXA_TRACE(("lixa_pq_rollback: rolling back XID '%s' is not the "
                        "same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_rollback: rolling back XID '%s'\n", lsx));

        if (lpsr->state.S == 3) {
            const char ROLLBACK_PREP_FMT[] = "ROLLBACK PREPARED '%s';";
            char pq_cmd_buf[sizeof(ROLLBACK_PREP_FMT) +
                            sizeof(lixa_ser_xid_t)];
            /* rolling back the transaction */
            snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), ROLLBACK_PREP_FMT, lsx);
            res = PQexec(lpsr->conn, pq_cmd_buf);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_rollback: error while executing "
                            "ROLLBACK PREPARED command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                lpsr->state.S = 0;
                THROW(ROLLBACK_ERROR1);
            }
        } else {
            /* the transaction is NOT in prepared state */
            res = PQexec(lpsr->conn, "ROLLBACK");
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_rollback: error while executing "
                            "ROLLBACK command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                lpsr->state.S = 0;
                THROW(ROLLBACK_ERROR2);
            }
            PQclear(res);
            res = NULL;
            lpsr->state.S = 0;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR1:
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case ROLLBACK_ERROR1:
            case ROLLBACK_ERROR2:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_rollback/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_prepare(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , PREPARE_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_prepare\n"));
    TRY {
        struct lixa_pq_status_rm_s *lpsr = NULL;
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;
        const char PREPARE_TRANS_FMT[] = "PREPARE TRANSACTION '%s';";
        char pq_cmd_buf[sizeof(PREPARE_TRANS_FMT) + sizeof(lixa_ser_xid_t)];
        
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_prepare: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_prepare: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
                
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == (lpsr = lixa_pq_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 || lpsr->state.S != 2) {
            LIXA_TRACE(("lixa_pq_prepare: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_prepare: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_ser_xid_serialize(lsx, xid)) {
            LIXA_TRACE(("lixa_pq_prepare: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_ser_xid_serialize(lsx2, &(lpsr->xid));
            LIXA_TRACE(("lixa_pq_prepare: preparing XID '%s' is not the same "
                        "of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_prepare: preparing XID '%s'\n", lsx));

        /* preparing transaction */
        snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), PREPARE_TRANS_FMT, lsx);
        res = PQexec(lpsr->conn, pq_cmd_buf);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_prepare: error while executing "
                        "PREPARE TRANSACTION command (%s)\n",
                        PQerrorMessage(lpsr->conn)));
            /* the resource manager could unilaterally rollback and return
               XA_RBROLLBACK to the transaction manager, but it is leaved
               as a future improvment if necessary */
            THROW(PREPARE_ERROR);
        }
        PQclear(res);
        res = NULL;
        lpsr->state.S = 3;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR1:
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case PREPARE_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_prepare/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_commit(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , COMMIT_ERROR1
                     , COMMIT_ERROR2
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_commit\n"));
    TRY {
        struct lixa_pq_status_rm_s *lpsr = NULL;
        const long valid_flags = TMNOWAIT|TMASYNC|TMONEPHASE|TMNOFLAGS;
        lixa_ser_xid_t lsx;
        int one_phase = flags & TMONEPHASE;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_commit: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOWAIT & flags || TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_commit: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_pq_status_mutex);

        if (NULL == (lpsr = lixa_pq_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 ||
            (lpsr->state.S != 2 && one_phase) ||
            (lpsr->state.S != 3 && !one_phase)) {
            LIXA_TRACE(("lixa_pq_commit: rmid %d state(R,S,T)={%d,%d,%d}, "
                        "one_phase_commit=%d\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T,
                        one_phase));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_commit: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_ser_xid_serialize(lsx, xid)) {
            LIXA_TRACE(("lixa_pq_commit: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_ser_xid_serialize(lsx2, &(lpsr->xid));
            LIXA_TRACE(("lixa_pq_commit: committing XID '%s' is not the "
                        "same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_commit: committing XID '%s'\n", lsx));

        if (lpsr->state.S == 3) {
            const char COMMIT_PREP_FMT[] = "COMMIT PREPARED '%s';";
            char pq_cmd_buf[sizeof(COMMIT_PREP_FMT) + sizeof(lixa_ser_xid_t)];
            /* committing transaction */
            snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), COMMIT_PREP_FMT, lsx);
            res = PQexec(lpsr->conn, pq_cmd_buf);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_commit: error while executing "
                            "COMMIT PREPARED command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                lpsr->state.S = 0;
                THROW(COMMIT_ERROR1);
            }
        } else {
            /* the transaction is NOT in prepared state */
            res = PQexec(lpsr->conn, "COMMIT");
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_commit: error while executing "
                            "COMMIT command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                lpsr->state.S = 0;
                THROW(COMMIT_ERROR2);
            }
            PQclear(res);
            res = NULL;
            lpsr->state.S = 0;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR1:
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case COMMIT_ERROR1:
            case COMMIT_ERROR2:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_pq_status_mutex);
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_commit/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_recover(XID *xids, long count, int rmid, long flags)
{
    return 0;
}



int lixa_pq_forget(XID *xid, int rmid, long flags)
{
    /* this Resource Manager does not heuristically resolve the transactions
       and this function should never be called */
    return XAER_PROTO;
}



int lixa_pq_complete(int *handle, int *retval, int rmid, long flags)
{
    /* asynchronous mode is not supported by this wrapper */
    return XAER_INVAL;
}



PGconn *lixa_pq_get_conn_by_rmid(int rmid)
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
                guint i;
                for (i=0; i<lps->rm->len; ++i) {
                    struct lixa_pq_status_rm_s *lpsr = &g_array_index(
                        lps->rm, struct lixa_pq_status_rm_s, i);
                    if (lpsr->rmid == rmid) {
                        conn = lpsr->conn;
                        break;
                    }
                }
                if (NULL == conn)
                    LIXA_TRACE(("lixa_pq_get_conn_by_rmid: no connection "
                                "found for rmid=%d\n", rmid));
            } else {
                LIXA_TRACE(("lixa_pq_get_conn_by_rmid: no connection "
                            "found to PostgreSQL databases\n"));
            }
        } else {
            LIXA_TRACE(("lixa_pq_get_conn_by_rmid: thread not registered\n"));
        }
    } else {
        LIXA_TRACE(("lixa_pq_get_conn_by_rmid: status is NULL\n"));
    }
    
    /* unlock the mutex */
    g_static_mutex_unlock(&lixa_pq_status_mutex);

    return conn;
}



PGconn *lixa_pq_get_conn_by_pos(int pos)
{
    PGconn *conn = NULL;
    
    /* lock the mutex */
    g_static_mutex_lock(&lixa_pq_status_mutex);

    if (NULL != lixa_pq_status) {
        pthread_t key = pthread_self();
        lixa_pq_status_t *lps = NULL;
        if (NULL != (lps = (lixa_pq_status_t *)g_hash_table_lookup(
                         lixa_pq_status, (gconstpointer)key))) {
            if (pos < lps->rm->len) {
                struct lixa_pq_status_rm_s *lpsr = &g_array_index(
                    lps->rm, struct lixa_pq_status_rm_s, pos);
                conn = lpsr->conn;
            } else {
                LIXA_TRACE(("lixa_pq_get_conn_by_pos: %d exceeds %d, "
                            "the last position\n", pos, lps->rm->len));
            }
        } else {
            LIXA_TRACE(("lixa_pq_get_conn_by_pos: thread not registered\n"));
        }
    } else {
        LIXA_TRACE(("lixa_pq_get_conn_by_pos: status is NULL\n"));
    }
    
    /* unlock the mutex */
    g_static_mutex_unlock(&lixa_pq_status_mutex);

    return conn;
}



PGconn *lixa_pq_get_conn(void)
{
    return lixa_pq_get_conn_by_pos(0);
}
