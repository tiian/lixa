/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
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
#include "liblixapq.h"
/* PostgreSQL XA wrapper provided by LIXA (public header) */
#include "lixapq.h"
/* LIXA standard trace library: it could be removed if "TRACE" calls would
 * be removed from this source */
#include "lixa_trace.h"
/* LIXA convenience macros: it could be removed if "TRY/CATCH" statement
 * would be removed from this source */
#include "lixa_defines.h"
/* LIXA xid: it contains XID serialization/deserialization utilities */
#include "lixa_xid.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA_SWITCH



struct xa_switch_t xapqls = {
    "PostgreSQL[LIXA]",
    TMNOMIGRATE,
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



const gchar *g_module_check_init(GModule *module)
{
    LIXA_TRACE(("lixa_pq/g_module_check_init: initializing module\n"));
    return NULL;
}



void g_module_unload(GModule *module)
{
    LIXA_TRACE(("lixa_pq/g_module_unload: releasing local storage\n"));
    /* lock the status mutex */
    g_mutex_lock(&lixa_sw_status_mutex);

    if (NULL != lixa_sw_status) {
        g_hash_table_destroy(lixa_sw_status);
        lixa_sw_status = NULL;
    }
    
    /* unlock the status mutex */
    g_mutex_unlock(&lixa_sw_status_mutex);
}



int lixa_pq_xid_serialize(const XID *xid, lixa_pq_ser_xid_t lpsx)
{
    gchar *base64_encoded = NULL;
    long len = LIXA_SERIALIZED_LONG_INT /* formatID */ +
        1 /* '_' separator */ +
        xid->gtrid_length*4/3+4 /* gtrid */ +
        1 /* '_' separator */ +
        xid->bqual_length*4/3+4 /* bqual */ +
        1 /* '\0' terminator */ ;
    size_t lpsx_len;
    
    /* check the XID can be serialized for PostgreSQL by this routine */
    if (len > sizeof(lixa_pq_ser_xid_t)) {
        LIXA_TRACE(("lixa_pq_xid_serialize: xid can not be serialized "
                    "because it would need %ld bytes instead of "
                    SIZE_T_FORMAT " (xid->gtrid_length=%ld,"
                    "xid->bqual_length=%ld)\n",
                    len, sizeof(lixa_ser_xid_t),
                    xid->gtrid_length, xid->bqual_length));
        return FALSE;
    }
    /* serialize formatID and put the first separator */
    sprintf(lpsx, "%ld%c", xid->formatID, LIXA_PQ_XID_SEPARATOR);

    /* serialize gtrid using base64 */
    if (NULL == (base64_encoded = g_base64_encode(
                     (const guchar *)&(xid->data[0]),
                     (gsize)xid->gtrid_length))) {
        LIXA_TRACE(("lixa_pq_xid_serialize: xid can not be serialized "
                    "because g_base64_encode returned NULL\n"));
        return FALSE;
    }
    strcat(lpsx, base64_encoded);
    g_free(base64_encoded);
    base64_encoded = NULL;
    
    /* put the second separator */
    lpsx_len = strlen(lpsx);
    lpsx[lpsx_len++] = LIXA_PQ_XID_SEPARATOR;
    lpsx[lpsx_len++] = '\0';

    /* check size */
    if (lpsx_len > len) {
        LIXA_TRACE(("lixa_pq_xid_serialize: current len (" SIZE_T_FORMAT
                    ", '%s') exceeds total len (%ld)\n",
                    lpsx_len, lpsx, len));
        return FALSE;
    }
    
    /* serialize bqual using base64 */
    if (NULL == (base64_encoded = g_base64_encode(
                     (const guchar *)&(xid->data[xid->gtrid_length]),
                     (gsize)xid->bqual_length))) {
        LIXA_TRACE(("lixa_pq_xid_serialize: xid can not be serialized "
                    "because g_base64_encode returned NULL\n"));
        return FALSE;
    }
    strcat(lpsx, base64_encoded);
    g_free(base64_encoded);
    base64_encoded = NULL;
    
    /* check size */
    lpsx_len = strlen(lpsx);
    if (lpsx_len > len) {
        /* this is a possible severe bug: memory corruption happened */
        LIXA_TRACE(("lixa_pq_xid_serialize: current len (" SIZE_T_FORMAT
                    ", '%s') exceeds total len (%ld)\n",
                    lpsx_len, lpsx, len));
        return FALSE;
    }
    
    LIXA_TRACE(("lixa_pq_xid_serialize: '%s'\n", lpsx));
    return TRUE;
}



int lixa_pq_xid_deserialize(XID *xid, const lixa_pq_ser_xid_t lpsx)
{
    enum Exception {
        SEPARATOR1,
        SEPARATOR2,
        G_BASE64_DECODE_ERROR1,
        G_BASE64_DECODE_ERROR2,
        NONE
    } excp;
    int ret_cod = FALSE;
    guchar *base64_decoded = NULL;
    
    LIXA_TRACE(("lixa_pq_xid_deserialize\n"));
    TRY {
        char tmp[LIXA_PQ_XID_SERIALIZE_LENGTH];
        char *first_sep = NULL, *second_sep = NULL;
        size_t formatID_len, gtrid_len, bqual_len;
        gsize base64_decoded_len;
        unsigned long l = 0;
        
        /* discover first separator */
        if (NULL == (first_sep = strchr(lpsx, LIXA_PQ_XID_SEPARATOR))) {
            LIXA_TRACE(("lixa_pq_xid_deserialize: '%s' does "
                        "not contain the first '%c' separator\n", lpsx, 
                        LIXA_PQ_XID_SEPARATOR));
            THROW(SEPARATOR1);
        }

        /* discover second separator */
        if (NULL == (second_sep = strchr(
                         first_sep+1, LIXA_PQ_XID_SEPARATOR))) {
            LIXA_TRACE(("lixa_pq_xid_deserialize: '%s' does "
                        "not contain the second '%c' separator\n", lpsx, 
                        LIXA_PQ_XID_SEPARATOR));
            THROW(SEPARATOR2);
        }
        
        /* retrieve formatID */
        formatID_len = first_sep - lpsx;
        strncpy(tmp, lpsx, formatID_len);
        tmp[formatID_len] = '\0';
        sscanf(tmp, "%ld", &l);
        xid->formatID = l;

        /* retrieve gtrid */
        gtrid_len = second_sep-first_sep;
        strncpy(tmp, first_sep+1, gtrid_len);
        tmp[gtrid_len] = '\0';
        if (NULL == (base64_decoded = g_base64_decode(
                         tmp, &base64_decoded_len))) {
            LIXA_TRACE(("lixa_pq_xid_deserialize: g_base64_decode returned "
                        "NULL while decoding gtrid\n"));
            THROW(G_BASE64_DECODE_ERROR1);
        }
        memcpy(&(xid->data[0]), base64_decoded, base64_decoded_len);
        g_free(base64_decoded);
        base64_decoded = NULL;
        xid->gtrid_length = base64_decoded_len;

        /* retrieve bqual */
        bqual_len = strchr(lpsx, '\0') - second_sep - 1;
        strncpy(tmp, second_sep+1, bqual_len);
        tmp[bqual_len] = '\0';
        if (NULL == (base64_decoded = g_base64_decode(
                         tmp, &base64_decoded_len))) {
            LIXA_TRACE(("lixa_pq_xid_deserialize: g_base64_decode returned "
                        "NULL while decoding bqual\n"));
            THROW(G_BASE64_DECODE_ERROR2);
        }
        memcpy(&(xid->data[xid->gtrid_length]), base64_decoded,
               base64_decoded_len);
        g_free(base64_decoded);
        base64_decoded = NULL;
        xid->bqual_length = base64_decoded_len;
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SEPARATOR1:
            case SEPARATOR2:
            case G_BASE64_DECODE_ERROR1:
            case G_BASE64_DECODE_ERROR2:
                ret_cod = FALSE;
                break;
            case NONE:
                ret_cod = TRUE;
                break;
            default:
                ret_cod = FALSE;
        } /* switch (excp) */
        /* memory recovery if necessary */
        if (NULL != base64_decoded) {
            g_free(base64_decoded);
            base64_decoded = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_xid_deserialize/excp=%d/ret_cod=%d\n",
                excp, ret_cod));
    return ret_cod;
}



int lixa_pq_open(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , G_HASH_TABLE_NEW_FULL_ERROR
                     , MALLOC_ERROR
                     , RM_TYPE_ERROR
                     , PQ_CONNECTDB_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;

    PGconn           *conn = NULL;
    lixa_sw_status_t *lps = NULL;
    
    LIXA_TRACE(("lixa_pq_open: xa_info='%s', rmid=%d, flags=%ld\n",
                xa_info, rmid, flags));
    TRY {
        pthread_t key = pthread_self();
        guint i;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        int insert_in_lixa_sw_status = FALSE;
        
        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }
        
        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_open: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        if (NULL == lixa_sw_status) {
            /* the status structure must be initialized from scratch */
            if (NULL == (lixa_sw_status = g_hash_table_new_full(
                             g_direct_hash, g_direct_equal, NULL,
                             lixa_sw_status_destroy))) {
                LIXA_TRACE(("lixa_pq_open/g_hash_table_new_full: unable to "
                            "allocate hash table status\n"));
                THROW(G_HASH_TABLE_NEW_FULL_ERROR);
            }
        }

        /* look for this thread status */
        if (NULL == (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                         lixa_sw_status, (gconstpointer)key))) {
            LIXA_TRACE(("lixa_sw_open: status for thread " PTHREAD_T_FORMAT
                        " not found, allocating it...\n", key));
            if (NULL == (lps = (lixa_sw_status_t *)malloc(
                             sizeof(lixa_sw_status_t)))) {
                LIXA_TRACE(("lixa_pq_open: unable to allocate %u bytes\n",
                            sizeof(lixa_sw_status_t)));
                THROW(MALLOC_ERROR);
            }
            lixa_sw_status_init(lps);
            insert_in_lixa_sw_status = TRUE;
        }

        /* check if rmid is already connected */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_sw_status_rm_s, i);
            if (rmid == lpsr->rmid) {
                if (LIXA_SW_STATUS_RM_TYPE_POSTGRESQL != lpsr->rm_type) {
                    LIXA_TRACE(("lixa_pq_open: internal error {lpsr->rmid=%d, "
                                "lpsr->rm_type=%d} (expected value %d)\n",
                                lpsr->rmid, lpsr->rm_type,
                                LIXA_SW_STATUS_RM_TYPE_POSTGRESQL));
                    THROW(RM_TYPE_ERROR);
                }
                conn = (PGconn *)lpsr->conn;
                break;
            }
        }

        if (NULL == conn) {
            /* create a new connection */
            struct lixa_sw_status_rm_s lpsr;
            lixa_sw_status_rm_init(&lpsr);
            conn = PQconnectdb(xa_info);
            if (CONNECTION_OK != PQstatus(conn)) {
                LIXA_TRACE(("lixa_pq_open: error while connecting to the "
                            "database: %s", PQerrorMessage(conn)));
                PQfinish(conn);
                THROW(PQ_CONNECTDB_ERROR);
            }
            /* save the connection for this thread/rmid */
            lpsr.rmid = rmid;
            lpsr.rm_type = LIXA_SW_STATUS_RM_TYPE_POSTGRESQL;
            lpsr.state.R = 1;
            lpsr.conn = (gpointer)conn;
            g_array_append_val(lps->rm, lpsr);
            if (insert_in_lixa_sw_status)
                g_hash_table_insert(lixa_sw_status, (gpointer)key,
                                    (gpointer)lps);
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
            case RM_TYPE_ERROR:
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
        g_mutex_unlock(&lixa_sw_status_mutex);
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
        lixa_sw_status_t *lps = NULL;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        
        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }

        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_close: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        if (NULL == lixa_sw_status) {
            /* the status structure does not exist, this is a dummy xa_close */
            THROW(NOTHING_TO_DO1);
        }
            
        if (NULL == (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                         lixa_sw_status, (gconstpointer)key))) {
            /* the status structure does not contain a record for this
               thread, this is a dummy xa_close */
            THROW(NOTHING_TO_DO2);
        }

        /* check the state of the resource manager */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_sw_status_rm_s, i);
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
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_close/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_start(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE} excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_start\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);
        
        xa_rc = lixa_pq_start_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_start/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_start_core(struct lixa_sw_status_rm_s *lpsr,
                       const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , BEGIN_ERROR
                     , NONE} excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_start_core\n"));
    TRY {
        const long valid_flags = TMJOIN|TMRESUME|TMNOWAIT|TMASYNC|TMNOFLAGS;
        lixa_ser_xid_t lsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_end: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOFLAGS != flags) {
            LIXA_TRACE(("lixa_pq_start_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 ||
            (lpsr->state.S != 0 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_pq_start_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_start_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_pq_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_pq_start_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }
        LIXA_TRACE(("lixa_pq_start_core: starting XID '%s'\n", lsx));

        /* saving xid */
        lpsr->xid = *xid;
        /* starting transaction */
        res = PQexec(lpsr->conn, "BEGIN");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_start_core: error while executing BEGIN "
                        "command (%s)\n", PQerrorMessage(lpsr->conn)));
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
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_start_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_end(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_end\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_pq_end_core(lpsr, xid, rmid, flags);
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_end/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_end_core(struct lixa_sw_status_rm_s *lpsr,
                     const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , ROLLBACK_ONLY
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_end_core\n"));
    TRY {
        const long valid_flags = TMSUSPEND|TMMIGRATE|TMSUCCESS|TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_end_core: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if ((TMSUSPEND|TMMIGRATE|TMASYNC) & flags) {
            LIXA_TRACE(("lixa_pq_end_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
                
        if (lpsr->state.R != 1 || lpsr->state.T != 1 ||
            (lpsr->state.S != 1 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_pq_end_core: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_end_core: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_pq_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_pq_end_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_pq_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_pq_end_core: ending XID '%s' is not the same "
                        "of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_end_core: ending XID '%s'\n", lsx));

        /* check if the TM marked as failed the transaction */
        if (TMFAIL & flags) {
            LIXA_TRACE(("lixa_pq_end_core: TMFAIL detected, entering "
                        "'Rollback Only' state\n"));
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
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_end_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_rollback(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_rollback\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);
        
        xa_rc = lixa_pq_rollback_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_rollback/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_rollback_core(struct lixa_sw_status_rm_s *lpsr,
                          const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , SELECT_ERROR
                     , XID_NOT_AVAILABLE
                     , PROTOCOL_ERROR3
                     , XID_MISMATCH
                     , ROLLBACK_ERROR1
                     , ROLLBACK_ERROR2
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_rollback_core\n"));
    TRY {
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_rollback_core: invalid flag in 0x%x\n",
                        flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_rollback_core: flags 0x%x are not "
                        "supported\n", flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0) {
            LIXA_TRACE(("lixa_pq_rollback_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }
        
        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_rollback_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_pq_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_pq_rollback_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        if (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4) {
            const char SELECT_FMT[] = "SELECT COUNT(*) FROM pg_prepared_xacts "
                "WHERE gid = '%s';";
            char select[sizeof(SELECT_FMT) + sizeof(lsx)];
            /* check the database state, it could be a prepared transaction */
            snprintf(select, sizeof(select), SELECT_FMT, lsx);
            res = PQexec(lpsr->conn, select);
            if (PGRES_TUPLES_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_rollback_core: error while executing "
                            "'%s' command (%d/%s)\n", select,
                            PQresultStatus(res), PQerrorMessage(lpsr->conn)));
                THROW(SELECT_ERROR);
            }
            if (PQntuples(res) == 1) {
                long count = strtol(PQgetvalue(res, 0, 0), NULL, 10);
                if (count > 0) {
                    /* it's a prepared transaction */
                    lpsr->state.S = 3;
                    lpsr->xid = *xid;
                } else {
                    PQclear(res);
                    res = NULL;
                    LIXA_TRACE(("lixa_pq_rollback_core: xid '%s' is not "
                                "available\n", lsx));
                    THROW(XID_NOT_AVAILABLE);
                }
            }
            PQclear(res);
            res = NULL;
        }

        if (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4) {
            LIXA_TRACE(("lixa_pq_rollback_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR3);
        }
        
        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_pq_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_pq_rollback_core: rolling back XID '%s' is not "
                        "the same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_rollback_core: rolling back XID '%s'\n", lsx));

        if (lpsr->state.S == 3) {
            const char ROLLBACK_PREP_FMT[] = "ROLLBACK PREPARED '%s';";
            char pq_cmd_buf[sizeof(ROLLBACK_PREP_FMT) +
                            sizeof(lixa_ser_xid_t)];
            lpsr->state.S = 0;
            /* rolling back the transaction */
            snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), ROLLBACK_PREP_FMT, lsx);
            res = PQexec(lpsr->conn, pq_cmd_buf);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_rollback_core: error while executing "
                            "ROLLBACK PREPARED command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                THROW(ROLLBACK_ERROR1);
            }
        } else {
            lpsr->state.S = 0;
            /* the transaction is NOT in prepared state */
            res = PQexec(lpsr->conn, "ROLLBACK");
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_rollback_core: error while executing "
                            "ROLLBACK command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                THROW(ROLLBACK_ERROR2);
            }
            PQclear(res);
            res = NULL;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
            case PROTOCOL_ERROR3:
                xa_rc = XAER_PROTO;
                break;
            case XID_NOT_AVAILABLE:
                xa_rc = XAER_NOTA;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case SELECT_ERROR:
                xa_rc = XAER_RMERR;
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
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_rollback_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_prepare(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_prepare\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        
        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);
        
        xa_rc = lixa_pq_prepare_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_prepare/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_prepare_core(struct lixa_sw_status_rm_s *lpsr,
                         const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , PREPARE_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_prepare_core\n"));
    TRY {
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;
        const char PREPARE_TRANS_FMT[] = "PREPARE TRANSACTION '%s';";
        char pq_cmd_buf[sizeof(PREPARE_TRANS_FMT) + sizeof(lixa_ser_xid_t)];
                
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_prepare_core: invalid flag in 0x%x\n",
                        flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_prepare_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
                
        if (lpsr->state.R != 1 || lpsr->state.T != 0 || lpsr->state.S != 2) {
            LIXA_TRACE(("lixa_pq_prepare_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_prepare_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_pq_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_pq_prepare_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_pq_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_pq_prepare_core: preparing XID '%s' is not the "
                        "same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_prepare_core: preparing XID '%s'\n", lsx));

        /* preparing transaction */
        snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), PREPARE_TRANS_FMT, lsx);
        res = PQexec(lpsr->conn, pq_cmd_buf);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_prepare_core: error while executing "
                        "PREPARE TRANSACTION command (%d/%s)\n",
                        PQresultStatus(res),
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
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_prepare_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_commit(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_commit\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);
                
        xa_rc = lixa_pq_commit_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_commit/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_commit_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , SELECT_ERROR
                     , XID_NOT_AVAILABLE
                     , PROTOCOL_ERROR3
                     , XID_MISMATCH
                     , COMMIT_ERROR1
                     , COMMIT_ERROR2
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_commit_core\n"));
    TRY {
        const long valid_flags = TMNOWAIT|TMASYNC|TMONEPHASE|TMNOFLAGS;
        lixa_ser_xid_t lsx;
        int one_phase = flags & TMONEPHASE;
        
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_commit_core: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOWAIT & flags || TMASYNC & flags) {
            LIXA_TRACE(("lixa_pq_commit_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0) {
            LIXA_TRACE(("lixa_pq_commit_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_commit_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_pq_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_pq_commit_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        if (lpsr->state.S != 3 && !one_phase) {
            const char SELECT_FMT[] = "SELECT COUNT(*) FROM pg_prepared_xacts "
                "WHERE gid = '%s';";
            char select[sizeof(SELECT_FMT) + sizeof(lsx)];
            /* check the database state, it could be a prepared transaction */
            snprintf(select, sizeof(select), SELECT_FMT, lsx);
            res = PQexec(lpsr->conn, select);
            if (PGRES_TUPLES_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_commit_core: error while executing "
                            "'%s' command (%d/%s)\n", select,
                            PQresultStatus(res), PQerrorMessage(lpsr->conn)));
                THROW(SELECT_ERROR);
            }
            if (PQntuples(res) == 1) {
                long count = strtol(PQgetvalue(res, 0, 0), NULL, 10);
                if (count > 0) {
                    /* it's a prepared transaction */
                    lpsr->state.S = 3;
                    lpsr->xid = *xid;
                } else {
                    PQclear(res);
                    res = NULL;
                    LIXA_TRACE(("lixa_pq_commit_core: xid '%s' is not "
                                "available\n", lsx));
                    THROW(XID_NOT_AVAILABLE);
                }
            }
            PQclear(res);
            res = NULL;
        }

        if ((lpsr->state.S != 2 && one_phase) ||
            (lpsr->state.S != 3 && !one_phase)) {
            LIXA_TRACE(("lixa_pq_commit_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}, one_phase_commit=%d\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T,
                        one_phase));
            THROW(PROTOCOL_ERROR3);
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_pq_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_pq_commit_core: committing XID '%s' is not the "
                        "same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_pq_commit_core: committing XID '%s'\n", lsx));

        if (lpsr->state.S == 3) {
            const char COMMIT_PREP_FMT[] = "COMMIT PREPARED '%s';";
            char pq_cmd_buf[sizeof(COMMIT_PREP_FMT) + sizeof(lixa_ser_xid_t)];
            lpsr->state.S = 0;
            /* committing transaction */
            snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), COMMIT_PREP_FMT, lsx);
            res = PQexec(lpsr->conn, pq_cmd_buf);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_commit_core: error while executing "
                            "COMMIT PREPARED command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                THROW(COMMIT_ERROR1);
            }
        } else {
            lpsr->state.S = 0;
            /* the transaction is NOT in prepared state */
            res = PQexec(lpsr->conn, "COMMIT");
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_commit_core: error while executing "
                            "COMMIT command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                lpsr->state.S = 0;
                THROW(COMMIT_ERROR2);
            }
            PQclear(res);
            res = NULL;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
            case PROTOCOL_ERROR3:
                xa_rc = XAER_PROTO;
                break;
            case XID_NOT_AVAILABLE:
                xa_rc = XAER_NOTA;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case SELECT_ERROR:
                xa_rc = XAER_RMERR;
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
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_commit_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_recover(XID *xids, long count, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_pq_recover\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);
        
        xa_rc = lixa_pq_recover_core(lpsr, xids, count, rmid, flags);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_recover/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_recover_core(struct lixa_sw_status_rm_s *lpsr,
                         XID *xids, long count, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , INVALID_OPTIONS1
                     , INVALID_OPTIONS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , BEGIN_ERROR
                     , OPEN_CURSOR_ERROR
                     , FETCH_ERROR
                     , CLOSE_CURSOR_ERROR
                     , END_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    long out_count = 0;
    PGresult *res = NULL;
    
    LIXA_TRACE(("lixa_pq_recover_core\n"));
    TRY {
        const long valid_flags = TMSTARTRSCAN|TMENDRSCAN|TMNOFLAGS;
        const char *CLOSE_CURSOR = "CLOSE lixa_pq_recover_core_cursor;";
        const char OPEN_CURSOR_FMT[] = "DECLARE lixa_pq_recover_core_cursor "
            "CURSOR "
            "FOR SELECT gid FROM pg_prepared_xacts ORDER BY prepared;";
        char open_cursor[sizeof(OPEN_CURSOR_FMT)];
        const char *BEGIN = "BEGIN;";
        const char *END = "END;";
        const char FETCH_COUNT_FMT[] = "FETCH %ld FROM "
            "lixa_pq_recover_core_cursor;";
        char fetch_count[sizeof(FETCH_COUNT_FMT) + sizeof(long)*3];
        int row;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_pq_recover_core: invalid flag in 0x%x\n",
                        flags));
            THROW(INVALID_FLAGS);
        }

        if ((NULL == xids && count > 0) || (count < 0)) {
            LIXA_TRACE(("lixa_pq_recover_core: xids=%p, count=%ld\n",
                        xids, count));
            THROW(INVALID_OPTIONS1);
        }
        
        if (!(flags & TMSTARTRSCAN)) {
            LIXA_TRACE(("lixa_pq_recover_core: TMSTARTRSCAN flag must be "
                        "set\n"));
            THROW(INVALID_OPTIONS2);
        }
        
        if (lpsr->state.R != 1) {
            LIXA_TRACE(("lixa_pq_recover_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_pq_recover_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* open the cursor if necessary */
        if (flags & TMSTARTRSCAN) {
            LIXA_TRACE(("lixa_pq_recover_core: cursor is not open, "
                        "opening it\n"));
            res = PQexec(lpsr->conn, BEGIN);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_recover_core: error while executing '%s' "
                            "command (%s)\n", BEGIN,
                            PQerrorMessage(lpsr->conn)));
                THROW(BEGIN_ERROR);
            }
            PQclear(res);
            res = NULL;
            snprintf(open_cursor, sizeof(open_cursor), OPEN_CURSOR_FMT);
            LIXA_TRACE(("lixa_pq_recover_core: '%s'\n", open_cursor));
            res = PQexec(lpsr->conn, open_cursor);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_pq_recover_core: error while executing '%s' "
                            "command (%s)\n", open_cursor,
                            PQerrorMessage(lpsr->conn)));
                THROW(OPEN_CURSOR_ERROR);
            }
            PQclear(res);
            res = NULL;
        }

        /* fetching records */
        snprintf(fetch_count, sizeof(fetch_count), FETCH_COUNT_FMT, count);
        res = PQexec(lpsr->conn, fetch_count);
        if (PGRES_TUPLES_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_recover_core: error while executing "
                        "'%s' command (%d/%s)\n", fetch_count,
                        PQresultStatus(res),
                        PQerrorMessage(lpsr->conn)));
            THROW(FETCH_ERROR);
        }
        for (row=0; row<PQntuples(res); ++row) {
            XID xid;            
            if (lixa_pq_xid_deserialize(&xid, PQgetvalue(res, row, 0))) {
                LIXA_TRACE(("lixa_pq_recover_core: xids[%d]='%s'\n", out_count,
                            PQgetvalue(res, row, 0)));
                xids[out_count++] = xid;
            }
        }
        PQclear(res);
        res = NULL;
        
        /* close the cursor if necessary */
        LIXA_TRACE(("lixa_pq_recover_core: TMENDRSCAN is set, closing it\n"));
        res = PQexec(lpsr->conn, CLOSE_CURSOR);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_recover_core: error while executing '%s' "
                        "command (%s)\n", CLOSE_CURSOR,
                        PQerrorMessage(lpsr->conn)));
            THROW(CLOSE_CURSOR_ERROR);
        }
        PQclear(res);
        res = NULL;
        res = PQexec(lpsr->conn, END);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_pq_recover_core: error while executing '%s' "
                        "command (%s)\n", END,
                        PQerrorMessage(lpsr->conn)));
            THROW(END_ERROR);
        }
        PQclear(res);
        res = NULL;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
            case INVALID_OPTIONS1:
            case INVALID_OPTIONS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case BEGIN_ERROR:
            case OPEN_CURSOR_ERROR:
            case FETCH_ERROR:
                xa_rc = XAER_RMFAIL;
                break;
            case CLOSE_CURSOR_ERROR:
            case END_ERROR:
                xa_rc = XAER_RMFAIL;
                break;
            case NONE:
                xa_rc = out_count;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        if (NULL != res)
            PQclear(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_pq_recover_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_pq_forget(const XID *xid, int rmid, long flags)
{
    /* this Resource Manager does not heuristically resolve the transactions
       and this function should never be called */
    return XAER_PROTO;
}



int lixa_pq_forget_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags)
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



PGconn *lixa_pq_get_conn_by_rmid(int rmid) {
    return (PGconn *)lixa_sw_get_conn_by_rmid(
        rmid, LIXA_SW_STATUS_RM_TYPE_POSTGRESQL);
}



PGconn *lixa_pq_get_conn_by_pos(int pos) {
    return lixa_sw_get_conn_by_pos(pos, LIXA_SW_STATUS_RM_TYPE_POSTGRESQL);
}



PGconn *lixa_pq_get_conn(void) {
    return lixa_sw_get_conn_by_pos(0, LIXA_SW_STATUS_RM_TYPE_POSTGRESQL);
}



int lixa_pq_is_managed_conn(const PGconn *conn) {
    return lixa_sw_is_managed_conn((gpointer)conn,
                                   LIXA_SW_STATUS_RM_TYPE_POSTGRESQL);
}



#ifdef HAVE_GNU_COBOL
PGconn *LIXAPQGETCONN() {
    PGconn *conn;
    LIXA_TRACE_INIT;
    LIXA_TRACE(("LIXAPQGETCONN\n"));
    conn = lixa_pq_get_conn();
    LIXA_TRACE(("LIXAPQGETCONN/PGCONN=%p\n", conn));
    return conn;
}



PGconn *LIXAPQGETCONNBYRMID(int rmid) {
    PGconn *conn;
    LIXA_TRACE_INIT;
    LIXA_TRACE(("LIXAPQGETCONNBYRMID/rmid=%d\n", rmid));
    conn = lixa_pq_get_conn_by_rmid(rmid);
    LIXA_TRACE(("LIXAPQGETCONNBYRMID/PGCONN=%p\n", conn));
    return conn;
}



PGconn *LIXAPQGETCONNBYPOS(int pos) {
    PGconn *conn;
    LIXA_TRACE_INIT;
    LIXA_TRACE(("LIXAPQGETCONNBYPOS/pos=%d\n", pos));
    conn = lixa_pq_get_conn_by_pos(pos);
    LIXA_TRACE(("LIXAPQGETCONNBYPOS/PGCONN=%p\n", conn));
    return conn;
}
#endif /* HAVE_GNU_COBOL */
