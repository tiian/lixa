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
/* MySQL front-end */
#include <mysql.h>
/* MySQL XA wrapper provided by LIXA (private header) */
#include <liblixamy.h>
/* MySQL XA wrapper provided by LIXA (public header) */
#include <lixamy.h>
/* LIXA standard trace library: it could be removed if "TRACE" calls would
 * be removed from this source */
#include <lixa_trace.h>
/* LIXA convenience macros: it could be removed if "TRY/CATCH" statement
 * would be removed from this source */
#include <lixa_defines.h>
/* LIXA xid: it contains XID serialization/deserialization utilities */
#include <lixa_xid.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



struct xa_switch_t xapqls = {
    "MySQL[LIXA]",
    TMNOFLAGS,
    0,
    lixa_my_open,
    lixa_my_close,
    lixa_my_start,
    lixa_my_end,
    lixa_my_rollback,
    lixa_my_prepare,
    lixa_my_commit,
    lixa_my_recover,
    lixa_my_forget,
    lixa_my_complete
};



const gchar *g_module_check_init(GModule *module)
{
    LIXA_TRACE(("lixa_my/g_module_check_init: initializing module\n"));
    return NULL;
}



void g_module_unload(GModule *module)
{
    LIXA_TRACE(("lixa_my/g_module_unload: releasing local storage\n"));
    /* lock the status mutex */
    g_static_mutex_lock(&lixa_sw_status_mutex);

    if (NULL != lixa_sw_status) {
        g_hash_table_destroy(lixa_sw_status);
        lixa_sw_status = NULL;
    }
    
    /* unlock the status mutex */
    g_static_mutex_unlock(&lixa_sw_status_mutex);
}



int lixa_my_parse_xa_info(const char *xa_info,
                          struct lixa_mysql_real_connect_s *lmrc)
{
    enum Exception { BUFFER_OVERFLOW
                     , SYNTAX_ERROR1
                     , PARSE_KEY_VALUE1
                     , SYNTAX_ERROR2
                     , INTERNAL_ERROR
                     , PARSE_KEY_VALUE2
                     , NONE } excp;
    int xa_rc = XAER_RMERR;
    size_t i = 0, l;
    char key[MAXINFOSIZE], value[MAXINFOSIZE];
    int k, v;
    enum State { ST_KEY, ST_VALUE } state;
    enum Token { TK_SEPAR, TK_ASSIGN, TK_CHAR } token;
    
    LIXA_TRACE(("lixa_my_parse_xa_info\n"));
    TRY {
        /* check the string is not a buffer overflow attack */
        if (MAXINFOSIZE <= (l = strlen(xa_info))) {
            LIXA_TRACE(("lixa_my_parse_xa_info: xa_info is too long ("
                        SIZE_T_FORMAT ")\n", l));
            THROW(BUFFER_OVERFLOW);
        }
        LIXA_TRACE(("lixa_my_parse_xa_info: parsing '%s'\n", xa_info));
        /* reset the struct */
        memset(lmrc, 0, sizeof(struct lixa_mysql_real_connect_s));
        lmrc->host = lmrc->user = lmrc->passwd = lmrc->db = lmrc->unix_socket =
            NULL;
        state = ST_KEY;
        k = v = 0;
        while (i<l) {
            char current, next;
            int escaping = FALSE;
            current = xa_info[i];
            if (i<l-1)
                next = xa_info[i+1];
            else
                next = '\0';
            /* recognize double separator or double assignator (they are
               escaping sequences) */
            if ((LIXA_MYSQL_XA_INFO_SEPARATOR == current &&
                 LIXA_MYSQL_XA_INFO_SEPARATOR == next) ||
                (LIXA_MYSQL_XA_INFO_ASSIGN == current &&
                 LIXA_MYSQL_XA_INFO_ASSIGN == next))
                escaping = TRUE;
            token = TK_CHAR;
            if (!escaping) {
                if (LIXA_MYSQL_XA_INFO_SEPARATOR == current)
                    token = TK_SEPAR;
                else if (LIXA_MYSQL_XA_INFO_ASSIGN == current)
                    token = TK_ASSIGN;
            }
            switch (token) {
                case TK_SEPAR:
                    if (ST_KEY == state) {
                        LIXA_TRACE(("lixa_my_parse_xa_info: separator '%c' "
                                    "unexpected while parsing a KEY at pos "
                                    SIZE_T_FORMAT " of xa_info '%s'\n",
                                    current, i, xa_info));
                        THROW(SYNTAX_ERROR1);
                    }
                    /* value terminated */
                    value[v] = '\0';
                    state = ST_KEY;
                    k = 0;
                    LIXA_TRACE(("lixa_my_parse_xa_info: (key=value) -> "
                                "(%s=%s)\n", key, value));
                    /* registering key/value in broken down structure */
                    if (XA_OK != (xa_rc = lixa_my_parse_key_value(
                                      lmrc, key, value)))
                        THROW(PARSE_KEY_VALUE1);
                    /* resetting... */
                    key[0] = value[0] = '\0';
                    break;
                case TK_ASSIGN:
                    if (ST_VALUE == state) {
                        LIXA_TRACE(("lixa_my_parse_xa_info: separator '%c' "
                                    "unexpected while parsing a VALUE at pos "
                                    SIZE_T_FORMAT " of xa_info '%s'\n",
                                    current, i, xa_info));
                        THROW(SYNTAX_ERROR2);
                    }
                    /* value terminated */
                    key[k] = '\0';
                    state = ST_VALUE;
                    v = 0;
                    break;
                case TK_CHAR:
                    if (ST_KEY == state) {
                        key[k++] = current;
                        key[k] = '\0'; /* debugging purpose */
                    } else if (ST_VALUE == state) {
                        value[v++] = current;
                        value[v] = '\0'; /* debugging purpose */
                    } else {
                        LIXA_TRACE(("lixa_my_parse_xa_info: unexpected state "
                                    "(%d) while parsing '%c' at pos "
                                    SIZE_T_FORMAT " of xa_info '%s'\n",
                                    state, current, i, xa_info));
                        THROW(INTERNAL_ERROR);
                    }
                default:
                    break;
            } /* switch (token) */
            /* shift one (or two) char(s) */
            if (escaping)
                i = i+2;
            else
                i++;
        }
        LIXA_TRACE(("lixa_my_parse_xa_info: (key=value) -> "
                    "(%s=%s)\n", key, value));
        /* registering key/value in broken down structure */
        if (XA_OK != (xa_rc = lixa_my_parse_key_value(lmrc, key, value)))
            THROW(PARSE_KEY_VALUE2);
        /* display brokendown structure */
        LIXA_TRACE(("lixa_my_parse_xa_info: lmrc[host=%s,user=%s,passwd=%s,"
                    "db=%s,port=%d,unix_socket=%s,client_flag=%ld\n",
                    lmrc->host != NULL ? lmrc->host : "NULL",
                    lmrc->user != NULL ? lmrc->user : "NULL",
                    lmrc->passwd != NULL ? lmrc->passwd : "NULL",
                    lmrc->db != NULL ? lmrc->db : "NULL",
                    lmrc->port,
                    lmrc->unix_socket != NULL ? lmrc->unix_socket : "NULL",
                    lmrc->client_flag));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_OVERFLOW:
            case SYNTAX_ERROR1:
            case SYNTAX_ERROR2:
                xa_rc = XAER_INVAL;
                break;
            case PARSE_KEY_VALUE1:
            case PARSE_KEY_VALUE2:
                break;
            case INTERNAL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_parse_xa_info/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_parse_key_value(struct lixa_mysql_real_connect_s *lmrc,
                            const char *key, const char *value)
{
    enum Exception { KEY_NOT_RECOGNIZED
                     , INTERNAL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMERR;
    enum Key { KY_HOST, KY_USER, KY_PASSWD, KY_DB, KY_PORT, KY_UNIX_SOCKET,
               KY_CLIENT_FLAG, KY_NULL } ky;
    
    LIXA_TRACE(("lixa_my_parse_key_value\n"));
    TRY {
        /* recognize key */
        if (!strcmp(key, "host"))
            ky = KY_HOST;
        else if (!strcmp(key, "user"))
            ky = KY_USER;
        else if (!strcmp(key, "passwd"))
            ky = KY_PASSWD;
        else if (!strcmp(key, "db"))
            ky = KY_DB;
        else if (!strcmp(key, "port"))
            ky = KY_PORT;
        else if (!strcmp(key, "unix_socket"))
            ky = KY_UNIX_SOCKET;
        else if (!strcmp(key, "client_flag"))
            ky = KY_CLIENT_FLAG;
        else if (0 == strlen(key))
            ky = KY_NULL;
        else {
            LIXA_TRACE(("lixa_my_parse_key_value: key='%s' is not recognized"
                        "\n", key));
            THROW(KEY_NOT_RECOGNIZED);
        }
        /* move in broken down structure */
        switch (ky) {
            case KY_HOST:
                strcpy(lmrc->host_buffer, value);
                lmrc->host = lmrc->host_buffer;
                break;
            case KY_USER:
                strcpy(lmrc->user_buffer, value);
                lmrc->user = lmrc->user_buffer;
                break;
            case KY_PASSWD:
                strcpy(lmrc->passwd_buffer, value);
                lmrc->passwd = lmrc->passwd_buffer;
                break;
            case KY_DB:
                strcpy(lmrc->db_buffer, value);
                lmrc->db = lmrc->db_buffer;
                break;
            case KY_PORT:
                lmrc->port = strtoul(value, NULL, 0);
                break;
            case KY_UNIX_SOCKET:
                strcpy(lmrc->unix_socket_buffer, value);
                lmrc->unix_socket = lmrc->unix_socket_buffer;
                break;
            case KY_CLIENT_FLAG:
                lmrc->client_flag = strtoul(value, NULL, 0);
                break;
            case KY_NULL:
                LIXA_TRACE(("lixa_my_parse_key_value: WARNING there is a null "
                            "key in xa_open_info string\n"));
                break;
            default:
                LIXA_TRACE(("lixa_my_parse_key_value: ky=%d invalid KY_ value"
                            "\n", ky));
                THROW(INTERNAL_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case KEY_NOT_RECOGNIZED:
                xa_rc = XAER_INVAL;
                break;
            case INTERNAL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_parse_key_value/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}


    
char *lixa_my_xid_serialize(const XID *xid)
{
    lixa_ser_xid_t lsx;
    char *gtrid, *bqual, *ser_xid;
    size_t size;
    
    LIXA_TRACE(("lixa_my_xid_serialize\n"));
    /* specific MySQL serialization */
    if (NULL == (gtrid = lixa_xid_get_gtrid_ascii(xid)))
        return NULL;
    if (NULL == (bqual = lixa_xid_get_bqual_ascii(xid))) {
        free(gtrid);
        return NULL;
    }
    size = strlen(gtrid) + strlen(bqual) + 30;
    if (NULL == (ser_xid = malloc(size))) {
        free(bqual);
        return NULL;
    }
    if (size <= snprintf(ser_xid, size, "'%s','%s',%lu",
                         gtrid, bqual, xid->formatID)) {
        free(ser_xid);
        return NULL;
    }
    return ser_xid;
}



int lixa_my_open(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , G_HASH_TABLE_NEW_FULL_ERROR
                     , MALLOC_ERROR
                     , PARSE_ERROR
                     , MYSQL_INIT_ERROR
                     , MYSQL_REAL_CONNECT_ERROR
                     , PQ_CONNECTDB_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMERR;

    MYSQL            *conn = NULL;
    lixa_sw_status_t *lps = NULL;
    
    LIXA_TRACE(("lixa_my_open: xa_info='%s', rmid=%d, flags=%ld\n",
                xa_info, rmid, flags));
    TRY {
        pthread_t key = pthread_self();
        guint i;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        struct lixa_mysql_real_connect_s lmrc;
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }
        
        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_open: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        if (NULL == lixa_sw_status) {
            /* the status structure must be initialized from scratch */
            if (NULL == (lixa_sw_status = g_hash_table_new_full(
                             g_direct_hash, g_direct_equal, NULL,
                             lixa_sw_status_destroy))) {
                LIXA_TRACE(("lixa_my_open/g_hash_table_new_full: unable to "
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
                LIXA_TRACE(("lixa_my_open: unable to allocate %u bytes\n",
                            sizeof(lixa_sw_status_t)));
                THROW(MALLOC_ERROR);
            }
            lixa_sw_status_init(lps);
        }

        /* check if rmid is already connected */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_sw_status_rm_s, i);
            if (lpsr->rmid == rmid) {
                conn = (MYSQL *)lpsr->conn;
                break;
            }
        }

        if (NULL == conn) {
            /* create a new connection */
            struct lixa_sw_status_rm_s lpsr;
            lixa_sw_status_rm_init(&lpsr);

            if (XA_OK != (xa_rc = lixa_my_parse_xa_info(xa_info, &lmrc)))
                THROW(PARSE_ERROR);
        
            if (NULL == (conn = mysql_init(NULL))) {
                LIXA_TRACE(("lixa_my_open: mysql_init(NULL) returned NULL\n"));
                THROW(MYSQL_INIT_ERROR);
            }

            if (NULL == mysql_real_connect(
                    conn, lmrc.host, lmrc.user, lmrc.passwd, lmrc.db,
                    lmrc.port, lmrc.unix_socket, lmrc.client_flag)) {
                LIXA_TRACE(("lixa_my_open: mysql_real_connect returned error: "
                            "%u: %s\n", mysql_errno(conn), mysql_error(conn)));
                mysql_close(conn);
                THROW(MYSQL_REAL_CONNECT_ERROR);
            }
            
            /* save the connection for this thread/rmid */
            lpsr.rmid = rmid;
            lpsr.state.R = 1;
            lpsr.conn = (gpointer)conn;
            g_array_append_val(lps->rm, lpsr);
            g_hash_table_insert(lixa_sw_status, (gpointer)key, (gpointer)lps);
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
            case PARSE_ERROR:
                break;
            case MYSQL_INIT_ERROR:
            case MYSQL_REAL_CONNECT_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
        if (NONE != excp && NULL != lps)
            free(lps);
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_open/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_close(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , NOTHING_TO_DO1
                     , NOTHING_TO_DO2
                     , PROTOCOL_ERROR
                     , NOTHING_TO_DO3
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_close\n"));
    TRY {
        guint             i;
        pthread_t         key = pthread_self();
        lixa_sw_status_t *lps = NULL;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }

        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_close: TMASYNC flag is not supported\n"));
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
                    LIXA_TRACE(("lixa_my_close: state is T%d, S%d and "
                                "xa_close() cannot be called\n",
                                lpsr->state.T, lpsr->state.S));
                    THROW(PROTOCOL_ERROR);
                }
                /* state found, checking "Un-initialized" state */
                if (lpsr->state.R == 0)
                    THROW(NOTHING_TO_DO3);
                /* closing database connection if any */
                if (NULL != lpsr->conn) {
                    mysql_close((MYSQL *)lpsr->conn);
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
        g_static_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_close/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_start(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , MALLOC_ERROR
                     , MYSQL_ERROR
                     , NONE} excp;
    int xa_rc = XAER_RMFAIL;
    char *ser_xid = NULL, *stmt = NULL;
    
    LIXA_TRACE(("lixa_my_start\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        const long valid_flags = TMJOIN|TMRESUME|TMNOWAIT|TMASYNC|TMNOFLAGS;
        const char stmt_fmt[] = "XA START %s";
        size_t stmt_size;
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_end: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOFLAGS != flags) {
            LIXA_TRACE(("lixa_my_start: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 ||
            (lpsr->state.S != 0 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_my_start: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_start: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (NULL == (ser_xid = lixa_my_xid_serialize(xid))) {
            LIXA_TRACE(("lixa_my_start: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }
        LIXA_TRACE(("lixa_my_start: starting XID %s\n", ser_xid));
        
        /* saving xid */
        lpsr->xid = *xid;
        /* preparing statement */
        stmt_size = strlen(ser_xid) + sizeof(stmt_fmt);
        if (NULL == (stmt = malloc(stmt_size)))
            THROW(MALLOC_ERROR);
        snprintf(stmt, stmt_size, stmt_fmt, ser_xid);
        /* starting transaction */
        if (mysql_query(lpsr->conn, stmt)) {
            LIXA_TRACE(("lixa_my_start: MySQL error while executing "
                        "'%s': %u/%s\n", stmt,
                        mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
            THROW(MYSQL_ERROR);
        }

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
            case MALLOC_ERROR:
                xa_rc = XAER_INVAL;
                break;                
            case MYSQL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_sw_status_mutex);
        if (NULL != ser_xid)
            free(ser_xid);
        if (NULL != stmt)
            free(stmt);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_start/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_end(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR1
                     , XID_MISMATCH
                     , XID_SERIALIZE_ERROR2
                     , MALLOC_ERROR
                     , MYSQL_ERROR
                     , ROLLBACK_ONLY
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    char *ser_xid = NULL, *stmt = NULL;
    
    LIXA_TRACE(("lixa_my_end\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        const long valid_flags = TMSUSPEND|TMMIGRATE|TMSUCCESS|TMFAIL|TMASYNC;
        const char stmt_fmt[] = "XA END %s";
        size_t stmt_size;

        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_end: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if ((TMSUSPEND|TMMIGRATE|TMASYNC) & flags) {
            LIXA_TRACE(("lixa_my_end: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 1 ||
            (lpsr->state.S != 1 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_my_end: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_end: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx, lsx2;
            if (!lixa_xid_serialize(xid, lsx) ||
                !lixa_xid_serialize(&(lpsr->xid), lsx2)) {
                LIXA_TRACE(("lixa_my_end: unable to serialize XID\n"));
                THROW(XID_SERIALIZE_ERROR1);
            }
            LIXA_TRACE(("lixa_my_end: ending XID '%s' is not the same "
                        "of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }

        /* serialize XID */
        if (NULL == (ser_xid = lixa_my_xid_serialize(xid))) {
            LIXA_TRACE(("lixa_my_end: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR2);
        }
        LIXA_TRACE(("lixa_my_end: ending XID %s\n", ser_xid));
        
        /* saving xid */
        lpsr->xid = *xid;
        /* preparing statement */
        stmt_size = strlen(ser_xid) + sizeof(stmt_fmt);
        if (NULL == (stmt = malloc(stmt_size)))
            THROW(MALLOC_ERROR);
        snprintf(stmt, stmt_size, stmt_fmt, ser_xid);
        /* starting transaction */
        if (mysql_query(lpsr->conn, stmt)) {
            LIXA_TRACE(("lixa_my_end: MySQL error while executing "
                        "'%s': %u/%s\n", stmt,
                        mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
            THROW(MYSQL_ERROR);
        }
        
        /* check if the TM marked as failed the transaction */
        if (TMFAIL & flags) {
            LIXA_TRACE(("lixa_my_end: TMFAIL detected, entering 'Rollback "
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
            case XID_SERIALIZE_ERROR1:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case XID_SERIALIZE_ERROR2:
                xa_rc = XAER_INVAL;
                break;
            case MALLOC_ERROR:
                xa_rc = XAER_INVAL;
                break;                
            case MYSQL_ERROR:
                xa_rc = XAER_RMERR;
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
        g_static_mutex_unlock(&lixa_sw_status_mutex);
        if (NULL != ser_xid)
            free(ser_xid);
        if (NULL != stmt)
            free(stmt);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_end/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_rollback(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
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
    /* ###
    PGresult *res = NULL;
    */
    
    LIXA_TRACE(("lixa_my_rollback\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;

        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_rollback: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_rollback: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0) {
            LIXA_TRACE(("lixa_my_rollback: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }
        
        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_rollback: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_my_rollback: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        if (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4) {
            const char SELECT_FMT[] = "SELECT COUNT(*) FROM pg_prepared_xacts "
                "WHERE gid = '%s';";
            char select[sizeof(SELECT_FMT) + sizeof(lsx)];
            /* check the database state, it could be a prepared transaction */
            snprintf(select, sizeof(select), SELECT_FMT, lsx);
            /* ###
            res = PQexec(lpsr->conn, select);
            if (PGRES_TUPLES_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_rollback: error while executing "
                            "'%s' command (%d/%s)\n", select,
                            PQresultStatus(res), PQerrorMessage(lpsr->conn)));
                THROW(SELECT_ERROR);
            }
            if (PQntuples(res) == 1) {
                long count = strtol(PQgetvalue(res, 0, 0), NULL, 10);
                if (count > 0) {
                    // it's a prepared transaction
                    lpsr->state.S = 3;
                    lpsr->xid = *xid;
                } else {
                    PQclear(res);
                    res = NULL;
                    LIXA_TRACE(("lixa_my_rollback: xid '%s' is not "
                                "available\n", lsx));
                    THROW(XID_NOT_AVAILABLE);
                }
            }
            PQclear(res);
            res = NULL;
            */
        }

        if (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4) {
            LIXA_TRACE(("lixa_my_rollback: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR3);
        }
        
        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_my_rollback: rolling back XID '%s' is not the "
                        "same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_my_rollback: rolling back XID '%s'\n", lsx));

        if (lpsr->state.S == 3) {
            const char ROLLBACK_PREP_FMT[] = "ROLLBACK PREPARED '%s';";
            char pq_cmd_buf[sizeof(ROLLBACK_PREP_FMT) +
                            sizeof(lixa_ser_xid_t)];
            lpsr->state.S = 0;
            /* rolling back the transaction */
            snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), ROLLBACK_PREP_FMT, lsx);
            /* ###
            res = PQexec(lpsr->conn, pq_cmd_buf);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_rollback: error while executing "
                            "ROLLBACK PREPARED command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                THROW(ROLLBACK_ERROR1);
            }
            */
        } else {
            lpsr->state.S = 0;
            /* the transaction is NOT in prepared state */
            /* ###
            res = PQexec(lpsr->conn, "ROLLBACK");
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_rollback: error while executing "
                            "ROLLBACK command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                THROW(ROLLBACK_ERROR2);
            }
            PQclear(res);
            res = NULL;
            */
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
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_sw_status_mutex);
        /* ###
        if (NULL != res)
            PQclear(res);
        */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_rollback/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_prepare(XID *xid, int rmid, long flags)
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
    /* ###
    PGresult *res = NULL;
    */
    
    LIXA_TRACE(("lixa_my_prepare\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_ser_xid_t lsx;
        const char PREPARE_TRANS_FMT[] = "PREPARE TRANSACTION '%s';";
        char pq_cmd_buf[sizeof(PREPARE_TRANS_FMT) + sizeof(lixa_ser_xid_t)];
        
        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_prepare: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_prepare: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
                
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 || lpsr->state.S != 2) {
            LIXA_TRACE(("lixa_my_prepare: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_prepare: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_my_prepare: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_my_prepare: preparing XID '%s' is not the same "
                        "of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_my_prepare: preparing XID '%s'\n", lsx));

        /* preparing transaction */
        snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), PREPARE_TRANS_FMT, lsx);
        /* ###
        res = PQexec(lpsr->conn, pq_cmd_buf);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_my_prepare: error while executing "
                        "PREPARE TRANSACTION command (%d/%s)\n",
                        PQresultStatus(res),
                        PQerrorMessage(lpsr->conn)));
               // the resource manager could unilaterally rollback and return
               // XA_RBROLLBACK to the transaction manager, but it is leaved
               // as a future improvment if necessary
            THROW(PREPARE_ERROR);
        }
        PQclear(res);
        res = NULL;
        */
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
        g_static_mutex_unlock(&lixa_sw_status_mutex);
        /* ###
        if (NULL != res)
            PQclear(res);
        */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_prepare/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_commit(XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR1
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
    /* ###
    PGresult *res = NULL;
    */
    
    LIXA_TRACE(("lixa_my_commit\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        const long valid_flags = TMNOWAIT|TMASYNC|TMONEPHASE|TMNOFLAGS;
        lixa_ser_xid_t lsx;
        int one_phase = flags & TMONEPHASE;

        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_commit: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOWAIT & flags || TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_commit: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0) {
            LIXA_TRACE(("lixa_my_commit: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_commit: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_xid_serialize(xid, lsx)) {
            LIXA_TRACE(("lixa_my_commit: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        if (lpsr->state.S != 3 && !one_phase) {
            const char SELECT_FMT[] = "SELECT COUNT(*) FROM pg_prepared_xacts "
                "WHERE gid = '%s';";
            char select[sizeof(SELECT_FMT) + sizeof(lsx)];
            /* check the database state, it could be a prepared transaction */
            snprintf(select, sizeof(select), SELECT_FMT, lsx);
            /* ###
            res = PQexec(lpsr->conn, select);
            if (PGRES_TUPLES_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_commit: error while executing "
                            "'%s' command (%d/%s)\n", select,
                            PQresultStatus(res), PQerrorMessage(lpsr->conn)));
                THROW(SELECT_ERROR);
            }
            if (PQntuples(res) == 1) {
                long count = strtol(PQgetvalue(res, 0, 0), NULL, 10);
                if (count > 0) {
                    // it's a prepared transaction
                    lpsr->state.S = 3;
                    lpsr->xid = *xid;
                } else {
                    PQclear(res);
                    res = NULL;
                    LIXA_TRACE(("lixa_my_commit: xid '%s' is not "
                                "available\n", lsx));
                    THROW(XID_NOT_AVAILABLE);
                }
            }
            PQclear(res);
            res = NULL;
            */
        }

        if ((lpsr->state.S != 2 && one_phase) ||
            (lpsr->state.S != 3 && !one_phase)) {
            LIXA_TRACE(("lixa_my_commit: rmid %d state(R,S,T)={%d,%d,%d}, "
                        "one_phase_commit=%d\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T,
                        one_phase));
            THROW(PROTOCOL_ERROR3);
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_ser_xid_t lsx2;
            lixa_xid_serialize(&(lpsr->xid), lsx2);
            LIXA_TRACE(("lixa_my_commit: committing XID '%s' is not the "
                        "same of started XID '%s'\n", lsx, lsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_my_commit: committing XID '%s'\n", lsx));

        if (lpsr->state.S == 3) {
            const char COMMIT_PREP_FMT[] = "COMMIT PREPARED '%s';";
            char pq_cmd_buf[sizeof(COMMIT_PREP_FMT) + sizeof(lixa_ser_xid_t)];
            lpsr->state.S = 0;
            /* committing transaction */
            snprintf(pq_cmd_buf, sizeof(pq_cmd_buf), COMMIT_PREP_FMT, lsx);
            /* ###
            res = PQexec(lpsr->conn, pq_cmd_buf);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_commit: error while executing "
                            "COMMIT PREPARED command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                THROW(COMMIT_ERROR1);
            }
            */
        } else {
            lpsr->state.S = 0;
            /* the transaction is NOT in prepared state */
            /* ###
            res = PQexec(lpsr->conn, "COMMIT");
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_commit: error while executing "
                            "COMMIT command (%s)\n",
                            PQerrorMessage(lpsr->conn)));
                lpsr->state.S = 0;
                THROW(COMMIT_ERROR2);
            }
            PQclear(res);
            res = NULL;
            */
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
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_sw_status_mutex);
/* ###
        if (NULL != res)
            PQclear(res);
*/
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_commit/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_recover(XID *xids, long count, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , INVALID_OPTIONS1
                     , PROTOCOL_ERROR1
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
    /* ###
    PGresult *res = NULL;
    */
    
    LIXA_TRACE(("lixa_my_recover\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        const long valid_flags = TMSTARTRSCAN|TMENDRSCAN|TMNOFLAGS;
        const char *CLOSE_CURSOR = "CLOSE lixa_my_recover_cursor;";
        const char OPEN_CURSOR_FMT[] = "DECLARE lixa_my_recover_cursor CURSOR "
            "FOR SELECT gid FROM pg_prepared_xacts WHERE gid LIKE '%s%%' "
            "ORDER BY prepared;";
        lixa_ser_xid_t lsx;
        char open_cursor[sizeof(OPEN_CURSOR_FMT) + sizeof(lsx)];
        const char *BEGIN = "BEGIN;";
        const char *END = "END;";
        const char FETCH_COUNT_FMT[] = "FETCH %ld FROM "
            "lixa_my_recover_cursor;";
        char fetch_count[sizeof(FETCH_COUNT_FMT) + sizeof(long)*3];
        int row;

        /* lock the status mutex */
        g_static_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_recover: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }

        if ((NULL == xids && count > 0) || (count < 0)) {
            LIXA_TRACE(("lixa_my_recover: xids=%p, count=%ld\n", xids, count));
            THROW(INVALID_OPTIONS1);
        }
        
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR1);
        
        if (!(flags & TMSTARTRSCAN)) {
            LIXA_TRACE(("lixa_my_recover: TMSTARTRSCAN flag must be set\n"));
            THROW(INVALID_OPTIONS2);
        }
        
        if (lpsr->state.R != 1) {
            LIXA_TRACE(("lixa_my_recover: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_recover: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* open the cursor if necessary */
        if (flags & TMSTARTRSCAN) {
            LIXA_TRACE(("lixa_my_recover: cursor is not open, opening it\n"));
            /* ###
            res = PQexec(lpsr->conn, BEGIN);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_start: error while executing '%s' "
                            "command (%s)\n", BEGIN,
                            PQerrorMessage(lpsr->conn)));
                THROW(BEGIN_ERROR);
            }
            PQclear(res);
            res = NULL;
            */
            lixa_xid_formatid(lsx);
            snprintf(open_cursor, sizeof(open_cursor), OPEN_CURSOR_FMT, lsx);
            LIXA_TRACE(("lixa_my_start: '%s'\n", open_cursor));
            /* ###
            res = PQexec(lpsr->conn, open_cursor);
            if (PGRES_COMMAND_OK != PQresultStatus(res)) {
                LIXA_TRACE(("lixa_my_start: error while executing '%s' "
                            "command (%s)\n", open_cursor,
                            PQerrorMessage(lpsr->conn)));
                THROW(OPEN_CURSOR_ERROR);
            }
            PQclear(res);
            res = NULL;
            */
        }

        /* fetching records */
        snprintf(fetch_count, sizeof(fetch_count), FETCH_COUNT_FMT, count);
        /* ###
        res = PQexec(lpsr->conn, fetch_count);
        if (PGRES_TUPLES_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_my_prepare: error while executing "
                        "'%s' command (%d/%s)\n", fetch_count,
                        PQresultStatus(res),
                        PQerrorMessage(lpsr->conn)));
            THROW(FETCH_ERROR);
        }
        for (row=0; row<PQntuples(res); ++row) {
            XID xid;            
            if (lixa_xid_deserialize(&xid, PQgetvalue(res, row, 0))) {
                LIXA_TRACE(("lixa_my_prepare: xids[%d]='%s'\n", out_count,
                            PQgetvalue(res, row, 0)));
                xids[out_count++] = xid;
            }
        }
        PQclear(res);
        res = NULL;
        */
        
        /* close the cursor if necessary */
        LIXA_TRACE(("lixa_my_recover: TMENDRSCAN is set, closing it\n"));
        /* ###
        res = PQexec(lpsr->conn, CLOSE_CURSOR);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_my_start: error while executing '%s' "
                        "command (%s)\n", CLOSE_CURSOR,
                        PQerrorMessage(lpsr->conn)));
            THROW(CLOSE_CURSOR_ERROR);
        }
        PQclear(res);
        res = NULL;
        res = PQexec(lpsr->conn, END);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("lixa_my_start: error while executing '%s' "
                        "command (%s)\n", END,
                        PQerrorMessage(lpsr->conn)));
            THROW(END_ERROR);
        }
        PQclear(res);
        res = NULL;
        */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
            case INVALID_OPTIONS1:
            case INVALID_OPTIONS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR1:
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
        /* unlock the status mutex */
        g_static_mutex_unlock(&lixa_sw_status_mutex);
        /* ###
        if (NULL != res)
            PQclear(res);
        */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_recover/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_forget(XID *xid, int rmid, long flags)
{
    /* this Resource Manager does not heuristically resolve the transactions
       and this function should never be called */
    return XAER_PROTO;
}



int lixa_my_complete(int *handle, int *retval, int rmid, long flags)
{
    /* asynchronous mode is not supported by this wrapper */
    return XAER_INVAL;
}



MYSQL *lixa_my_get_conn_by_rmid(int rmid) {
    return (MYSQL *)lixa_sw_get_conn_by_rmid(rmid);
}



MYSQL *lixa_my_get_conn(void)
{
    return lixa_sw_get_conn_by_pos(0);
}
