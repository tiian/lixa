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



#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_errors.h>
#include <liblixamonkey.h>



/**
 * Hash table containing the status of the Monkey's resource manager(s): <br>
 * - first level of hashing is thread_id <br>
 * - second level of hashing is rmid
 */
GHashTable *monkey_status = NULL;



/**
 * This mutex is necessary to protect the access to @ref monkey_status
 * hash table
 */
GStaticMutex monkey_mutex = G_STATIC_MUTEX_INIT;



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



gboolean monkey_status_gequal(gconstpointer a, gconstpointer b) {
    return a == b;
}



void monkey_status_destroy1(gpointer data) {
    g_hash_table_remove_all((GHashTable *)data);
}



void monkey_status_destroy2(gpointer data) {
    g_free(data);
}



int lixa_monkeyrm_get_rc(struct monkey_status_s *mss, 
                        enum monkey_status_verb_e verb, int *rc)
{
    enum Exception { OUT_OF_RANGE
                     , INVALID_OPTION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_monkeyrm_get_rc\n"));
    TRY {
        struct monkey_status_record_s *msrs = NULL;
        
        /* check a potential out of range */
        if (mss->next_record >= mss->records->len) {
            LIXA_TRACE(("lixa_monkeyrm_get_rc: already picked all the "
                        "available return codes (%u)\n", mss->next_record));
            THROW(OUT_OF_RANGE);
        }
        msrs = &g_array_index(mss->records, struct monkey_status_record_s,
                              mss->next_record);
        if (msrs->verb != verb) {
            LIXA_TRACE(("lixa_monkeyrm_get_rc: expected verb is %d, passed "
                        "verb is %d\n", msrs->verb, verb));
            THROW(INVALID_OPTION);
        }
        LIXA_TRACE(("lixa_monkeyrm_get_rc: verb is %d, XA return code is %d\n",
                    msrs->verb, msrs->rc));
        *rc = msrs->rc;

        /* increment next_record */
        mss->next_record++;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case INVALID_OPTION:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_get_rc/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int lixa_monkeyrm_open(char *xa_info, int rmid, long flags)
{
    enum Exception { HASH_TABLE_NEW1
                     , HASH_TABLE_NEW2
                     , OPEN_INIT
                     , GET_RC_ERROR
                     , NONE } excp;
    
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    LIXA_TRACE(("lixa_monkeyrm_open: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;
        
        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);

        /* first call ? */
        if (NULL == monkey_status) {
            /* create hash structure */
            LIXA_TRACE(("lixa_monkeyrm_open: creating new first level hash "
                        "table...\n"));
            if (NULL == (monkey_status = g_hash_table_new_full(
                             g_direct_hash, monkey_status_gequal,
                             NULL, monkey_status_destroy1)))
                THROW(HASH_TABLE_NEW1);
        }

        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid))) {
            /* first time for this thread */
            LIXA_TRACE(("lixa_monkeyrm_open: creating new second level hash "
                        "table for tid=" PTHREAD_T_FORMAT "\n", tid));
            if (NULL == (slht = g_hash_table_new_full(
                             g_direct_hash, monkey_status_gequal,
                             NULL, monkey_status_destroy2)))
                THROW(HASH_TABLE_NEW2);
            g_hash_table_insert(monkey_status, (gpointer)tid, (gpointer)slht);
        }

        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid))) {
            /* first time for this rmid */
            LIXA_TRACE(("lixa_monkeyrm_open: creating new status block for "
                        "tid=" PTHREAD_T_FORMAT ", rmid=%d\n", tid, rmid));
            mss = g_malloc(sizeof(struct monkey_status_s));
            if (LIXA_RC_OK != lixa_monkeyrm_open_init(
                    xa_info, rmid, flags, mss))
                THROW(OPEN_INIT);
            g_hash_table_insert(slht, (gpointer)rmid, (gpointer)mss);
        }

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_OPEN, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case HASH_TABLE_NEW1:
            case HASH_TABLE_NEW2:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                xa_rc = XAER_RMERR;
                break;
            case OPEN_INIT:
                xa_rc = XAER_RMERR;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_open/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_open_init(char *xa_info, int rmid, long flags,
                            struct monkey_status_s *mss)
{
    enum Exception { ARRAY_NEW_ERROR
                     , FOPEN_ERROR
                     , INVALID_VERB
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    FILE *config = NULL;
    
    LIXA_TRACE(("lixa_monkeyrm_open_init\n"));
    TRY {
        char buffer[1024];
        struct monkey_status_record_s record;
        
        if (NULL == (mss->records = g_array_new(
                         FALSE, FALSE, sizeof(struct monkey_status_record_s))))
            THROW(ARRAY_NEW_ERROR);
        
        if (NULL == (config = fopen(xa_info, "r")))
            THROW(FOPEN_ERROR);

        while (!feof(config)) {
            char *delim, *rc;
            if (NULL == fgets(buffer, sizeof(buffer), config))
                break;
            /* bypass comment rows */
            if ('#' == buffer[0])
                continue;
            /* locate delimiter */
            if (NULL == (delim = strchr(buffer, '/'))) {
                LIXA_TRACE(("lixa_monkeyrm_open_init: found row without "
                            "'/' delimiter, skipping... %s", buffer));
                continue;
            }
            /* split string, pos rc */
            *delim = '\0';
            rc = delim + 1;
            LIXA_TRACE(("lixa_monkeyrm_open_init: verb='%s'\n", buffer));
            if (NULL != strstr(buffer, "xa_open"))
                record.verb = XA_OPEN;
            else if (NULL != strstr(buffer, "xa_close"))
                record.verb = XA_CLOSE;
            else if (NULL != strstr(buffer, "xa_start"))
                record.verb = XA_START;
            else if (NULL != strstr(buffer, "xa_end"))
                record.verb = XA_END;
            else if (NULL != strstr(buffer, "xa_rollback"))
                record.verb = XA_ROLLBACK;
            else if (NULL != strstr(buffer, "xa_prepare"))
                record.verb = XA_PREPARE;
            else if (NULL != strstr(buffer, "xa_commit"))
                record.verb = XA_COMMIT;
            else if (NULL != strstr(buffer, "xa_recover"))
                record.verb = XA_RECOVER;
            else if (NULL != strstr(buffer, "xa_forget"))
                record.verb = XA_FORGET;
            else if (NULL != strstr(buffer, "xa_complete"))
                record.verb = XA_COMPLETE;
            else
                THROW(INVALID_VERB);
            record.rc = (int)strtol(rc, NULL, 10);
            LIXA_TRACE(("lixa_monkeyrm_open_init: appending record verb=%d, "
                        "rc=%d\n", record.verb, record.rc));

            g_array_append_val(mss->records, record);
        }
        mss->next_record = 0;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ARRAY_NEW_ERROR:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case FOPEN_ERROR:
                ret_cod = LIXA_RC_FOPEN_ERROR;
                break;
            case INVALID_VERB:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (NULL != config)
            fclose(config);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_open_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int lixa_monkeyrm_close(char *xa_info, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    LIXA_TRACE(("lixa_monkeyrm_close: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_CLOSE, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_close/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_start(XID *xid, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_start: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_START, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_start/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_end(XID *xid, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_end: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_END, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_end/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_rollback(XID *xid, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_rollback: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_ROLLBACK, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_rollback/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_prepare(XID *xid, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_prepare: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_PREPARE, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_prepare/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_commit(XID *xid, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_commit: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_COMMIT, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_commit/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_recover(XID *xid, long count, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_recover: *xid=%s, count=%ld, rmid=%d, "
                "flags=0x%lx\n", xid_str, count, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_RECOVER, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_recover/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_forget(XID *xid, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    char *xid_str = xid_serialize(xid); 
    LIXA_TRACE(("lixa_monkeyrm_forget: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_FORGET, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_commit/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_complete(int *handle, int *retval, int rmid, long flags)
{
    enum Exception { NULL_MONKEY_STATUS
                     , INVALID_STATUS1
                     , INVALID_STATUS2
                     , GET_RC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int xa_rc = XA_OK;
    
    LIXA_TRACE(("lixa_monkeyrm_complete: rmid=%d, flags=0x%lx\n",
                rmid, flags));    
    TRY {
        pthread_t tid;
        GHashTable *slht;
        struct monkey_status_s *mss;

        /* lock mutex */
        g_static_mutex_lock(&monkey_mutex);
        
        /* first call ? */
        if (NULL == monkey_status)
            THROW(NULL_MONKEY_STATUS);
            
        /* search current thread id in the hash table */
        tid = pthread_self();
        if (NULL == (slht = (GHashTable *)g_hash_table_lookup(
                         monkey_status, (gconstpointer)tid)))
            THROW(INVALID_STATUS1);
        
        /* search passed rmid in the second level hash table */
        if (NULL == (mss = (struct monkey_status_s *)g_hash_table_lookup(
                         slht, (gconstpointer)rmid)))
            THROW(INVALID_STATUS2);

        /* retrieve the return code must be returned */
        if (LIXA_RC_OK != lixa_monkeyrm_get_rc(mss, XA_COMPLETE, &xa_rc))
            THROW(GET_RC_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_MONKEY_STATUS:
                xa_rc = XAER_PROTO;
                ret_cod = LIXA_RC_OK;
                break;
            case INVALID_STATUS1:
            case INVALID_STATUS2:
                xa_rc = XAER_RMERR;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_RC_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex */
        g_static_mutex_unlock(&monkey_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_monkeyrm_complete/excp=%d/"
                "ret_cod=%d/xa_rc=%d/errno=%d\n",
                excp, ret_cod, xa_rc, errno));
    assert(LIXA_RC_OK == ret_cod);
    return xa_rc;
}



int lixa_monkeyrm_call_ax_reg(int rmid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_monkeyrm_call_ax_reg\n"));
    TRY {
        XID xid;
        long flags = TMNOFLAGS;
        int rc = ax_reg(rmid, &xid, flags);

        LIXA_TRACE(("lixa_monkeyrm_call_ax_reg: ax_reg returned %d\n", rc));
        
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
    LIXA_TRACE(("lixa_monkeyrm_call_ax_reg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_monkeyrm_call_ax_unreg(int rmid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_monkeyrm_call_ax_unreg\n"));
    TRY {
        long flags = TMNOFLAGS;
        int rc = ax_unreg(rmid, flags);

        LIXA_TRACE(("lixa_monkeyrm_call_ax_unreg: ax_unreg returned %d\n",
                    rc));
        
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
    LIXA_TRACE(("lixa_monkeyrm_call_ax_unreg/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



/**
 * This is the struct pointing to monkey functions (static registration
 * version)
 */
struct xa_switch_t lixa_monkeyrm_sta_sw = {
    "LIXA Monkey RM",
    TMNOFLAGS,
    0,
    lixa_monkeyrm_open,
    lixa_monkeyrm_close,
    lixa_monkeyrm_start,
    lixa_monkeyrm_end,
    lixa_monkeyrm_rollback,
    lixa_monkeyrm_prepare,
    lixa_monkeyrm_commit,
    lixa_monkeyrm_recover,
    lixa_monkeyrm_forget,
    lixa_monkeyrm_complete
};



/**
 * This is the struct pointing to monkey functions (dynamic registration
 * version)
 */
struct xa_switch_t lixa_monkeyrm_dyn_sw = {
    "LIXA Monkey RM",
    TMREGISTER,
    0,
    lixa_monkeyrm_open,
    lixa_monkeyrm_close,
    lixa_monkeyrm_start,
    lixa_monkeyrm_end,
    lixa_monkeyrm_rollback,
    lixa_monkeyrm_prepare,
    lixa_monkeyrm_commit,
    lixa_monkeyrm_recover,
    lixa_monkeyrm_forget,
    lixa_monkeyrm_complete
};
