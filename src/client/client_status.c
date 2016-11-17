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
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
 */
#include <config.h>


#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif


#include <lixa_trace.h>
#include <lixa_errors.h>
#include <client_config.h>
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_STATUS


/* this static structure is used by all the threads of the program
 * linking the library; the structure i protected by a mutex to avoid
 * concurrency issues */
client_status_coll_t global_csc = {G_STATIC_MUTEX_INIT,
                                   NULL};


/* this static structure is used by all the threads of the program and contains
 * the configuration read by the first thread and used by all the thread
 * hosted by the same process */
client_config_coll_t global_ccc = {G_STATIC_MUTEX_INIT,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   "",
                                   {NULL, NULL},
                                   NULL,
                                   NULL,
                                   NULL};


void client_status_init(client_status_t *cs) {
    LIXA_TRACE(("client_status_init: begin\n"));
    cs->active = FALSE;
    cs->sockfd = LIXA_NULL_FD;
    common_status_conthr_init(&cs->state);
    cs->rmstatus = g_array_new(FALSE, FALSE,
                               sizeof(struct client_status_rsrmgr_s));
    cs->tx_timeout = 0;
    cs->tx_timeout_time = 0;
    cs->failed = FALSE;
#ifdef _CRASH
    cs->crash_count = 0;
#endif
    LIXA_TRACE(("client_status_init: end\n"));
    return;
}


void client_status_free(client_status_t *cs) {
    LIXA_TRACE(("client_status_free: begin\n"));
    g_array_free(cs->rmstatus, TRUE);
    LIXA_TRACE(("client_status_free: end\n"));
}


void client_status_display(const client_status_t *cs) {
    LIXA_TRACE(("client_status_display: active=%d, sockfd=%d\n", cs->active,
            cs->sockfd));
    common_status_conthr_display(&cs->state);
}


int client_status_coll_add(client_status_coll_t *csc) {
    enum Exception {
        YET_ADDED, MALLOC_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    client_status_t *cs = NULL;

    LIXA_TRACE(("client_status_coll_add\n"));
    TRY {
        pthread_t key = pthread_self();

        /* take an exclusive lock to avoid collisions */
        LIXA_TRACE(("client_status_coll_add: acquiring mutex\n"));
        g_static_mutex_lock(&(csc->mutex));

        /* verify the thread was not already registered */
        if (NULL != (cs = (client_status_t *) g_hash_table_lookup(
                csc->status_data, (gconstpointer) key))) {
            LIXA_TRACE(("client_status_coll_add: status for thread "
                               PTHREAD_T_FORMAT
                               " yet added, skipping...\n", key));
            THROW(YET_ADDED);
        }

        /* allocate a new status object */
        if (NULL == (cs = (client_status_t *) g_malloc(
                sizeof(client_status_t)))) THROW(MALLOC_ERROR);

        /* reset & set slot */
        client_status_init(cs);
        client_status_active(cs);

        g_hash_table_insert(csc->status_data, (gpointer) key, (gpointer) cs);
        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case YET_ADDED:
                ret_cod = LIXA_RC_OK;
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

        /* release exclusive lock */
        LIXA_TRACE(("client_status_coll_add: releasing mutex\n"));
        g_static_mutex_unlock(&(csc->mutex));

    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_add/excp=%d/"
            "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int client_status_coll_del(client_status_coll_t *csc) {
    enum Exception {
        OBJ_NOT_FOUND, OBJ_CORRUPTED, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    client_status_t *cs = NULL;

    LIXA_TRACE(("client_status_coll_del\n"));
    TRY {
        pthread_t key = pthread_self();

        /* take an exclusive lock to avoid collisions */
        LIXA_TRACE(("client_status_coll_del: acquiring mutex\n"));
        g_static_mutex_lock(&(csc->mutex));

        /* retrieve client status */
        if (NULL == (cs = (client_status_t *) g_hash_table_lookup(
                csc->status_data, (gconstpointer) key))) {
            LIXA_TRACE(("client_status_coll_del: status for thread "
                               PTHREAD_T_FORMAT
                               " not found, skipping...\n", key));
            THROW(OBJ_NOT_FOUND);
        }

        /* free dynamic memory */
        client_status_free(cs);
        g_free(cs);
        cs = NULL;

        /* remove from hash table */
        if (!g_hash_table_remove(csc->status_data, (gconstpointer) key)) {
            LIXA_TRACE(("client_status_coll_del: the key was found by "
                    "g_hash_table_lookup, but was not removed by "
                    "g_hast_table_remove; this is a severe error!\n"));
            THROW(OBJ_CORRUPTED);
        }

        /* if hash table is empty, remove hash table */
        if (0 == g_hash_table_size(csc->status_data)) {
            LIXA_TRACE(("client_status_coll_del: the hash table is empty, "
                    "removing it...\n"));
            g_hash_table_destroy(csc->status_data);
            csc->status_data = NULL;
        }

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case OBJ_NOT_FOUND:
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */

        /* release exclusive lock */
        LIXA_TRACE(("client_status_coll_del: releasing mutex\n"));
        g_static_mutex_unlock(&(csc->mutex));
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_del/excp=%d/"
            "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int client_status_could_one_phase(const client_status_t *cs) {
    guint i, n = 0;

    if (global_ccc.actconf.rsrmgrs->len == 1)
        return TRUE;

    /* scan all the resource manager status */
    for (i = 0; i < global_ccc.actconf.rsrmgrs->len; ++i) {
        struct client_status_rsrmgr_s *csr = &g_array_index(
        cs->rmstatus,
        struct client_status_rsrmgr_s, i);
        LIXA_TRACE(("client_status_could_one_phase: i=%u, csr->dynamic=%d, "
                "csr->xa_td_state=%d\n", i, csr->common.dynamic,
                csr->common.xa_td_state));
        if (csr->common.dynamic) {
            if (csr->common.xa_td_state == XA_STATE_D1)
                n++;
        } else
            n++;
    }
    LIXA_TRACE(("client_status_could_one_phase: found %u registered "
            "resource managers\n", n));
    return n == 1;
}


gboolean client_status_coll_gequal(gconstpointer a, gconstpointer b) {
    return a == b;
}


int client_status_coll_get_cs(client_status_coll_t *csc,
                              client_status_t **cs) {
    enum Exception {
        HASH_TABLE_NEW, OBJ_NOT_FOUND, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_status_coll_get_cs\n"));
    TRY {
        pthread_t key = pthread_self();

        /* take a shared lock to avoid collisions */
        LIXA_TRACE(("client_status_coll_get_cs: acquiring mutex\n"));
        g_static_mutex_lock(&(csc->mutex));

        if (NULL == csc->status_data) {
            LIXA_TRACE(("client_status_coll_get_cs: initializing hash "
                    "table for client status...\n"));
            if (NULL == (csc->status_data = g_hash_table_new(
                    g_direct_hash, client_status_coll_gequal))) THROW(HASH_TABLE_NEW);
        }

        /* retrieve client status */
        if (NULL == (*cs = (client_status_t *) g_hash_table_lookup(
                csc->status_data, (gconstpointer) key))) {
            LIXA_TRACE(("client_status_coll_get_cs: status for thread "
                               PTHREAD_T_FORMAT
                               " not found, skipping...\n", key));
            THROW(OBJ_NOT_FOUND);
        }

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case HASH_TABLE_NEW:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case OBJ_NOT_FOUND:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        LIXA_TRACE(("client_status_coll_get_cs: releasing mutex\n"));
        g_static_mutex_unlock(&(csc->mutex));
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_get_cs/excp=%d/"
            "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int client_status_coll_is_empty(client_status_coll_t *csc) {
    int ret_cod = FALSE;

    LIXA_TRACE(("client_status_coll_is_empty: acquiring mutex\n"));
    g_static_mutex_lock(&(csc->mutex));

    if (NULL != csc->status_data)
        ret_cod = (0 == g_hash_table_size(csc->status_data));
    else
        ret_cod = TRUE;

    LIXA_TRACE(("client_status_coll_is_empty: releasing mutex\n"));
    g_static_mutex_unlock(&(csc->mutex));
    return ret_cod;
}
