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



#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
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
client_status_coll_t global_csc;



/* this static structure is used by all the threads of the program and contains
 * the configuration read by the first thread and used by all the thread
 * hosted by the same process */
client_config_coll_t global_ccc;



/**
 * Initialize the library when the library is loaded.
 * This piece of code is GNU/Linux + GCC specific: it will need some
 * rework for different platforms (probably it will not compile at all)
 */
void __attribute__ ((constructor)) lixac_init(void)
{
    LIXA_TRACE_INIT;
    client_status_coll_init(&global_csc);
    client_config_coll_init(&global_ccc);
}



void client_status_init(client_status_t *cs)
{
    LIXA_TRACE(("client_status_init: begin\n"));
    cs->active = FALSE;
    cs->sockfd = LIXA_NULL_FD;
    LIXA_TRACE(("client_status_init: end\n"));
    return;
}



int client_status_coll_init(client_status_coll_t *csc)    
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_coll_init\n"));
    TRY {
        LIXA_TRACE(("client_status_coll_init: initializing sequentialization "
                    "rwlock\n"));
        ret_cod = pthread_rwlock_init(&(csc->rwlock), NULL);
        LIXA_TRACE(("client_status_coll_init: rwlock initialization return "
                    "code: %d\n", ret_cod));
        csc->index_size = 0;
        csc->index_data = NULL;
        csc->status_size = 0;
        csc->status_used = 0;
        csc->status_data = NULL;
        
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
    LIXA_TRACE(("client_status_coll_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_coll_register(client_status_coll_t *csc, int *pos)
{
    enum Exception { INTERNAL_ERROR
                     , REGISTER_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_coll_register\n"));
    TRY {
        *pos = 0;

        switch (ret_cod = client_status_coll_search(csc, pos, TRUE)) {
            case LIXA_RC_OK:
                LIXA_TRACE(("client_status_coll_register: thread already "
                            "registered.\n"));
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* register the new thread */
                if (LIXA_RC_OK != (ret_cod = client_status_coll_add(csc, pos)))
                    THROW(REGISTER_ERROR);
                break;
            default:
                THROW(INTERNAL_ERROR);
                break;
        } /* switch (pos) */
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case REGISTER_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_register/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_coll_add(client_status_coll_t *csc, int *status_pos)
{
    enum Exception { RWLOCK_WRLOCK_ERROR
                     , MALLOC_ERROR
                     , OBJ_CORRUPTED
                     , REALLOC_ERROR
                     , RWLOCK_UNLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    struct client_status_index_s *new_index_data = NULL;
    client_status_t *new_status_data = NULL;
    
    LIXA_TRACE(("client_status_coll_add\n"));
    TRY {
        pthread_t key = pthread_self();
        int new_index_size = 0, i = 0;
        int free_slot = 0;
        int new_status_size = csc->status_size;
        int inserted = FALSE;

        /* take an exclusive lock to avoid collisions */
        if (0 != pthread_rwlock_wrlock(&(csc->rwlock)))
            THROW(RWLOCK_WRLOCK_ERROR);

        /* allocate a new index */
        new_index_size = csc->index_size + 1;
        if (NULL == (new_index_data = malloc(
                         sizeof(struct client_status_index_s) *
                         new_index_size)))
            THROW(MALLOC_ERROR);

        /* copy & insert */
        if (csc->index_size > 0) {
            while (i < csc->index_size) {
                assert(csc->index_data[i].key != key);
                if (csc->index_data[i].key < key)
                    new_index_data[i] = csc->index_data[i];
                else {
                    /* insert new key */
                    new_index_data[i].key = key;
                    inserted = TRUE;
                    memcpy(new_index_data + i + 1, csc->index_data + i,
                           (csc->index_size - i) *
                           sizeof(struct client_status_index_s));
                    break;
                }
                ++i;
            } /* while (i < new_index_size) */
        }
        if (!inserted)
            new_index_data[i].key = key;
        LIXA_TRACE(("client_status_coll_add: index key inserted at pos %d, "
                    "old index size = %d, new index size = %d\n",
                    i, csc->index_size, new_index_size));
        /* now i is the pos in index of the new key */

        LIXA_TRACE(("client_status_coll_add: status_used = %d, "
                    "status_size = %d\n", csc->status_used,
                    csc->status_size));
        if (csc->status_used < csc->status_size) {
            /* there must be at least one free slot */
            int j;
            for (j = 0; j < csc->status_size; ++j) {
                if (!client_status_is_active(&(csc->status_data[j])))
                    break;
            }
            if (client_status_is_active(&(csc->status_data[j])))
                THROW(OBJ_CORRUPTED);
            free_slot = j;
        } else {
            new_status_size = csc->status_size + 1;
            if (NULL == (new_status_data = realloc(
                             csc->status_data,
                             sizeof(client_status_t) * new_status_size)))
                THROW(REALLOC_ERROR);
            csc->status_data = new_status_data;
            free_slot = csc->status_size;
        }
        LIXA_TRACE(("client_status_coll_add: status inserted at pos %d, "
                    "old status size = %d, new status size = %d, "
                    "old used status = %d\n", free_slot, csc->status_size,
                    new_status_size, csc->status_used));
        /* reset & set slot */
        client_status_init(&(csc->status_data[free_slot]));
        client_status_active(&(csc->status_data[free_slot]));

        /* finalize operations */
        *status_pos = free_slot;
        csc->status_size = new_status_size;
        csc->status_used++;
        csc->index_size = new_index_size;
        if (NULL != csc->index_data)
            free(csc->index_data);
        LIXA_TRACE(("client_status_coll_add: after free\n"));
        csc->index_data = new_index_data;
        csc->index_data[i].value = free_slot;
        
        LIXA_TRACE(("client_status_coll_add: index key = "
                    PTHREAD_T_FORMAT ", index value = %d\n", key, free_slot));
        
        /* release exclusive lock */
        if (0 != pthread_rwlock_unlock(&(csc->rwlock)))
            THROW(RWLOCK_UNLOCK_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RWLOCK_WRLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_RWLOCK_WRLOCK_ERROR;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case RWLOCK_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_RWLOCK_UNLOCK_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        
        /* recovery actions */
        if (NONE != excp)
            LIXA_TRACE(("client_status_coll_add: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
        if (excp > RWLOCK_WRLOCK_ERROR && excp < RWLOCK_UNLOCK_ERROR) {
            ret_cod = pthread_rwlock_unlock(&(csc->rwlock));
            if (0 != ret_cod)
                LIXA_TRACE(("client_status_coll_add/pthread_rwunlock_unlock: "
                            "ret_cod=%d/errno=%d\n", ret_cod, errno));
        }
        if (excp > MALLOC_ERROR && excp < RWLOCK_UNLOCK_ERROR)
            free(new_index_data);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_add/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_coll_del(client_status_coll_t *csc)
{
    enum Exception { RWLOCK_WRLOCK_ERROR
                     , EMPTY_INDEX
                     , COLL_SEARCH
                     , OBJ_CORRUPTED
                     , MALLOC_ERROR
                     , RWLOCK_UNLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    struct client_status_index_s *new_index_data = NULL;

    LIXA_TRACE(("client_status_coll_del\n"));
    TRY {
        int new_index_size = 0, pos = 0;
        
        /* take an exclusive lock to avoid collisions */
        if (0 != pthread_rwlock_wrlock(&(csc->rwlock)))
            THROW(RWLOCK_WRLOCK_ERROR);

        if (csc->index_size < 1)
            THROW(EMPTY_INDEX);

        if (LIXA_RC_OK != (ret_cod = client_status_coll_search(
                               csc, &pos, FALSE)))
            THROW(COLL_SEARCH);
        
        if (pos >= csc->index_size)
            THROW(OBJ_CORRUPTED);

#ifndef NDEBUG
    {
        pthread_t whoami = pthread_self();
        if (csc->index_data[pos].key != whoami) {
            LIXA_TRACE(("client_status_coll_del: whoami = "
                        PTHREAD_T_FORMAT ", csc->index_data[pos].key = "
                        PTHREAD_T_FORMAT "\n", whoami,
                        csc->index_data[pos].key));
            assert(csc->index_data[pos].key != whoami);
        }
    }
#endif /* NDEBUG */
        
        /* reset the status slot */
        client_status_init(&(csc->status_data[csc->index_data[pos].value]));
        csc->status_used--;
        
        new_index_size = csc->index_size - 1;
        LIXA_TRACE(("client_status_coll_del: index size = %d, "
                    "new index size = %d, pos = %d\n", csc->index_size,
                    new_index_size, pos));
        if (new_index_size > 0) {
            if (NULL == (new_index_data = malloc(
                         sizeof(struct client_status_index_s) *
                         new_index_size)))
                THROW(MALLOC_ERROR);
            if (pos > 0)
                memcpy(new_index_data, csc->index_data,
                       pos * sizeof(struct client_status_index_s));
            if (pos < csc->index_size - 1)
                memcpy(new_index_data + pos, csc->index_data + pos + 1,
                       (csc->index_size - pos - 1) *
                       sizeof(struct client_status_index_s));
            csc->index_size = new_index_size;
            free(csc->index_data);
            csc->index_data = new_index_data;
        } else {
            csc->index_size = 0;
            free(csc->index_data);
            csc->index_data = NULL;
        }
        /* release exclusive lock */
        if (0 != pthread_rwlock_unlock(&(csc->rwlock)))
            THROW(RWLOCK_UNLOCK_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RWLOCK_WRLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_RWLOCK_WRLOCK_ERROR;
                break;
            case EMPTY_INDEX:
                LIXA_TRACE(("client_status_coll_del: the index is empty (%d), "
                            "no client status can be removed from the "
                            "container\n", csc->index_size));
                ret_cod = LIXA_RC_EMPTY_CONTAINER;
                break;
            case COLL_SEARCH:
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case RWLOCK_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_RWLOCK_UNLOCK_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        
        /* recovery actions */
        if (NONE != excp)
            LIXA_TRACE(("client_status_coll_del: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
        if (excp > RWLOCK_WRLOCK_ERROR && excp < RWLOCK_UNLOCK_ERROR) {
            ret_cod = pthread_rwlock_unlock(&(csc->rwlock));
            if (0 != ret_cod)
                LIXA_TRACE(("client_status_coll_del/pthread_rwunlock_unlock: "
                            "ret_cod=%d/errno=%d\n", ret_cod, errno));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_del/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_coll_search(client_status_coll_t *csc, int *pos, int lock)
{
    enum Exception { RWLOCK_RDLOCK_ERROR
                     , NOT_FOUND1
                     , OBJ_CORRUPTED
                     , NONE
                     , NOT_FOUND2 } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_coll_search\n"));
    TRY {
        int p,u,m;
        pthread_t key = pthread_self();

        /* take a shared lock to avoid collisions */
        if (lock && 0 != pthread_rwlock_rdlock(&(csc->rwlock)))
            THROW(RWLOCK_RDLOCK_ERROR);

        if (NULL == csc->index_data)
            THROW(NOT_FOUND1);
        if (1 > csc->index_size)
            THROW(OBJ_CORRUPTED);
        p = 0;
        u = csc->index_size - 1;
        while (p <= u) {
            m = (p+u)/2;
            if (csc->index_data[m].key == key) {
                *pos = m;
                THROW(NONE);
            }
            if (csc->index_data[m].key < key)
                p = m + 1;
            else
                u = m - 1;
        }

        THROW(NOT_FOUND2);
    } CATCH {
        switch (excp) {
            case RWLOCK_RDLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_RWLOCK_RDLOCK_ERROR;
                break;
            case NOT_FOUND1:
            case NOT_FOUND2:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
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
        if (NONE != excp)
            LIXA_TRACE(("client_status_coll_search: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
        if (lock && excp > RWLOCK_RDLOCK_ERROR) {
            int ret_cod2 = pthread_rwlock_unlock(&(csc->rwlock));
            if (0 != ret_cod2)
                LIXA_TRACE(("client_status_coll_search/pthread_mutex_unlock: "
                            "ret_cod=%d/errno=%d\n", ret_cod2, errno));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_search/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_coll_get_cs(client_status_coll_t *csc,
                              client_status_t *cs)
{
    enum Exception { RWLOCK_RDLOCK_ERROR
                     , COLL_SEARCH_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_coll_get_cs\n"));
    TRY {
        int pos = 0;
        
        /* take a shared lock to avoid collisions */
        if (0 != pthread_rwlock_rdlock(&(csc->rwlock)))
            THROW(RWLOCK_RDLOCK_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_status_coll_search(
                               csc, &pos, FALSE)))
            THROW(COLL_SEARCH_ERROR);

        *cs = csc->status_data[csc->index_data[pos].value];
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RWLOCK_RDLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_RWLOCK_RDLOCK_ERROR;
                break;
            case COLL_SEARCH_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (NONE != excp)
            LIXA_TRACE(("client_status_coll_get_cs: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
        if (excp > RWLOCK_RDLOCK_ERROR) {
            int ret_cod2 = pthread_rwlock_unlock(&(csc->rwlock));
            if (0 != ret_cod2)
                LIXA_TRACE(("client_status_coll_get_cs/pthread_mutex_unlock: "
                            "ret_cod=%d/errno=%d\n", ret_cod2, errno));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_coll_get_cs/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

