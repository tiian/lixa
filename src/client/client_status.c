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



#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#include <lixa_trace.h>
#include <lixa_errors.h>
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_STATUS



/* this is a static structure used by all the threads of the program
 * linking the library; the structure i protected by a mutex to avoid
 * concurrency issues */
client_status_set_t css;



/**
 * Initialize the library when the library is loaded.
 * This piece of code is GNU/Linux + GCC specific: it will need some
 * rework for different platforms (probably it will not compile at all)
 */
void __attribute__ ((constructor)) lixac_init(void)
{
    LIXA_TRACE_INIT;
    client_status_set_init(&css);
}



int client_status_create(client_status_t *cs)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_create\n"));
    TRY {
        cs->profile = NULL;
        
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
    LIXA_TRACE(("client_status_create/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_set_init(client_status_set_t *css)    
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_set_init\n"));
    TRY {
        LIXA_TRACE(("client_status_set_init: initializing sequentialization "
                    "mutex\n"));
        ret_cod = pthread_mutex_init(&(css->mutex), NULL);
        LIXA_TRACE(("client_status_set_init: mutex initialization return "
                    "code: %d\n", ret_cod));
        css->index_size = 0;
        css->index_data = NULL;
        css->status_size = 0;
        css->status_data = NULL;
        
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
    LIXA_TRACE(("client_status_set_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_set_register(client_status_set_t *css)
{
    enum Exception { INTERNAL_ERROR
                     , REGISTER_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_set_register\n"));
    TRY {
        int pos = client_status_set_search(css);

        switch (pos) {
            case KEY_ERROR:
                THROW(INTERNAL_ERROR);
                break;
            case KEY_NOT_FOUND:
                /* register the new thread */
                if (LIXA_RC_OK != (ret_cod = client_status_set_add(css)))
                    THROW(REGISTER_ERROR);
                break;
            default:
                LIXA_TRACE(("client_status_set_register: thread already "
                            "registered.\n"));
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
    LIXA_TRACE(("client_status_set_register/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_set_add(client_status_set_t *css)
{
    enum Exception { MUTEX_LOCK_ERROR
                     , MUTEX_UNLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_status_set_add\n"));
    TRY {
        pthread_t key = pthread_self();

        /* lock the mutex to avoid collisions */
        if (0 != pthread_mutex_lock(&(css->mutex)))
            THROW(MUTEX_LOCK_ERROR);

        /* unlock the mutex to avoid collisions */
        if (0 != pthread_mutex_unlock(&(css->mutex)))
            THROW(MUTEX_UNLOCK_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        
        /* recovery actions */
        if (NONE != excp)
            LIXA_TRACE(("client_status_set_add: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
        if (excp > MUTEX_LOCK_ERROR && excp < MUTEX_UNLOCK_ERROR) {
            ret_cod = pthread_mutex_unlock(&(css->mutex));
            if (0 != ret_cod)
                LIXA_TRACE(("client_status_set_add/pthread_mutex_unlock: "
                            "ret_cod=%d/errno=%d\n", ret_cod, errno));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("client_status_set_add/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_status_set_search(client_status_set_t *css)
{
    int p,u,m;
    pthread_t key = pthread_self();
    if (NULL == css->index_data)
        return KEY_NOT_FOUND;
    if (1 > css->index_size)
        return KEY_ERROR;
    p = 0;
    u = css->index_size - 1;
    while (p <= u) {
        m = (p+u)/2;
        if (css->index_data[m].key == key)
            return m;
        if (css->index_data[m].key < key)
            p = m + 1;
        else
            u = m - 1;
    }
    return KEY_NOT_FOUND;
}
