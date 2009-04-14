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



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_manager.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_MANAGER



int server_manager(struct server_config_s *sc,
                   struct thread_pipe_array_s *tpa,
                   struct thread_status_array_s *tsa)
{
    enum Exception { MALLOC_ERROR
                     , PTHREAD_CREATE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager\n"));
    TRY {
        int i;

        LIXA_TRACE(("server_manager: number of managers to activate = %d\n",
                    sc->managers.n));
        
        /* prepare thread status array structure */
        if (NULL == (tsa->array = malloc(
                         tpa->n * sizeof(struct thread_status_s))))
            THROW(MALLOC_ERROR);
        tsa->n = tpa->n;

        /* first thread slot is for listener: it's the main thread of the
         * process, it's IMPLICITLY created */
        for (i = 0; i < tsa->n; ++i) {
            tsa->array[i].id = i;
            tsa->array[i].tpa = tpa;
            tsa->array[i].poll_size = 0;
            tsa->array[i].poll_array = NULL;
            tsa->array[i].excp = tsa->array[i].ret_cod =
                tsa->array[i].last_errno = 0;
            if (i == 0) { /* listener */
                tsa->array[i].tid = pthread_self();
            } else {
                /* it will be fixed by the thread itself */
                tsa->array[i].tid = 0;
                if (0 != (ret_cod = pthread_create(
                              &(tsa->array[i].tid), NULL, server_manager_thread,
                              tsa->array + i)))
                    THROW(PTHREAD_CREATE_ERROR);
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case PTHREAD_CREATE_ERROR:
                ret_cod = LIXA_RC_PTHREAD_CREATE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void *server_manager_thread(void *void_ts)
{
    enum Exception { ADD_POLL_ERROR
                     , POLL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    struct thread_status_s *ts = (struct thread_status_s *)void_ts;
    
    LIXA_TRACE(("server_manager_thread\n"));
    TRY {
        LIXA_TRACE(("server_manager_thread: id = %d, "
                    "tid = " PTHREAD_T_FORMAT "\n", ts->id, ts->tid));

        if (LIXA_RC_OK != (ret_cod = server_manager_add_poll(
                               ts,
                               ts->tpa->array[ts->id].pipefd[0])))
            THROW(ADD_POLL_ERROR);
        if (0 != (ret_cod = poll(ts->poll_array, ts->poll_size, -1)))
            THROW(POLL_ERROR);
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ADD_POLL_ERROR:
                break;
            case POLL_ERROR:
                ret_cod = LIXA_RC_POLL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    ts->excp = excp;
    ts->ret_cod = ret_cod;
    ts->last_errno = errno;
    LIXA_TRACE(("server_manager_thread/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    pthread_exit(void_ts);
}



int server_manager_add_poll(struct thread_status_s *ts,
                            int new_fd)
{
    enum Exception { REALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_add_poll\n"));
    TRY {
        nfds_t last = 0;

        /* this algorithm must be reviewed: look for an available slot, and
           then allocate a new one */
        if (NULL == (ts->poll_array = realloc(
                         ts->poll_array,
                         ++ts->poll_size * sizeof(struct pollfd))))
            THROW(REALLOC_ERROR);
        last = ts->poll_size - 1;
        ts->poll_array[last].fd = new_fd;
        ts->poll_array[last].events = POLLIN;
        LIXA_TRACE(("server_panager_add_poll: added file descriptor %d "
                    "at position " NFDS_T_FORMAT "\n",
                    ts->poll_array[last].fd, last));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_add_poll/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
