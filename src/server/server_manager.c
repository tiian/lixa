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



#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg_deserialize.h>
#include <lixa_xml_msg_trace.h>
#include <server_manager.h>
#include <server_messages.h>
#include <server_reply.h>
#include <server_xa.h>



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
                     , THREAD_STATUS_LOAD_FILES_ERROR
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
            thread_status_init(&(tsa->array[i]), i, tpa);
            if (i != 0) {
                /* load status file for thread != listener */
                if (LIXA_RC_OK != (
                        ret_cod = thread_status_load_files(
                            &(tsa->array[i]),
                            sc->managers.array[i-1].status_file)))
                    THROW(THREAD_STATUS_LOAD_FILES_ERROR);
                /* it will be fixed by the thread itself */
                if (0 != (ret_cod = pthread_create(
                              &(tsa->array[i].tid), NULL,
                              server_manager_thread, tsa->array + i)))
                    THROW(PTHREAD_CREATE_ERROR);
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case THREAD_STATUS_LOAD_FILES_ERROR:
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
                     , THREAD_STATUS_SYNC_FILES_ERROR
                     , FIX_POLL_ERROR
                     , POLL_ERROR
                     , NETWORK_EVENT_ERROR
                     , POLLIN_CTRL_ERROR
                     , POLLIN_DATA_ERROR
                     , POLLOUT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    struct thread_status_s *ts = (struct thread_status_s *)void_ts;
    
    LIXA_TRACE(("server_manager_thread\n"));
    TRY {
        int ready_fd, found_fd;
        nfds_t place = 0, i, n;
        
        LIXA_TRACE(("server_manager_thread: id = %d, "
                    "tid = " PTHREAD_T_FORMAT "\n", ts->id, ts->tid));
        if (LIXA_RC_OK != (ret_cod = server_manager_add_poll(
                               ts, ts->tpa->array[ts->id].pipefd[0], &place)))
            THROW(ADD_POLL_ERROR);
        while (TRUE) {
            if (ts->asked_sync > 0) {
                LIXA_TRACE(("server_manager_thread: %d sessions asked file "
                            "status synchronization\n", ts->asked_sync));
                if (LIXA_RC_OK != (ret_cod = thread_status_sync_files(ts)))
                    THROW(THREAD_STATUS_SYNC_FILES_ERROR);
                ts->asked_sync = 0;
            }
            if (LIXA_RC_OK != (ret_cod = server_manager_fix_poll(ts)))
                THROW(FIX_POLL_ERROR);
            LIXA_TRACE(("server_manager_thread: id = %d, entering poll...\n",
                        ts->id));
            if (0 >= (ready_fd = poll(ts->poll_array, ts->poll_size, -1)))
                THROW(POLL_ERROR);
            LIXA_TRACE(("server_manager_thread: id = %d, ready file "
                        "descriptors = %d\n", ts->id, ready_fd));
            found_fd = 0;
            n = ts->poll_size;
            for (i = 0; i < n; ++i) {
                LIXA_TRACE(("server_manager_thread: slot=" NFDS_T_FORMAT
                            ", fd=%d, POLLIN=%d, POLLOUT=%d, POLLERR=%d, "
                            "POLLHUP=%d, POLLNVAL=%d\n",
                            i, ts->poll_array[i].fd,
                            ts->poll_array[i].revents & POLLIN,
                            ts->poll_array[i].revents & POLLOUT,
                            ts->poll_array[i].revents & POLLERR,
                            ts->poll_array[i].revents & POLLHUP,
                            ts->poll_array[i].revents & POLLNVAL));
                if (ts->poll_array[i].revents &
                    (POLLERR | POLLHUP | POLLNVAL)) {
                    found_fd++;
                    LIXA_TRACE(("server_manager_thread: unespected "
                                "network event on slot " NFDS_T_FORMAT
                                ", fd = %d\n", i, ts->poll_array[i].fd));
                    /* @@@ this piece of code must be improved */
                    THROW(NETWORK_EVENT_ERROR);
                }
                if (ts->poll_array[i].revents & POLLIN) {
                    found_fd++;
                    if (i == 0) {
                        if (LIXA_RC_OK != (
                                ret_cod = server_manager_pollin_ctrl(
                                    ts, ts->poll_array[i].fd)))
                            THROW(POLLIN_CTRL_ERROR);
                    } else {
                        if (LIXA_RC_OK != (
                                ret_cod = server_manager_pollin_data(
                                    ts, i)))
                            THROW(POLLIN_DATA_ERROR);
                    }
                } else if (ts->poll_array[i].revents & POLLOUT) {
                    found_fd++;
                    if (LIXA_RC_OK != (ret_cod = server_manager_pollout(
                                           ts, i)))
                        THROW(POLLOUT_ERROR);
                }
                if (found_fd == ready_fd)
                    break;
            } /* for (i = 0; i < n; ++i) */

        } /* while (TRUE) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ADD_POLL_ERROR:
                break;
            case THREAD_STATUS_SYNC_FILES_ERROR:
            case FIX_POLL_ERROR:
                break;
            case POLL_ERROR:
                ret_cod = LIXA_RC_POLL_ERROR;
                break;
            case NETWORK_EVENT_ERROR:
                ret_cod = LIXA_RC_NETWORK_EVENT_ERROR;
                break;
            case POLLIN_DATA_ERROR:
            case POLLIN_CTRL_ERROR:
            case POLLOUT_ERROR:
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



int server_manager_pollin_ctrl(struct thread_status_s *ts, int fd)
{
    enum Exception { READ_ERROR
                     , ADD_POLL_ERROR
                     , NEW_CLIENT_ERROR
                     , INVALID_MESSAGE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_pollin_ctrl\n"));
    TRY {
        struct srv_msg_s msg;
        nfds_t place = 0;
        
        if (sizeof(msg) != read(fd, &msg, sizeof(msg)))
            THROW(READ_ERROR);
        switch (msg.type) {
            case SRV_MSG_TYPE_NEW_CLIENT:
                if (LIXA_RC_OK != (ret_cod = server_manager_add_poll(
                                       ts, msg.body.nc.fd, &place)))
                    THROW(ADD_POLL_ERROR);
                if (LIXA_RC_OK != (ret_cod = server_manager_new_client(
                                       ts, msg.body.nc.fd, place)))
                    THROW(NEW_CLIENT_ERROR);
                break;
            default:
                THROW(INVALID_MESSAGE);
        } /* switch (msg.type) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case READ_ERROR:
                ret_cod = LIXA_RC_READ_ERROR;
                break;
            case ADD_POLL_ERROR:
                break;
            case NEW_CLIENT_ERROR:
                break;
            case INVALID_MESSAGE:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_pollin_ctrl/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_pollin_data(struct thread_status_s *ts, size_t slot_id)
{
    enum Exception { CLOSE_ERROR
                     , FREE_SLOTS
                     , XML_PROC
                     , MSG_RETRIEVE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_pollin_data\n"));
    TRY {
        char buf[READ_BUFFER_SIZE];
        ssize_t read_bytes;

        ret_cod = lixa_msg_retrieve(ts->poll_array[slot_id].fd,
                                    buf, sizeof(buf), &read_bytes);
        if (LIXA_RC_CONNECTION_CLOSED == ret_cod) {
            /* client has closed the connection */
            /* @@@ check what happens to current transaction */

            /* close socket, release file descriptor and thread status slot */
            LIXA_TRACE(("server_manager_pollin_data: close socket, "
                        "fd = %d\n", ts->poll_array[slot_id].fd));
            if (0 != close(ts->poll_array[slot_id].fd))
                THROW(CLOSE_ERROR);
            if (LIXA_RC_OK != (ret_cod =
                               server_manager_free_slots(ts, slot_id)))
                THROW(FREE_SLOTS);
        } else if (LIXA_RC_OK == ret_cod) {
            /* XML message to process from client */
            if (LIXA_RC_OK != (ret_cod = server_manager_msg_proc(
                                   ts, slot_id, buf, read_bytes)))
                THROW(XML_PROC);
        } else
            THROW(MSG_RETRIEVE_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case FREE_SLOTS:
            case XML_PROC:
            case MSG_RETRIEVE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_pollin_data/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_pollout(struct thread_status_s *ts, size_t slot_id)
{
    enum Exception { SEND_ERROR
                     , STORE_VERB_STEP_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_pollout\n"));
    TRY {
        ssize_t wrote_bytes;
        
        if (ts->client_array[slot_id].output_buffer_size >
            (wrote_bytes = send(ts->poll_array[slot_id].fd,
                                ts->client_array[slot_id].output_buffer,
                                ts->client_array[slot_id].output_buffer_size,
                                0)))
            THROW(SEND_ERROR);
        LIXA_TRACE(("server_manager_pollout: sent " SSIZE_T_FORMAT " bytes "
                    "to client\n", wrote_bytes));
        free(ts->client_array[slot_id].output_buffer);
        ts->client_array[slot_id].output_buffer = NULL;
        ts->client_array[slot_id].output_buffer_size = 0;
        if (LIXA_RC_OK != (ret_cod = payload_header_store_verb_step(
                               ts, slot_id,
                               &ts->client_array[slot_id].last_verb_step)))
            THROW(STORE_VERB_STEP_ERROR);
        ts->client_array[slot_id].last_verb_step.verb = 0;
        ts->client_array[slot_id].last_verb_step.step = 0;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SEND_ERROR:
                ret_cod = LIXA_RC_SEND_ERROR;
                break;
            case STORE_VERB_STEP_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_pollout/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_free_slots(struct thread_status_s *ts, size_t slot_id)
{
    enum Exception { INTERNAL_ERROR
                     , REALLOC_ERROR1
                     , REALLOC_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_free_slots\n"));
    TRY {
        size_t i = ts->poll_size - 1;
        size_t new_poll_size = ts->poll_size;
        
        /* slot reset */
        ts->poll_array[slot_id].fd = LIXA_NULL_FD;
        ts->poll_array[slot_id].events = 0;
        ts->active_clients--;

        while (i > 0 && ts->poll_array[i].fd == LIXA_NULL_FD) {
            new_poll_size--;
            i--;
        }

        if (new_poll_size <= ts->active_clients) {
            LIXA_TRACE(("server_manager_free_slots: new_poll_size = "
                        SIZE_T_FORMAT ", ts->active_clients = "
                        SIZE_T_FORMAT "\n", new_poll_size, ts->active_clients));
            THROW(INTERNAL_ERROR);
        }
        
        /* can we free the slot? */
        if (new_poll_size != ts->poll_size) {
            LIXA_TRACE(("server_manager_free_slots: old poll_size = "
                        SIZE_T_FORMAT ", new poll_size = "
                        SIZE_T_FORMAT "\n", ts->poll_size, new_poll_size));
            if (NULL == (ts->poll_array = realloc(
                             ts->poll_array,
                             new_poll_size * sizeof(struct pollfd))))
                THROW(REALLOC_ERROR1);
            if (NULL == (ts->client_array = realloc(
                             ts->client_array,
                             new_poll_size * sizeof(
                                 struct server_client_status_s))))
                THROW(REALLOC_ERROR2);
            ts->poll_size = new_poll_size;
        }
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case REALLOC_ERROR1:
            case REALLOC_ERROR2:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_free_slots/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_msg_proc(struct thread_status_s *ts, size_t slot_id,
                            char *buf, ssize_t read_bytes)
{
    enum Exception { OUTMSG_PREP_ERROR
                     , INMSG_PROC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int rc;
    
    struct lixa_msg_s lmo;

    LIXA_TRACE(("server_manager_msg_proc\n"));
    TRY {
        /* process the input message */
        rc = server_manager_inmsg_proc(ts, slot_id, buf, read_bytes, &lmo);
        
        if (LIXA_RC_OK != (ret_cod = server_manager_outmsg_prep(
                               ts, slot_id, &lmo, rc))) {
            LIXA_TRACE(("server_manager_msg_proc: server_manager_outmsg_prep "
                        "return code is %d\n", ret_cod));
            THROW(OUTMSG_PREP_ERROR);
        }

        if (LIXA_RC_OK != rc) {
            LIXA_TRACE(("server_manager_msg_proc: server_manager_inmsg_prep "
                        "return code is %d\n", rc));
            THROW(INMSG_PROC_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUTMSG_PREP_ERROR:
            case INMSG_PROC_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_msg_proc/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_inmsg_proc(struct thread_status_s *ts, size_t slot_id,
                              char *buf, ssize_t read_bytes,
                              struct lixa_msg_s *lmo)
{
    enum Exception { LIXA_MSG_DESERIALIZE_ERROR
                     , LIXA_MSG_TRACE_ERROR
                     , SERVER_XA_OPEN_ERROR
                     , SERVER_XA_CLOSE_ERROR
                     , SERVER_XA_START_ERROR
                     , SERVER_XA_END_ERROR
                     , INVALID_VERB
                     , STORE_VERB_STEP_ERROR
                     , LIXA_MSG_FREE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct lixa_msg_s lmi;

    LIXA_TRACE(("server_manager_inmsg_proc\n"));
    TRY {
        uint32_t block_id;
        
        LIXA_TRACE(("server_manager_inmsg_proc: message is |%*.*s|\n",
                    read_bytes, read_bytes, buf));

        /* deserialize the message from XML to native C ... */
        if (LIXA_RC_OK != (ret_cod = lixa_msg_deserialize(
                               buf, read_bytes, &lmi)))
            THROW(LIXA_MSG_DESERIALIZE_ERROR);
#ifdef _TRACE
        if (LIXA_RC_OK != (ret_cod = lixa_msg_trace(&lmi)))
            THROW(LIXA_MSG_TRACE_ERROR);
#endif
        /* retrieve the block is storing the status of the client inside
           memory mapped status file */
        block_id = ts->client_array[slot_id].pers_status_slot_id;

        /* set output message */
        lmo->header.level = LIXA_MSG_LEVEL;
        lmo->header.pvs.verb = LIXA_MSG_VERB_NULL;
        lmo->header.pvs.step = lmi.header.pvs.step + LIXA_MSG_STEP_INCR;
        
        /* process the message */
        switch (lmi.header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN:
                if (LIXA_RC_OK != (ret_cod = server_xa_open(
                                       ts, &lmi, lmo, block_id)))
                    THROW(SERVER_XA_OPEN_ERROR)
                break;
            case LIXA_MSG_VERB_CLOSE:
                if (LIXA_RC_OK != (ret_cod = server_xa_close(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_XA_CLOSE_ERROR)
                break;
            case LIXA_MSG_VERB_START:
                if (LIXA_RC_OK != (ret_cod = server_xa_start(
                                       ts, &lmi, lmo, block_id)))
                    THROW(SERVER_XA_START_ERROR)
                break;
            case LIXA_MSG_VERB_END:
                if (LIXA_RC_OK != (ret_cod = server_xa_end(
                                       ts, &lmi, lmo, block_id)))
                    THROW(SERVER_XA_END_ERROR)
                break;
            default:
                THROW(INVALID_VERB);
        }
        
        /* register protocol step */
        if (LIXA_RC_OK != (ret_cod = payload_header_store_verb_step(
                               ts, block_id, &lmi.header.pvs)))
            THROW(STORE_VERB_STEP_ERROR);
        
        /* release dynamically allocated strings */
        if (LIXA_RC_OK != (ret_cod = lixa_msg_free(&lmi)))
            THROW(LIXA_MSG_FREE_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case LIXA_MSG_DESERIALIZE_ERROR:
            case LIXA_MSG_TRACE_ERROR:
            case LIXA_MSG_FREE_ERROR:
            case SERVER_XA_OPEN_ERROR:
            case SERVER_XA_CLOSE_ERROR:
            case SERVER_XA_START_ERROR:
            case SERVER_XA_END_ERROR:
                break;
            case INVALID_VERB:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case STORE_VERB_STEP_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_inmsg_proc/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_outmsg_prep(struct thread_status_s *ts, size_t slot_id,
                               struct lixa_msg_s *lmo, int rc)
{
    enum Exception { NOTHING_TO_DO
                     , MALLOC_ERROR
                     , VERB_NOT_FOUND
                     , REPLY_OPEN_ERROR
                     , REPLY_START_ERROR
                     , REPLY_END_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_outmsg_prep\n"));
    TRY {
        if (lmo->header.pvs.verb == LIXA_MSG_VERB_NULL)
            THROW(NOTHING_TO_DO);
        
        /* allocate the output buffer */
        if (NULL == (ts->client_array[slot_id].output_buffer = malloc(
                         LIXA_MSG_XML_BUFFER_SIZE))) {
            LIXA_TRACE(("server_manager_outmsg_proc: error while "
                        "allocating the buffer to reply to client\n"));
            THROW(MALLOC_ERROR);
        }

        /* prepare output message */
        switch (lmo->header.pvs.verb) {
            case LIXA_MSG_VERB_NULL:
                break;
            case LIXA_MSG_VERB_OPEN:
                if (LIXA_RC_OK != (ret_cod = server_reply_open(
                                       ts, slot_id, lmo, rc)))
                    THROW(REPLY_OPEN_ERROR);
                break;
            case LIXA_MSG_VERB_START:
                if (LIXA_RC_OK != (ret_cod = server_reply_start(
                                       ts, slot_id, lmo, rc)))
                    THROW(REPLY_START_ERROR);
                break;
            case LIXA_MSG_VERB_END:
                if (LIXA_RC_OK != (ret_cod = server_reply_end(
                                       ts, slot_id, lmo, rc)))
                    THROW(REPLY_END_ERROR);
                break;
            default:
                THROW(VERB_NOT_FOUND);
        } /* switch (lmo.header.pvs.verb) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NOTHING_TO_DO:
                ret_cod = LIXA_RC_OK;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case VERB_NOT_FOUND:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case REPLY_OPEN_ERROR:
            case REPLY_START_ERROR:
            case REPLY_END_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_outmsg_prep/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_add_poll(struct thread_status_s *ts,
                            int new_fd, nfds_t *place)
{
    enum Exception { REALLOC_ERROR1
                     , REALLOC_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_add_poll\n"));
    TRY {
        *place = 0;
        int first_add = ts->poll_array == NULL;

        /* look for a free slot */
        if (ts->poll_size > 1 && ts->active_clients + 1 < ts->poll_size) {
            /* there must be at least one free slot */
            nfds_t i, n = ts->poll_size;
            for (i = 1; i < n; ++i) {
                if (LIXA_NULL_FD == ts->poll_array[i].fd) {
                    *place = i;
                    break;
                }
            }
        }
        if (*place == 0) {
            /* no free slot, allocate a new slot at the end of the array */
            if (NULL == (ts->poll_array = realloc(
                             ts->poll_array,
                             ++ts->poll_size * sizeof(struct pollfd))))
                THROW(REALLOC_ERROR1);
            if (NULL == (ts->client_array = realloc(
                             ts->client_array,
                             ts->poll_size * sizeof(
                                 struct server_client_status_s))))
                THROW(REALLOC_ERROR2);
            *place = ts->poll_size - 1;
        } /* if (*place > 0) */
        ts->poll_array[*place].fd = new_fd;
        ts->poll_array[*place].events = POLLIN;
        if (!first_add)
            ts->active_clients++;

        LIXA_TRACE(("server_manager_add_poll: added file descriptor %d "
                    "at position " NFDS_T_FORMAT "\n",
                    ts->poll_array[*place].fd, *place));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR1:
            case REALLOC_ERROR2:
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



int server_manager_fix_poll(struct thread_status_s *ts)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_fix_poll\n"));
    TRY {
        nfds_t i;

        for (i=1; i<ts->poll_size; ++i) {
            /* skip records are not used */
            if (ts->poll_array[i].fd == LIXA_NULL_FD)
                continue;
            if (ts->client_array[i].output_buffer == NULL)
                ts->poll_array[i].events = POLLIN;
            else
                ts->poll_array[i].events = POLLOUT;
        }
        
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
    LIXA_TRACE(("server_manager_fix_poll/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_new_client(struct thread_status_s *ts, int fd, nfds_t place)
{
    enum Exception { RECORD_INSERT_ERROR
                     , PAYLOAD_HEADER_INIT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_new_client\n"));
    TRY {
        uint32_t slot = 0;
        
        /* get a free block from status file and insert in used list */
        if (LIXA_RC_OK != (ret_cod = status_record_insert(ts, &slot)))
            THROW(RECORD_INSERT_ERROR);

        /* create the header and reset it */
        if (LIXA_RC_OK != (ret_cod = payload_header_init(
                               &ts->curr_status[slot].sr.data, fd)))
            THROW(PAYLOAD_HEADER_INIT);

        /* save a reference to the slot */
        ts->client_array[place].pers_status_slot_id = place;
        /* reset the output buffer pointer (it may be garbage if unused
           before */
        ts->client_array[place].output_buffer = NULL;
        ts->client_array[place].output_buffer_size = 0;
        ts->client_array[place].last_verb_step.verb = 0;
        ts->client_array[place].last_verb_step.step = 0;
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case RECORD_INSERT_ERROR:
                break;
            case PAYLOAD_HEADER_INIT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_new_client/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

