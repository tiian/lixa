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
#include <server_manager.h>
#include <server_messages.h>



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
                     , STATUS_FILE_ERROR
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
            tsa->array[i].active_clients = 0;
            tsa->array[i].client_array = NULL;
            tsa->array[i].status = NULL;
            tsa->array[i].excp = tsa->array[i].ret_cod =
                tsa->array[i].last_errno = 0;
            if (i == 0) { /* listener */
                tsa->array[i].tid = pthread_self();
            } else {
                /* load status file */
                if (LIXA_RC_OK != (ret_cod = status_record_load(
                                       &tsa->array[i].status,
                                       sc->managers.array[i-1].status_file)))
                    THROW(STATUS_FILE_ERROR);
                
                /* it will be fixed by the thread itself */
                tsa->array[i].tid = 0;
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
            case STATUS_FILE_ERROR:
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
                     , NETWORK_EVENT_ERROR
                     , POLLIN_CTRL
                     , POLLIN_DATA
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
            LIXA_TRACE(("server_manager_thread: id = %d, entering poll...\n",
                        ts->id));
            if (0 >= (ready_fd = poll(ts->poll_array, ts->poll_size, -1)))
                THROW(POLL_ERROR);
            LIXA_TRACE(("server_manager_thread: id = %d, ready file "
                        "descriptors = %d\n", ts->id, ready_fd));
            found_fd = 0;
            n = ts->poll_size;
            for (i = 0; i < n; ++i) {
                LIXA_TRACE(("server_manager_thread: slot = " NFDS_T_FORMAT
                            ", fd = %d, POLLIN = %d, POLLERR = %d, "
                            "POLLHUP = %d, POLLNVAL = %d\n",
                            i, ts->poll_array[i].fd,
                            ts->poll_array[i].revents & POLLIN,
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
                            THROW(POLLIN_CTRL);
                    } else {
                        if (LIXA_RC_OK != (
                                ret_cod = server_manager_pollin_data(
                                    ts, i)))
                            THROW(POLLIN_DATA);
                    }
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
            case POLL_ERROR:
                ret_cod = LIXA_RC_POLL_ERROR;
                break;
            case NETWORK_EVENT_ERROR:
                ret_cod = LIXA_RC_NETWORK_EVENT_ERROR;
                break;
            case POLLIN_DATA:
            case POLLIN_CTRL:
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
    enum Exception { READ_ERROR
                     , PAYLOAD_CHAIN_RELEASE
                     , CLOSE_ERROR
                     , FREE_SLOTS
                     , XML_PROC
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_manager_pollin_data\n"));
    TRY {
        char buf[READ_BUFFER_SIZE];
        ssize_t read_bytes;

        if (0 > (read_bytes = recv(ts->poll_array[slot_id].fd, buf,
                                   sizeof(buf), 0)))
            THROW(READ_ERROR);
        LIXA_TRACE(("server_manager_pollin_data: fd = %d returned %u bytes\n",
                    ts->poll_array[slot_id].fd, read_bytes));
        if (0 == read_bytes) {
            /* client has closed the connection */
            /* @@@ check what happens to current transaction */

            /* release all allocated blocks */
            if (LIXA_RC_OK != (ret_cod = payload_chain_release(
                                   &ts->status,
                                   ts->client_array[slot_id].
                                   pers_status_slot_id)))
                THROW(PAYLOAD_CHAIN_RELEASE);
            
            /* close socket, release file descriptor and thread status slot */
            LIXA_TRACE(("server_manager_pollin_data: close socket, "
                        "fd = %d\n", ts->poll_array[slot_id].fd));
            if (0 != close(ts->poll_array[slot_id].fd))
                THROW(CLOSE_ERROR);
            if (LIXA_RC_OK != (ret_cod =
                               server_manager_free_slots(ts, slot_id)))
                THROW(FREE_SLOTS);
        } else {
            /* XML message to process from client */
            if (LIXA_RC_OK != (ret_cod = server_manager_XML_proc(
                                   ts, slot_id, buf, read_bytes)))
                THROW(XML_PROC);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case READ_ERROR:
                ret_cod = LIXA_RC_READ_ERROR;
                break;
            case PAYLOAD_CHAIN_RELEASE:
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case FREE_SLOTS:
            case XML_PROC:
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



int server_manager_XML_proc(struct thread_status_s *ts, size_t slot_id,
                            const char *buf, ssize_t read_bytes)
{
    enum Exception { XML_READ_MEMORY_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlDocPtr doc;
    xmlNode *root_element = NULL;
    
    LIXA_TRACE(("server_manager_XML_proc\n"));
    TRY {
        LIXA_TRACE(("server_manager_XML_proc: message is |%*.*s|\n",
                    read_bytes, read_bytes, buf));

        /* load XML tree */
        if (NULL == (doc = xmlReadMemory(buf, (int)read_bytes, "buffer.xml",
                                         NULL, 0)))
            THROW(XML_READ_MEMORY_ERROR);

        /* retrieve root element from XML tree */
        if (NULL == (root_element = xmlDocGetRootElement(doc)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        /*
        parse the document...
        */
        
        /* free parsed document */
        xmlFreeDoc(doc);

        /* release libxml2 stuff */
        xmlCleanupParser();

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XML_READ_MEMORY_ERROR:
                ret_cod = LIXA_RC_XML_READ_MEMORY_ERROR;
                break;
            case XML_DOC_GET_ROOT_ELEMENT_ERROR:
                ret_cod = LIXA_RC_XML_DOC_GET_ROOT_ELEMENT_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (excp > XML_READ_MEMORY_ERROR && excp < NONE) {
            xmlFreeDoc(doc);
            xmlCleanupParser();
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_XML_proc/excp=%d/"
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
        if (LIXA_RC_OK != (ret_cod = status_record_insert(&ts->status, &slot)))
            THROW(RECORD_INSERT_ERROR);

        /* create the header and reset it */
        if (LIXA_RC_OK != (ret_cod = payload_header_init(
                               &ts->status[slot].data, fd)))
            THROW(PAYLOAD_HEADER_INIT);

        /* save a reference to the slot */
        ts->client_array[place].pers_status_slot_id = place;
        
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

