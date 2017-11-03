/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif



#include "lixa_crash.h"
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_syslog.h"
#include "lixa_xml_msg_deserialize.h"
#include "lixa_xml_msg_trace.h"
#include "server_manager.h"
#include "server_messages.h"
#include "server_thread_status.h"
#include "server_recovery.h"
#include "server_reply.h"
#include "server_xa.h"
#include "server_tpm.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_MANAGER

GMutex state_file_synchronization;

int server_manager(struct server_config_s *sc,
                   struct thread_pipe_array_s *tpa,
                   struct thread_status_array_s *tsa,
                   srvr_rcvr_tbl_t *srt,
                   server_trans_tbl_t *stt,
                   const struct ts_dump_spec_s *tsds,
                   const struct ts_recovery_spec_s *tsrs, int mmode)
{
    enum Exception
    {
        SRVR_RCVR_TBL_NEW_ERROR,
        SERVER_TRANS_TBL_NEW_ERROR,
        MALLOC_ERROR,
        THREAD_STATUS_LOAD_FILES_ERROR,
        THREAD_STATUS_DUMP_ERROR,
        THREAD_STATUS_CLEAN_FAILED_ERROR,
        THREAD_STATUS_RECOVERY_ERROR,
        PTHREAD_CREATE_ERROR,
        NULL_SERVER_TRANS_TABLE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager\n"));
    TRY {
        int i;
        static long crash_count = 0; /* this mimics a global var */

        LIXA_TRACE(("server_manager: number of managers to activate = %d\n",
                    sc->managers.n));

        if (LIXA_RC_OK != (ret_cod = srvr_rcvr_tbl_init(srt, tpa->n)))
            THROW(SRVR_RCVR_TBL_NEW_ERROR);

        if (LIXA_RC_OK != (ret_cod = server_trans_tbl_init(stt, tpa->n)))
            THROW(SERVER_TRANS_TBL_NEW_ERROR);

        /* prepare thread status array structure */
        if (NULL == (tsa->array = malloc(
                         tpa->n * sizeof(struct thread_status_s))))
            THROW(MALLOC_ERROR);
        tsa->n = tpa->n;

        /* first thread slot is for listener: it's the main thread of the
         * process, it's IMPLICITLY created */
        for (i = 0; i < tsa->n; ++i) {
            thread_status_init(&(tsa->array[i]), i, tpa, mmode, &crash_count);
            tsa->array[i].min_elapsed_sync_time = sc->min_elapsed_sync_time;
            tsa->array[i].max_elapsed_sync_time = sc->max_elapsed_sync_time;
            if (i) {
                /* load status file for thread != listener */
                if (LIXA_RC_OK != (
                        ret_cod = thread_status_load_files(
                            &(tsa->array[i]),
                            sc->managers.array[i - 1].status_file, tsds))) THROW(
                                THREAD_STATUS_LOAD_FILES_ERROR);
                /* dump the status file if asked */
                if (tsds->dump) {
                    if (LIXA_RC_OK != (ret_cod = thread_status_dump(
                                           &(tsa->array[i]), tsds))) THROW(
                                               THREAD_STATUS_DUMP_ERROR);
                    continue;
                }
                if (tsrs->clean_failed &&
                    LIXA_RC_OK != (ret_cod = thread_status_clean_failed(
                                       &(tsa->array[i])))) THROW(
                                           THREAD_STATUS_CLEAN_FAILED_ERROR);

                /* enqueue recovery pending transactions */
                if (LIXA_RC_OK != (ret_cod = thread_status_recovery(
                                       &(tsa->array[i]), srt))) THROW(
                                           THREAD_STATUS_RECOVERY_ERROR);

                /* link to global transaction table */
                if (NULL == stt) THROW(NULL_SERVER_TRANS_TABLE);
                tsa->array[i].trans_table = stt;

                /* it will be fixed by the thread itself */
                if (0 != (ret_cod = pthread_create(
                              &(tsa->array[i].tid), NULL,
                              server_manager_thread, tsa->array + i))) THROW(
                                  PTHREAD_CREATE_ERROR);

                LIXA_TRACE(("server_manager: started thread "
                            PTHREAD_T_FORMAT
                            "\n", tsa->array[i].tid));
            }
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case NULL_SERVER_TRANS_TABLE:
                    ret_cod = LIXA_RC_NULL_OBJECT;
                    break;
                case SRVR_RCVR_TBL_NEW_ERROR:
                case SERVER_TRANS_TBL_NEW_ERROR:
                    break;
                case MALLOC_ERROR:
                    ret_cod = LIXA_RC_MALLOC_ERROR;
                    break;
                case THREAD_STATUS_LOAD_FILES_ERROR:
                case THREAD_STATUS_DUMP_ERROR:
                case THREAD_STATUS_CLEAN_FAILED_ERROR:
                case THREAD_STATUS_RECOVERY_ERROR:
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

int server_pipes_init(struct thread_pipe_array_s *tpa)
{
    enum Exception
    {
        PIPE_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_pipes_init\n"));
    TRY {
        int i;

        for (i = 0; i < tpa->n; ++i) {
            if (0 != pipe(tpa->array[i].pipefd)) THROW(PIPE_ERROR);
            LIXA_TRACE(("server_pipes_init: pipe for %s (%d) is [%d,%d]\n",
                        i ? "manager" : "listener", i,
                        tpa->array[i].pipefd[0], tpa->array[i].pipefd[1]));
        }
        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case PIPE_ERROR:
                    ret_cod = LIXA_RC_PIPE_ERROR;
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_pipes_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void *server_manager_thread(void *void_ts)
{
    enum Exception { ADD_POLL_ERROR,
                     THREAD_STATUS_SYNC_FILES_ERROR1,
                     THREAD_STATUS_SYNC_FILES_ERROR2,
                     FIX_POLL_ERROR,
                     POLL_ERROR,
                     DROP_CLIENT_ERROR,
                     NETWORK_EVENT_ERROR,
                     POLLIN_CTRL_ERROR,
                     POLLIN_DATA_ERROR,
                     POLLOUT_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct thread_status_s *ts = (struct thread_status_s *) void_ts;

    LIXA_TRACE(("server_manager_thread\n"));
    TRY {
        int ready_fd, found_fd;
        nfds_t place = 0, i, n;

        LIXA_TRACE(("server_manager_thread: id=%d, "
                    "tid="
                    PTHREAD_T_FORMAT
                    "\n", ts->id, ts->tid));
        if (LIXA_RC_OK != (ret_cod = server_manager_add_poll(
                               ts, ts->tpa->array[ts->id].pipefd[0], &place)))
            THROW(ADD_POLL_ERROR);
        while (TRUE) {
            int timeout;
            long delay = 0;
            if (status_sync_get_asked(&ts->status_sync) > 0) {
                delay = status_sync_get_sync_delay(&ts->status_sync) / 1000;
                LIXA_TRACE(("server_manager_thread: %d session(s) asked file "
                            "status synchronization; current delay is %ld ms, "
                            "min_delay=%ld ms, max_delay=%ld ms\n",
                            status_sync_get_asked(&ts->status_sync), delay,
                            ts->min_elapsed_sync_time,
                            ts->max_elapsed_sync_time));
                if (delay >= ts->max_elapsed_sync_time ||
                    SHUTDOWN_NULL != ts->shutdown_type) {
                    /* start synchronization as soon as possible. This
                       operation does not enqueue on the mutex but will
                       enqueues on disk I/O; a better algorithm would use
                       a mutex protected counter, but it will cost twice
                       mutex synchronizations. This must be considered a
                       "good enought" approach to reduce disk enqueing, but
                       not to avoid disk enqueing (the operating system will
                       manage it without any issue) */
                    if (LIXA_RC_OK !=
                        (ret_cod = thread_status_sync_files(ts)))
                        THROW(THREAD_STATUS_SYNC_FILES_ERROR1);
                    status_sync_init(&ts->status_sync);
                } else if (delay >= ts->min_elapsed_sync_time) {
                    /* start synchronization only if there is no another thread
                       that's synchronizing its own state file */
                    if (g_mutex_trylock(&state_file_synchronization)) {
                        ret_cod = thread_status_sync_files(ts);
                        g_mutex_unlock(&state_file_synchronization);
                        if (LIXA_RC_OK != ret_cod)
                            THROW(THREAD_STATUS_SYNC_FILES_ERROR2);
                        status_sync_init(&ts->status_sync);
                    }
                }
            }
            if (SHUTDOWN_NULL != ts->shutdown_type) {
                LIXA_TRACE(("server_manager_thread: id=%d, leaving main "
                            "loop and shutdown...\n", ts->id));
                break;
            }
            if (LIXA_RC_OK != (ret_cod = server_manager_fix_poll(ts)))
                THROW(FIX_POLL_ERROR);
            if (status_sync_get_asked(&ts->status_sync) > 0)
                /* limited timeout poll */
                timeout = ts->max_elapsed_sync_time - delay;
            else
                timeout = -1; /* unlimited timeout poll */
            LIXA_TRACE(("server_manager_thread: id=%d, entering poll "
                        "with timeout=%dms\n",
                        ts->id, timeout));
            if (0 >=
                (ready_fd = poll(ts->poll_array, ts->poll_size, -1)))
                THROW(POLL_ERROR);
            LIXA_TRACE(("server_manager_thread: id=%d, ready file "
                        "descriptors=%d\n", ts->id, ready_fd));
            found_fd = 0;
            n = ts->poll_size;
            for (i = 0; i < n; ++i) {
                int connection_closed = FALSE;
                LIXA_TRACE(("server_manager_thread: slot="
                            NFDS_T_FORMAT
                            ", fd=%d, POLLIN=%d, POLLOUT=%d, POLLERR=%d, "
                            "POLLHUP=%d, POLLNVAL=%d, control_only=%d\n",
                            i, ts->poll_array[i].fd,
                            ts->poll_array[i].revents & POLLIN,
                            ts->poll_array[i].revents & POLLOUT,
                            ts->poll_array[i].revents & POLLERR,
                            ts->poll_array[i].revents & POLLHUP,
                            ts->poll_array[i].revents & POLLNVAL,
                            i ? ts->client_array[i].control_only : 0));
                if (LIXA_NULL_FD == ts->poll_array[i].fd) {
                    LIXA_TRACE(("server_manager_thread: this slot must "
                                "be temporary skipped\n"));
                    continue;
                }
                if (ts->poll_array[i].revents &
                    (POLLERR | POLLHUP | POLLNVAL)) {
                    if ((ts->poll_array[i].revents & POLLERR) ||
                        (ts->poll_array[i].revents & POLLHUP)) {
                        found_fd++;
                        LIXA_TRACE(("server_manager_thread: hang-up event, "
                                    "the client broke the connection\n"));
                        if (LIXA_RC_OK != (
                                ret_cod = server_manager_drop_client(
                                    ts, i)))
                            THROW(DROP_CLIENT_ERROR);
                        connection_closed = TRUE;
                    } else {
                        LIXA_TRACE(("server_manager_thread: unexpected "
                                    "network event slot="
                                    NFDS_T_FORMAT
                                    ", fd=%d\n", i, ts->poll_array[i].fd));
                        THROW(NETWORK_EVENT_ERROR);
                    }
                } else if (ts->poll_array[i].revents & POLLIN) {
                    found_fd++;
                    if (!i) {
                        ret_cod = server_manager_pollin_ctrl(
                            ts, ts->poll_array[i].fd);
                        if (LIXA_RC_ASKED_SHUTDOWN == ret_cod) {
                            status_sync_ask_sync(&ts->status_sync);
                            if (SHUTDOWN_FORCE == ts->shutdown_type) {
                                LIXA_TRACE(("server_manager_thread: break "
                                            "current loop to perform force "
                                            "shutdown\n"));
                                break;
                            }
                        } else if (LIXA_RC_OK != ret_cod)
                            THROW(POLLIN_CTRL_ERROR);
                    } else {
                        ret_cod = server_manager_pollin_data(ts, i);
                        switch (ret_cod) {
                            case LIXA_RC_OK:
                                break;
                            case LIXA_RC_CONNECTION_CLOSED:
                                connection_closed = TRUE;
                                break;
                            case LIXA_RC_THREAD_SWITCH:
                                ret_cod = server_manager_switch_1(ts, i);
                                break;
                            default:
                                THROW(POLLIN_DATA_ERROR);
                        }
                    }
                }
                if (!connection_closed &&
                    ts->poll_array[i].revents & POLLOUT) {
                    found_fd++;
                    if (LIXA_RC_OK != (
                            ret_cod = server_manager_pollout(ts, i)))
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
            case THREAD_STATUS_SYNC_FILES_ERROR1:
            case THREAD_STATUS_SYNC_FILES_ERROR2:
            case FIX_POLL_ERROR:
                break;
            case POLL_ERROR:
                ret_cod = LIXA_RC_POLL_ERROR;
                break;
            case DROP_CLIENT_ERROR:
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
        /* update thread status */
        ts->excp = excp;
        ts->ret_cod = ret_cod;
        ts->last_errno = errno;
        if (NONE != excp)
            syslog(LOG_CRIT, LIXA_SYSLOG_LXD018C, ts->id, ts->excp,
                   ts->ret_cod, ts->last_errno);
        /* call clean-up routine */
        server_manager_thread_cleanup(ts);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_thread/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return NULL;
}



void server_manager_thread_cleanup(struct thread_status_s *ts)
{
    /* closing control pipe input fd */
    int fd, i;
    struct srv_msg_s msg;

    fd = ts->tpa->array[ts->id].pipefd[1];
    LIXA_TRACE(("server_manager_thread_cleanup: closing writer control pipe "
                "...\n"));
    ts->tpa->array[ts->id].pipefd[1] = LIXA_NULL_FD;
    LIXA_TRACE(("server_manager_thread_cleanup: close(%d)=%d, errno=%d\n",
                fd, close(fd), errno));

    fd = ts->tpa->array[ts->id].pipefd[0];
    LIXA_TRACE(("server_manager_thread_cleanup: closing reader control pipe "
                "...\n"));
    ts->tpa->array[ts->id].pipefd[0] = LIXA_NULL_FD;
    LIXA_TRACE(("server_manager_thread_cleanup: close(%d)=%d, errno=%d\n",
                fd, close(fd), errno));

    /* scan all file descriptors and close it */
    for (i = 1; i < ts->poll_size; ++i) {
        if (ts->poll_array[i].fd == LIXA_NULL_FD)
            continue;
        fd = ts->poll_array[i].fd;
        LIXA_TRACE(("server_manager_thread_cleanup: closing socket...\n"));
        ts->poll_array[i].fd = LIXA_NULL_FD;
        LIXA_TRACE(("server_manager_thread_cleanup: close(%d)=%d, errno=%d\n",
                    fd, close(fd), errno));
    }

    /* notify a shutdown message to all the threads */
    memset(&msg, 0, sizeof(msg));
    msg.type = SRV_MSG_TYPE_SHUTDOWN;
    msg.body.sd.type = SHUTDOWN_IMMEDIATE;
    for (i = 0; i < tpa.n; ++i) {
        if (i == ts->id)
            continue; /* skipping myself */
        if (LIXA_NULL_FD == tpa.array[i].pipefd[1]) {
            LIXA_TRACE(("server_manager_thread_cleanup: thread id %d closed "
                        "control pipe, skipping...\n", i));
            continue;
        } else
            LIXA_TRACE(("server_manager_thread_cleanup: sending shutdown "
                        "message to thread id %d\n", i));
        if (sizeof(msg) != write(tpa.array[i].pipefd[1], &msg, sizeof(msg)))
            LIXA_TRACE(("server_manager_thread_cleanup: error while writing "
                        "to thread id %d (errno=%d)\n", i, errno));
    }

    /* clean-up memory */
    thread_status_destroy(ts);
    return;
}


int server_manager_pollin_ctrl(struct thread_status_s *ts, int fd)
{
    enum Exception
    {
        READ_ERROR,
        SETSOCKOPT_ERROR,
        ADD_POLL_ERROR,
        NEW_CLIENT_ERROR,
        SWITCH_2,
        SWITCH_3,
        SHUTDOWN_ERROR,
        ASKED_SHUTDOWN,
        INVALID_MESSAGE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_pollin_ctrl\n"));
    TRY {
        struct srv_msg_s msg;
        nfds_t place = 0;
        int sock_opt = 1;

        if (sizeof(msg) != read(fd, &msg, sizeof(msg))) THROW(READ_ERROR);
        switch (msg.type) {
            case SRV_MSG_TYPE_NEW_CLIENT:
                /* disable Nagle's algorithm to reduce latency */
                if (0 != setsockopt(msg.body.nc.fd, IPPROTO_TCP, TCP_NODELAY,
                                    (void *) (&sock_opt),
                                    sizeof(sock_opt))) THROW(SETSOCKOPT_ERROR);
                if (LIXA_RC_OK != (ret_cod = server_manager_add_poll(
                                       ts, msg.body.nc.fd, &place))) THROW(ADD_POLL_ERROR);
                if (LIXA_RC_OK != (ret_cod = server_manager_new_client(
                                       ts, msg.body.nc.fd, place))) THROW(NEW_CLIENT_ERROR);
                break;
            case SRV_MSG_TYPE_SWITCH_REQ:
                if (LIXA_RC_OK != (
                        ret_cod = server_manager_switch_2(ts, &msg))) THROW(
                            SWITCH_2);
                break;
            case SRV_MSG_TYPE_SWITCH_REP:
                if (LIXA_RC_OK != (
                        ret_cod = server_manager_switch_3(ts, &msg))) THROW(
                            SWITCH_3);
                break;
            case SRV_MSG_TYPE_SHUTDOWN:
                if (LIXA_RC_OK != (
                        ret_cod = server_manager_shutdown(ts, &msg))) THROW(
                            SHUTDOWN_ERROR);
                THROW(ASKED_SHUTDOWN);
                break;
            default: THROW(INVALID_MESSAGE);
        } /* switch (msg.type) */

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case READ_ERROR:
                    ret_cod = LIXA_RC_READ_ERROR;
                    break;
                case SETSOCKOPT_ERROR:
                    ret_cod = LIXA_RC_SETSOCKOPT_ERROR;
                    break;
                case ADD_POLL_ERROR:
                case NEW_CLIENT_ERROR:
                case SWITCH_2:
                case SWITCH_3:
                case SHUTDOWN_ERROR:
                    break;
                case ASKED_SHUTDOWN:
                    ret_cod = LIXA_RC_ASKED_SHUTDOWN;
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
    enum Exception { DROP_CLIENT_ERROR,
                     CONNECTION_CLOSED,
                     INTERNAL_ERROR,
                     MALLOC_ERROR,
                     THREAD_SWITCH,
                     XML_PROC,
                     DROP_CLIENT_ERROR2,
                     MSG_RETRIEVE_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_pollin_data\n"));
    TRY {
        char buf[READ_BUFFER_SIZE];
        ssize_t read_bytes;

        ret_cod = lixa_msg_retrieve(ts->poll_array[slot_id].fd,
                                    buf, sizeof(buf), &read_bytes);
        if (LIXA_RC_CONNECTION_CLOSED == ret_cod) {
            /* client has closed the connection, dropping it */
            if (LIXA_RC_OK != (ret_cod =
                               server_manager_drop_client(ts, slot_id)))
                THROW(DROP_CLIENT_ERROR);
            THROW(CONNECTION_CLOSED);
        } else if (LIXA_RC_OK == ret_cod) {
            struct thread_status_switch_s *tss =
                &(ts->client_array[slot_id].switch_thread);
            /* XML message to process from client */
            ret_cod = server_manager_msg_proc(ts, slot_id, buf, read_bytes);
            switch (ret_cod) {
                case LIXA_RC_OK:
                    break;
                case LIXA_RC_THREAD_SWITCH:
#ifdef LIXA_DEBUG
                    if (NULL != tss->buffer)
                        THROW(INTERNAL_ERROR);
#endif
                    if (NULL == (tss->buffer = malloc(read_bytes + 1)))
                        THROW(MALLOC_ERROR);
                    memcpy(tss->buffer, buf, read_bytes);
                    tss->buffer[read_bytes] = '\0';
                    tss->buffer_size = read_bytes;
                    THROW(THREAD_SWITCH);
                case LIXA_RC_MAINTENANCE_MODE:
                    break;
                case LIXA_RC_BRANCHES_ON_MULTIPLE_THREADS:
                    /* drop client connection */
                    if (LIXA_RC_OK != (ret_cod =
                                       server_manager_drop_client(
                                           ts, slot_id)))
                        THROW(DROP_CLIENT_ERROR2);
                    break;
                default:
                    THROW(XML_PROC);
            }
        } else
            THROW(MSG_RETRIEVE_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case DROP_CLIENT_ERROR:
                break;
            case CONNECTION_CLOSED:
                ret_cod = LIXA_RC_CONNECTION_CLOSED;
                break;
#ifdef LIXA_DEBUG
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
#endif
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case THREAD_SWITCH:
            case DROP_CLIENT_ERROR2:
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



int server_manager_drop_client(struct thread_status_s *ts, size_t slot_id)
{
    enum Exception
    {
        BLOCK_STATUS_ERROR,
        RECOVERY_TABLE_INSERT_ERROR,
        PAYLOAD_CHAIN_RELEASE,
        CLOSE_ERROR,
        FREE_SLOTS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_drop_client\n"));
    TRY {
        int rec_pend = FALSE;
        uint32_t block_id = ts->client_array[slot_id].pers_status_slot_id;
        struct status_record_data_s *data =
            &(ts->curr_status[block_id].sr.data);

        if (LIXA_RC_OK != (ret_cod = thread_status_check_recovery_pending(
                               data, &rec_pend))) THROW(BLOCK_STATUS_ERROR);
        if (rec_pend) {
            struct srvr_rcvr_tbl_rec_s srtr;
            /* insert a new record in the recovery pending table */
            srtr.job = &data->pld.ph.job;
            srtr.tsid = ts->id;
            srtr.block_id = block_id;
            if (LIXA_RC_OK != (ret_cod = srvr_rcvr_tbl_insert(
                                   ts->recovery_table, &srtr))) THROW(RECOVERY_TABLE_INSERT_ERROR);
        } else {
            /* release blocks in status file: now they are useless */
            if (LIXA_RC_OK != (
                    ret_cod = payload_chain_release(ts, block_id))) THROW(
                        PAYLOAD_CHAIN_RELEASE);
        }
        /* close socket, release file descriptor and thread status slot */
        LIXA_TRACE(("server_manager_pollin_data: close socket, "
                    "fd = %d\n", ts->poll_array[slot_id].fd));
        if (0 != close(ts->poll_array[slot_id].fd)) THROW(CLOSE_ERROR);
        if (LIXA_RC_OK != (ret_cod =
                           server_manager_free_slots(ts, slot_id))) THROW(
                               FREE_SLOTS);

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case BLOCK_STATUS_ERROR:
                case RECOVERY_TABLE_INSERT_ERROR:
                case PAYLOAD_CHAIN_RELEASE:
                    break;
                case CLOSE_ERROR:
                    ret_cod = LIXA_RC_CLOSE_ERROR;
                    break;
                case FREE_SLOTS:
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_drop_client/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_manager_switch_1(struct thread_status_s *ts,
                            size_t slot_id)
{
    enum Exception
    {
        WRITE_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    LIXA_TRACE(("server_manager_switch_1\n"));
    TRY {
        struct srv_msg_s msg;
        uint32_t block_id = ts->client_array[slot_id].pers_status_slot_id;
        int i;
        struct thread_status_switch_s *tss =
            &(ts->client_array[slot_id].switch_thread);

        LIXA_TRACE(("server_manager_switch_1: source thread id=%d, "
                    "source slot id="
                    SIZE_T_FORMAT
                    ", source block id="
                    UINT32_T_FORMAT
                    ", destination thread "
                    "id=%d, input buffer is '%s'\n",
                    ts->id, slot_id, block_id, tss->id, tss->buffer));
        /* prepare the message for the destination thread */
        memset(&msg, 0, sizeof(msg));
        msg.type = SRV_MSG_TYPE_SWITCH_REQ;
        msg.body.sr.source = ts->id;
        msg.body.sr.fd = ts->poll_array[slot_id].fd;
        msg.body.sr.block_id = block_id;
        msg.body.sr.slot_id = slot_id;
        msg.body.sr.buffer = tss->buffer;
        msg.body.sr.buffer_size = tss->buffer_size;
        msg.body.sr.header = &(ts->curr_status[block_id].sr.data.pld.ph);
        for (i = 0; i < msg.body.sr.header->n; ++i) {
            msg.body.sr.rsrmgr[i] = &(ts->curr_status[
                                          msg.body.sr.header->block_array[i]
                                                      ].sr.data.pld.rm);
        }

        /* move the thread to control only state */
        ts->client_array[slot_id].control_only = TRUE;
        /* send the message to the destination thread */
        if (sizeof(msg) != write(ts->tpa->array[tss->id].pipefd[1],
                                 &msg, sizeof(msg))) THROW(WRITE_ERROR);

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case WRITE_ERROR:
                    ret_cod = LIXA_RC_WRITE_ERROR;
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_switch_1/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_manager_switch_2(struct thread_status_s *ts,
                            const struct srv_msg_s *msg)
{
    enum Exception
    {
        ADD_POLL_ERROR,
        NEW_CLIENT_ERROR,
        CHAIN_ALLOCATE_ERROR,
        MSG_PROC_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct srv_msg_s answer;

    LIXA_TRACE(("server_manager_switch_2\n"));
    TRY {
        nfds_t slot_id = 0;
        uint32_t block_id;
        uint32_t tmp_block_array[CHAIN_MAX_SIZE];
        int i;

        if (LIXA_RC_OK != (ret_cod = server_manager_add_poll(
                               ts, msg->body.sr.fd, &slot_id))) THROW(ADD_POLL_ERROR);
        if (LIXA_RC_OK != (ret_cod = server_manager_new_client(
                               ts, msg->body.sr.fd, slot_id))) THROW(NEW_CLIENT_ERROR);
        block_id = ts->client_array[slot_id].pers_status_slot_id;
        if (LIXA_RC_OK != (ret_cod = payload_chain_allocate(
                               ts, block_id, msg->body.sr.header->n))) THROW(CHAIN_ALLOCATE_ERROR);
        /* copy header block */
        /* save the chain block array */
        memcpy(tmp_block_array,
               &ts->curr_status[block_id].sr.data.pld.ph.block_array,
               sizeof(tmp_block_array));
        /* copy header block */
        memcpy(&ts->curr_status[block_id].sr.data.pld.ph,
               msg->body.sr.header, sizeof(struct payload_header_s));
        /* restore the chain block array */
        memcpy(&ts->curr_status[block_id].sr.data.pld.ph.block_array,
               tmp_block_array,
               sizeof(tmp_block_array));
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        /* copy rsrmgr blocks */
        for (i = 0; i < msg->body.sr.header->n; ++i) {
            uint32_t child_block_id =
                ts->curr_status[block_id].sr.data.pld.ph.block_array[i];
            memcpy(&ts->curr_status[child_block_id].sr.data.pld.rm,
                   msg->body.sr.rsrmgr[i], sizeof(struct payload_rsrmgr_s));
            status_record_update(ts->curr_status + child_block_id,
                                 child_block_id, ts->updated_records);
        }

        if (LIXA_RC_OK != (ret_cod = server_manager_msg_proc(
                               ts, slot_id, msg->body.sr.buffer,
                               msg->body.sr.buffer_size))) THROW(MSG_PROC_ERROR);

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case ADD_POLL_ERROR:
                case NEW_CLIENT_ERROR:
                case CHAIN_ALLOCATE_ERROR:
                case MSG_PROC_ERROR:
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
            /* prepare answer message */
            memset(&answer, 0, sizeof(answer));
            answer.type = SRV_MSG_TYPE_SWITCH_REP;
            answer.body.sp.result = ret_cod;
            answer.body.sp.block_id = msg->body.sr.block_id;
            answer.body.sp.slot_id = msg->body.sr.slot_id;
            if (sizeof(answer) != write(
                    ts->tpa->array[msg->body.sr.source].pipefd[1],
                    &answer, sizeof(answer))) {
                LIXA_TRACE(("server_manager_switch_2: error while writing to "
                            "control pipe of thread # %d\n", msg->body.sr.source));
                ret_cod = LIXA_RC_WRITE_ERROR;
            }
        } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_switch_2/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_manager_switch_3(struct thread_status_s *ts,
                            const struct srv_msg_s *msg)
{
    enum Exception
    {
        PAYLOAD_CHAIN_RELEASE, MANAGER_FREE_SLOTS, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_switch_3\n"));
    TRY {
        struct thread_status_switch_s *tss =
            &(ts->client_array[msg->body.sp.slot_id].switch_thread);
        /* clear switch info */
        ts->client_array[msg->body.sp.slot_id].control_only = FALSE;
        tss->id = 0;
        tss->buffer_size = 0;
        if (NULL != tss->buffer) {
            free(tss->buffer);
            tss->buffer = NULL;
        }
        if (LIXA_RC_OK == msg->body.sp.result) {
            LIXA_TRACE(("server_manager_switch_3: release switched client\n"));
            if (LIXA_RC_OK != (ret_cod = payload_chain_release(
                                   ts, msg->body.sp.block_id))) THROW(PAYLOAD_CHAIN_RELEASE);
            if (LIXA_RC_OK != (ret_cod = server_manager_free_slots(
                                   ts, msg->body.sp.slot_id))) THROW(MANAGER_FREE_SLOTS);
        } else {
            LIXA_TRACE(("server_manager_switch_3: the client can not be "
                        "switched; keeped in this thread\n"));
            syslog(LOG_WARNING, LIXA_SYSLOG_LXD013W);
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case PAYLOAD_CHAIN_RELEASE:
                case MANAGER_FREE_SLOTS:
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_switch_3/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_manager_shutdown(struct thread_status_s *ts,
                            const struct srv_msg_s *msg)
{
    enum Exception
    {
        INVALID_TYPE, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_shutdown\n"));
    TRY {
        switch (msg->body.sd.type) {
            case SHUTDOWN_QUIESCE:
            case SHUTDOWN_IMMEDIATE:
            case SHUTDOWN_FORCE:
                ts->shutdown_type = msg->body.sd.type;
                break;
            default: THROW(INVALID_TYPE);
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case INVALID_TYPE:
                    ret_cod = LIXA_RC_INVALID_OPTION;
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_shutdown/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_manager_pollout(struct thread_status_s *ts, size_t slot_id)
{
    enum Exception
    {
        SEND_ERROR, STORE_VERB_STEP_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_pollout\n"));
    TRY {
        uint32_t block_id;
        ssize_t wrote_bytes;

        if (ts->client_array[slot_id].output_buffer_size >
            (wrote_bytes = send(ts->poll_array[slot_id].fd,
                                ts->client_array[slot_id].output_buffer,
                                ts->client_array[slot_id].output_buffer_size,
                                0))) THROW(SEND_ERROR);
        LIXA_TRACE(("server_manager_pollout: sent "
                    SSIZE_T_FORMAT
                    " bytes "
                    "to client\n", wrote_bytes));
        free(ts->client_array[slot_id].output_buffer);
        ts->client_array[slot_id].output_buffer = NULL;
        ts->client_array[slot_id].output_buffer_size = 0;

#ifdef _CRASH
        LIXA_TRACE(("server_manager_pollout: verb=%d\n",
                    ts->client_array[slot_id].last_verb_step.verb));
        switch (ts->client_array[slot_id].last_verb_step.verb) {
            case LIXA_MSG_VERB_OPEN:
                LIXA_CRASH(LIXA_CRASH_POINT_SERVER_POLLOUT_OPEN,
                           thread_status_get_crash_count(ts));
                break;
            case LIXA_MSG_VERB_START:
                LIXA_CRASH(LIXA_CRASH_POINT_SERVER_POLLOUT_START,
                           thread_status_get_crash_count(ts));
                break;
            case LIXA_MSG_VERB_END:
                LIXA_CRASH(LIXA_CRASH_POINT_SERVER_POLLOUT_END,
                           thread_status_get_crash_count(ts));
                break;
            case LIXA_MSG_VERB_PREPARE:
                LIXA_CRASH(LIXA_CRASH_POINT_SERVER_POLLOUT_PREPARE,
                           thread_status_get_crash_count(ts));
                break;
            case LIXA_MSG_VERB_QRCVR:
                LIXA_CRASH(LIXA_CRASH_POINT_SERVER_POLLOUT_RECOVERY,
                           thread_status_get_crash_count(ts));
                break;
        }
#endif /* _CRASH */

        /* retrieve the block is storing the status of the client inside
           memory mapped status file */
        block_id = ts->client_array[slot_id].pers_status_slot_id;
        LIXA_TRACE(("server_manager_pollout: I/O slot_id = "
                    UINT32_T_FORMAT
                    ", status block_id = "
                    UINT32_T_FORMAT
                    "\n", slot_id, block_id));
        if (LIXA_RC_OK != (ret_cod = payload_header_store_verb_step(
                               ts, block_id,
                               &ts->client_array[slot_id].last_verb_step))) THROW(
                                   STORE_VERB_STEP_ERROR);

        /* now useless, reset for first cycle */
        ts->client_array[slot_id].last_verb_step.verb = 0;
        ts->client_array[slot_id].last_verb_step.step = 0;

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        INTERNAL_ERROR, REALLOC_ERROR1, REALLOC_ERROR2, NONE
    } excp;
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
                        SIZE_T_FORMAT
                        ", ts->active_clients = "
                        SIZE_T_FORMAT
                        "\n", new_poll_size,
                        ts->active_clients));
            THROW(INTERNAL_ERROR);
        }

        /* can we free the slot? */
        if (new_poll_size != ts->poll_size) {
            LIXA_TRACE(("server_manager_free_slots: old poll_size = "
                        SIZE_T_FORMAT
                        ", new poll_size = "
                        SIZE_T_FORMAT
                        "\n", ts->poll_size, new_poll_size));
            if (NULL == (ts->poll_array = realloc(
                             ts->poll_array,
                             new_poll_size * sizeof(struct pollfd)))) THROW(REALLOC_ERROR1);
            if (NULL == (ts->client_array = realloc(
                             ts->client_array,
                             new_poll_size * sizeof(
                                 struct server_client_status_s)))) THROW(REALLOC_ERROR2);
            ts->poll_size = new_poll_size;
        }
        THROW(NONE);
    }
    CATCH
        {
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



int server_manager_msg_proc(struct thread_status_s *ts,
                            size_t slot_id, char *buf, ssize_t read_bytes)
{
    enum Exception { THREAD_SWITCH,
                     MAINTENANCE_MODE,
                     OUTMSG_PREP_ERROR,
                     INMSG_PROC_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int rc;

    struct lixa_msg_s lmo;

    LIXA_TRACE(("server_manager_msg_proc\n"));
    TRY {
        lixa_msg_init(&lmo);

        /* process the input message */
        rc = server_manager_inmsg_proc(ts, slot_id, buf, read_bytes, &lmo);
        if (LIXA_RC_THREAD_SWITCH == rc)
            THROW(THREAD_SWITCH);

        if (LIXA_RC_OK != (ret_cod = server_manager_outmsg_prep(
                               ts, slot_id, &lmo, rc))) {
            LIXA_TRACE(("server_manager_msg_proc: server_manager_outmsg_prep "
                        "return code is %d\n", ret_cod));
            THROW(OUTMSG_PREP_ERROR);
        }

        if (LIXA_RC_MAINTENANCE_MODE == rc)
            THROW(MAINTENANCE_MODE);

        if (LIXA_RC_OK != rc && LIXA_RC_RECOVERY_PENDING_TX != rc) {
            LIXA_TRACE(("server_manager_msg_proc: server_manager_inmsg_prep "
                        "return code is %d\n", rc));
            THROW(INMSG_PROC_ERROR);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case THREAD_SWITCH:
                ret_cod = LIXA_RC_THREAD_SWITCH;
                break;
            case MAINTENANCE_MODE:
                ret_cod = rc;
                break;
            case OUTMSG_PREP_ERROR:
            case INMSG_PROC_ERROR:
                ret_cod = rc;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release message memory */
        lixa_msg_free(&lmo);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_msg_proc/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_inmsg_proc(struct thread_status_s *ts,
                              size_t slot_id, char *buf, ssize_t read_bytes,
                              struct lixa_msg_s *lmo)
{
    enum Exception { LIXA_MSG_DESERIALIZE_ERROR,
                     LIXA_MSG_TRACE_ERROR,
                     SERVER_MANAGER_RECOVERY_ERROR,
                     SERVER_XA_OPEN_ERROR,
                     SERVER_XA_CLOSE_ERROR,
                     SERVER_XA_START_ERROR,
                     SERVER_XA_END_ERROR,
                     SERVER_XA_PREPARE_ERROR,
                     SERVER_XA_COMMIT_ERROR,
                     SERVER_XA_ROLLBACK_ERROR,
                     SERVER_QRCVR_ERROR,
                     SERVER_AX_REG_ERROR,
                     SERVER_AX_UNREG_ERROR,
                     SERVER_XA_FORGET_ERROR,
                     SERVER_TRANS_ERROR,
                     INVALID_VERB,
                     STORE_VERB_STEP_ERROR,
                     NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct lixa_msg_s lmi;
    int recovery_pending = FALSE;

    LIXA_TRACE(("server_manager_inmsg_proc\n"));
    TRY {
        uint32_t block_id;

        lixa_msg_init(&lmi);

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
        /* is this the first message from the client? check recovery pending
           transactions... */
        if (ts->client_array[slot_id].first_message &&
            LIXA_MSG_VERB_OPEN == lmi.header.pvs.verb) {
            LIXA_TRACE(("server_manager_inmsg_proc: processing the first "
                        "message from this client...\n"));
            /* verify if there are recovery pending transaction for the
               job/thread */
            if (LIXA_RC_OK != (ret_cod = server_manager_check_recovery(
                                   ts, &lmi, &recovery_pending)))
                THROW(SERVER_MANAGER_RECOVERY_ERROR);
            /* reset the flag: this check should happen no more for this
               client */
            ts->client_array[slot_id].first_message = FALSE;
        }

        /* retrieve the block is storing the status of the client inside
           memory mapped status file */
        block_id = ts->client_array[slot_id].pers_status_slot_id;
        LIXA_TRACE(("server_manager_inmsg_proc: I/O slot_id="
                    UINT32_T_FORMAT
                    ", status block_id="
                    UINT32_T_FORMAT
                    "\n", slot_id, block_id));

        /* set output message */
        lmo->header.level = LIXA_MSG_LEVEL;
        lmo->header.pvs.verb = LIXA_MSG_VERB_NULL;
        lmo->header.pvs.step = lmi.header.pvs.step + LIXA_MSG_STEP_INCR;

        /* process the message */
        switch (lmi.header.pvs.verb) {
            case LIXA_MSG_VERB_OPEN:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_open(
                            ts, &lmi, lmo, block_id,
                            &(ts->client_array[slot_id].last_verb_step))))
                    THROW(SERVER_XA_OPEN_ERROR);
                break;
            case LIXA_MSG_VERB_CLOSE:
                if (LIXA_RC_OK != (ret_cod = server_xa_close(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_XA_CLOSE_ERROR);
                break;
            case LIXA_MSG_VERB_START:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_start(
                            ts, &lmi, lmo, block_id,
                            &(ts->client_array[slot_id].last_verb_step))))
                    THROW(SERVER_XA_START_ERROR);
                break;
            case LIXA_MSG_VERB_END:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_end(
                            ts, &lmi, lmo, block_id,
                            &(ts->client_array[slot_id].last_verb_step))))
                    THROW(SERVER_XA_END_ERROR);
                break;
            case LIXA_MSG_VERB_PREPARE:
                if (LIXA_RC_OK != (
                        ret_cod = server_xa_prepare(
                            ts, &lmi, lmo, block_id,
                            &(ts->client_array[slot_id].last_verb_step))))
                    THROW(SERVER_XA_PREPARE_ERROR);
                break;
            case LIXA_MSG_VERB_COMMIT:
                if (LIXA_RC_OK != (ret_cod = server_xa_commit(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_XA_COMMIT_ERROR);
                break;
            case LIXA_MSG_VERB_ROLLBACK:
                if (LIXA_RC_OK != (ret_cod = server_xa_rollback(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_XA_ROLLBACK_ERROR);
                break;
            case LIXA_MSG_VERB_QRCVR:
                if (LIXA_RC_OK != (
                        ret_cod = server_recovery(
                            ts, slot_id, &lmi, lmo, block_id,
                            &(ts->client_array[slot_id].last_verb_step))))
                    THROW(SERVER_QRCVR_ERROR);
                break;
            case LIXA_MSG_VERB_REG:
                if (LIXA_RC_OK != (ret_cod = server_ax_reg(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_AX_REG_ERROR);
                break;
            case LIXA_MSG_VERB_UNREG:
                if (LIXA_RC_OK != (ret_cod = server_ax_unreg(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_AX_UNREG_ERROR);
                break;
            case LIXA_MSG_VERB_FORGET:
                if (LIXA_RC_OK != (ret_cod = server_xa_forget(
                                       ts, &lmi, block_id)))
                    THROW(SERVER_XA_FORGET_ERROR);
                break;
            case LIXA_MSG_VERB_TRANS:
                if (LIXA_RC_OK !=
                    (ret_cod =
                     server_trans(ts, slot_id, &lmi, lmo, block_id,
                                  &(ts->client_array[slot_id
                                                     ].last_verb_step))))
                    THROW(SERVER_TRANS_ERROR);
                break;
            default:
                THROW(INVALID_VERB);
        }

        /* register protocol step */
        if (LIXA_RC_OK != (ret_cod = payload_header_store_verb_step(
                               ts, block_id, &lmi.header.pvs)))
            THROW(STORE_VERB_STEP_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case LIXA_MSG_DESERIALIZE_ERROR:
            case LIXA_MSG_TRACE_ERROR:
            case SERVER_MANAGER_RECOVERY_ERROR:
            case SERVER_XA_OPEN_ERROR:
            case SERVER_XA_CLOSE_ERROR:
            case SERVER_XA_START_ERROR:
            case SERVER_XA_END_ERROR:
            case SERVER_XA_PREPARE_ERROR:
            case SERVER_XA_COMMIT_ERROR:
            case SERVER_XA_ROLLBACK_ERROR:
            case SERVER_QRCVR_ERROR:
            case SERVER_AX_REG_ERROR:
            case SERVER_AX_UNREG_ERROR:
            case SERVER_XA_FORGET_ERROR:
            case SERVER_TRANS_ERROR:
                break;
            case INVALID_VERB:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case STORE_VERB_STEP_ERROR:
                break;
            case NONE:
                ret_cod = recovery_pending ?
                    LIXA_RC_RECOVERY_PENDING_TX : LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release dynamically allocated strings */
        lixa_msg_free(&lmi);
    } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_inmsg_proc/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_manager_outmsg_prep(struct thread_status_s *ts, size_t slot_id,
                               struct lixa_msg_s *lmo, int rc)
{
    enum Exception
    {
        NOTHING_TO_DO,
        MALLOC_ERROR,
        REPLY_OPEN_ERROR,
        REPLY_START_ERROR,
        REPLY_END_ERROR,
        REPLY_PREPARE_ERROR,
        REPLY_QRCVR_ERROR,
        REPLY_SCAN_ERROR,
        VERB_NOT_FOUND,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_outmsg_prep\n"));
    TRY {
        if (lmo->header.pvs.verb == LIXA_MSG_VERB_NULL) THROW(NOTHING_TO_DO);

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
                                       ts, slot_id, lmo, rc))) THROW(REPLY_OPEN_ERROR);
                break;
            case LIXA_MSG_VERB_START:
                if (LIXA_RC_OK != (ret_cod = server_reply_start(
                                       ts, slot_id, lmo, rc))) THROW(REPLY_START_ERROR);
                break;
            case LIXA_MSG_VERB_END:
                if (LIXA_RC_OK != (ret_cod = server_reply_end(
                                       ts, slot_id, lmo, rc))) THROW(REPLY_END_ERROR);
                break;
            case LIXA_MSG_VERB_PREPARE:
                if (LIXA_RC_OK != (ret_cod = server_reply_prepare(
                                       ts, slot_id, lmo, rc))) THROW(REPLY_PREPARE_ERROR);
                break;
            case LIXA_MSG_VERB_QRCVR:
                if (LIXA_RC_OK != (ret_cod = server_reply_qrcvr(
                                       ts, slot_id, lmo))) THROW(REPLY_QRCVR_ERROR);
                break;
            case LIXA_MSG_VERB_TRANS:
                if (LIXA_RC_OK !=
                    (ret_cod = server_reply_trans(ts, slot_id, lmo))) THROW(
                        REPLY_SCAN_ERROR)
                                                                          break;
            default: THROW(VERB_NOT_FOUND);
        } /* switch (lmo.header.pvs.verb) */

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case NOTHING_TO_DO:
                    ret_cod = LIXA_RC_OK;
                    break;
                case MALLOC_ERROR:
                    ret_cod = LIXA_RC_MALLOC_ERROR;
                    break;
                case REPLY_OPEN_ERROR:
                case REPLY_START_ERROR:
                case REPLY_END_ERROR:
                case REPLY_PREPARE_ERROR:
                case REPLY_QRCVR_ERROR:
                case REPLY_SCAN_ERROR:
                    break;
                case VERB_NOT_FOUND:
                    ret_cod = LIXA_RC_INVALID_STATUS;
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


int server_manager_check_recovery(struct thread_status_s *ts,
                                  const struct lixa_msg_s *lmi,
                                  int *recovery_pending)
{
    enum Exception
    {
        PROTOCOL_ERROR, JOB_SET_RAW_ERROR, GET_BLOCK_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_check_recovery\n"));
    TRY {
        struct srvr_rcvr_tbl_rec_s query, result;
        lixa_job_t query_job, result_job;

        /* optimistic guess */
        *recovery_pending = FALSE;

        if (LIXA_MSG_VERB_OPEN != lmi->header.pvs.verb) THROW(PROTOCOL_ERROR);

        /* prepare the query object */
        if (LIXA_RC_OK != (ret_cod = lixa_job_set_raw(
                               &query_job,
                               (const char *) lmi->body.open_8.client.job))) THROW(
                                   JOB_SET_RAW_ERROR);
        query.job = &query_job;
        query.tsid = ts->id;
        result.job = &result_job; /* reserve room for job object */

        /* query the recovery table */
        ret_cod = srvr_rcvr_tbl_get_block(ts->recovery_table, &query, &result,
                                          TRUE);
        switch (ret_cod) {
            case LIXA_RC_OK:
            case LIXA_RC_BYPASSED_OPERATION:
                *recovery_pending = TRUE;
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* nothing to do */
                break;
            default: THROW(GET_BLOCK_ERROR);
        } /* switch (rc) */

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case PROTOCOL_ERROR:
                    ret_cod = LIXA_RC_PROTOCOL_ERROR;
                    break;
                case JOB_SET_RAW_ERROR:
                    break;
                case GET_BLOCK_ERROR:
                    break;
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_manager_check_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_manager_add_poll(struct thread_status_s *ts,
                            int new_fd, nfds_t *place)
{
    enum Exception
    {
        REALLOC_ERROR1, REALLOC_ERROR2, NONE
    } excp;
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
                             ++ts->poll_size * sizeof(struct pollfd)))) THROW(
                                 REALLOC_ERROR1);
            if (NULL == (ts->client_array = realloc(
                             ts->client_array,
                             ts->poll_size * sizeof(
                                 struct server_client_status_s)))) THROW(REALLOC_ERROR2);
            *place = ts->poll_size - 1;
        } /* if (*place > 0) */
        ts->poll_array[*place].fd = new_fd;
        ts->poll_array[*place].events = POLLIN;
        if (!first_add)
            ts->active_clients++;

        LIXA_TRACE(("server_manager_add_poll: added file descriptor %d "
                    "at position "
                    NFDS_T_FORMAT
                    "\n",
                    ts->poll_array[*place].fd, *place));

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_fix_poll\n"));
    TRY {
        nfds_t i;

        for (i = 1; i < ts->poll_size; ++i) {
            /* skip records are not used */
            if (ts->poll_array[i].fd == LIXA_NULL_FD) {
                ts->poll_array[i].events = 0;
                continue;
            }
            if (ts->client_array[i].control_only)
                ts->poll_array[i].events = 0;
            else if (ts->client_array[i].output_buffer == NULL)
                ts->poll_array[i].events = POLLIN;
            else
                ts->poll_array[i].events = POLLOUT;
        }

        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        RECORD_INSERT_ERROR, PAYLOAD_HEADER_INIT, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_manager_new_client\n"));
    TRY {
        uint32_t slot = 0;

        /* get a free block from status file and insert in used list */
        if (LIXA_RC_OK != (ret_cod = status_record_insert(ts, &slot))) THROW(
            RECORD_INSERT_ERROR);

        /* create the header and reset it */
        if (LIXA_RC_OK != (ret_cod = payload_header_init(
                               &ts->curr_status[slot].sr.data, fd))) THROW(PAYLOAD_HEADER_INIT);

        /* save a reference to the slot */
        ts->client_array[place].pers_status_slot_id = slot;
        /* reset the output buffer pointer (it may be garbage if unused
           before */
        server_client_status_init(&(ts->client_array[place]));

        THROW(NONE);
    }
    CATCH
        {
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

