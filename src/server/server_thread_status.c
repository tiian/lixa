/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include "lixa_crash.h"
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_utils.h"
#include "lixa_xid.h"
#include "lixa_syslog.h"
#include "server_thread_status.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



void thread_status_init(struct thread_status_s *ts, int id,
                        struct thread_pipe_array_s *tpa, int mmode,
                        long *crash_count)
{
    LIXA_TRACE(("thread_status_init: initializing thread status (id = %d)\n",
                id));
    ts->id = id;
    ts->tpa = tpa;
    ts->poll_size = 0;
    ts->poll_array = NULL;
    ts->active_clients = 0;
    ts->client_array = NULL;
    ts->min_elapsed_sync_time = ts->max_elapsed_sync_time = 0;
    status_sync_init(&ts->status_sync);
    ts->status1_filename = ts->status2_filename = NULL;
    ts->status1 = ts->status2 = NULL;
    ts->curr_status = NULL;
    if (id)
        ts->updated_records = g_tree_new(size_t_compare_func);
    else /* listener does not need this structure */
        ts->updated_records = NULL;
    ts->recovery_table = NULL;
    ts->trans_table = NULL;
    ts->mmode = mmode;
    ts->excp = ts->ret_cod = ts->last_errno = 0;
    if (id == 0)
        ts->tid = pthread_self();
    else
        ts->tid = 0;
    ts->shutdown_type = SHUTDOWN_NULL;
#ifdef _CRASH
    ts->crash_count = crash_count;
#endif
    LIXA_TRACE(("thread_status_init: end initialization (id = %d)\n", id));
}



void thread_status_destroy(struct thread_status_s *ts)
{
    LIXA_TRACE(("thread_status_destroy\n"));

    if (NULL != ts->poll_array) {
        free(ts->poll_array);
        ts->poll_array = NULL;
        ts->poll_size = 0;
    }
    if (NULL != ts->client_array) {
        free(ts->client_array);
        ts->client_array = NULL;
    }
    if (NULL != ts->updated_records) {
        g_tree_destroy(ts->updated_records);
        ts->updated_records = NULL;
    }
    if (NULL != ts->status1_filename) {
        g_free(ts->status1_filename);
        ts->status1_filename = NULL;
    }
    if (NULL != ts->status2_filename) {
        g_free(ts->status2_filename);
        ts->status2_filename = NULL;
    }
}



int thread_status_dump(const struct thread_status_s *ts,
                       const struct ts_dump_spec_s *tsds)
{
    enum Exception { ISO_TIMESTAMP_ERROR
                     , DUMP_HEADER1
                     , DUMP_RSRMGR1
                     , DUMP_HEADER2
                     , DUMP_RSRMGR2
                     , DUMP_HEADER3
                     , DUMP_RSRMGR3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_dump\n"));
    TRY {
        struct status_record_ctrl_s *first_record =
            &(ts->curr_status->sr.ctrl);
        char string_date_time[ISO_TIMESTAMP_BUFFER_SIZE];
        uint32_t i;

        printf("===================================="
               "====================================\n");
        if (ts->curr_status == ts->status1)
            printf("First file ('%s') will be dumped\n",
                   ts->status1_filename);
        else 
            printf("Second file ('%s') will be dumped\n",
                   ts->status2_filename);
        /* dump first record content */
        printf("Magic number is: " UINT32_T_FORMAT " (" UINT32_T_FORMAT
               ")\n", first_record->magic_number, STATUS_FILE_MAGIC_NUMBER);
        printf("Level is: " UINT32_T_FORMAT " (" UINT32_T_FORMAT ")\n",
               first_record->level, STATUS_FILE_LEVEL);
        /* if for some reason there are porting issues, related to localtime_r,
           localtime can be used instead (dumps are produced by only one
           thread */
        if (LIXA_RC_OK != (ret_cod = lixa_utils_iso_timestamp(
                               &(first_record->last_sync), string_date_time,
                               sizeof(string_date_time))))
            THROW(ISO_TIMESTAMP_ERROR);
        printf("Last sync timestamp: %s\n", string_date_time);
        printf("Size: " UINT32_T_FORMAT " blocks\n",
               first_record->number_of_blocks);
        printf("Used block chain starts at: " UINT32_T_FORMAT " %s\n",
               first_record->first_used_block,
               first_record->first_used_block != 0 ? "" : "(empty chain)");
        printf("Free block chain starts at: " UINT32_T_FORMAT " %s\n",
               first_record->first_free_block,
               first_record->first_free_block != 0 ? "" : "(empty chain)");

        printf("Dumping records following physical order: %d\n", tsds->seq);
        printf("Dumping records following free block chain: %d\n", tsds->free);
        printf("Dumping records following used block chain: %d\n", tsds->used);

        if (tsds->seq) {
            for (i=1; i<first_record->number_of_blocks; ++i) {
                struct status_record_data_s *record =
                    &(ts->curr_status[i].sr.data);
                printf("------------------------------------"
                       "------------------------------------\n");
                printf("Block: " UINT32_T_FORMAT ", next block in chain: "
                       UINT32_T_FORMAT "\n", i, record->next_block);
                printf("Block type: ");
                switch (record->pld.type) {
                    case DATA_PAYLOAD_TYPE_HEADER:
                        printf("transaction manager record "
                               "(transaction header)\n");
                        if (LIXA_RC_OK != (ret_cod = thread_status_dump_header(
                                               &(record->pld.ph))))
                            THROW(DUMP_HEADER1);
                        break;
                    case DATA_PAYLOAD_TYPE_RSRMGR:
                        printf("resource manager record\n");
                        if (LIXA_RC_OK != (ret_cod = thread_status_dump_rsrmgr(
                                               &(record->pld.rm))))
                            THROW(DUMP_RSRMGR1);
                        break;
                    default:
                        printf("unknown (%d)\n", record->pld.type);
                }
            } /* for (i=1; ... */
        } /* if (tsds->seq) */

        if (tsds->free) {
            i = ts->curr_status[0].sr.ctrl.first_free_block;
            while (0 != i) {
                struct status_record_data_s *record =
                    &(ts->curr_status[i].sr.data);
                printf("------------------------------------"
                       "------------------------------------\n");
                printf("Block: " UINT32_T_FORMAT ", next block in chain: "
                       UINT32_T_FORMAT "\n", i, record->next_block);
                printf("Block type: ");
                switch (record->pld.type) {
                    case DATA_PAYLOAD_TYPE_HEADER:
                        printf("transaction manager record "
                               "(transaction header)\n");
                        if (LIXA_RC_OK != (ret_cod = thread_status_dump_header(
                                               &(record->pld.ph))))
                            THROW(DUMP_HEADER2);
                        break;
                    case DATA_PAYLOAD_TYPE_RSRMGR:
                        printf("resource manager record\n");
                        if (LIXA_RC_OK != (ret_cod = thread_status_dump_rsrmgr(
                                               &(record->pld.rm))))
                            THROW(DUMP_RSRMGR2);
                        break;
                    default:
                        printf("unknown (%d)\n", record->pld.type);
                }
                i = ts->curr_status[i].sr.data.next_block;
            }
        }
        
        if (tsds->used) {
            i = ts->curr_status[0].sr.ctrl.first_used_block;
            while (0 != i) {
                struct status_record_data_s *record =
                    &(ts->curr_status[i].sr.data);
                printf("------------------------------------"
                       "------------------------------------\n");
                printf("Block: " UINT32_T_FORMAT ", next block in chain: "
                       UINT32_T_FORMAT "\n", i, record->next_block);
                printf("Block type: ");
                switch (record->pld.type) {
                    case DATA_PAYLOAD_TYPE_HEADER:
                        printf("transaction manager record "
                               "(transaction header)\n");
                        if (LIXA_RC_OK != (ret_cod = thread_status_dump_header(
                                               &(record->pld.ph))))
                            THROW(DUMP_HEADER3);
                        break;
                    case DATA_PAYLOAD_TYPE_RSRMGR:
                        printf("resource manager record\n");
                        if (LIXA_RC_OK != (ret_cod = thread_status_dump_rsrmgr(
                                               &(record->pld.rm))))
                            THROW(DUMP_RSRMGR3);
                        break;
                    default:
                        printf("unknown (%d)\n", record->pld.type);
                }
                i = ts->curr_status[i].sr.data.next_block;
            } 
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ISO_TIMESTAMP_ERROR:
                break;
            case DUMP_HEADER1:
            case DUMP_RSRMGR1:
            case DUMP_HEADER2:
            case DUMP_RSRMGR2:
            case DUMP_HEADER3:
            case DUMP_RSRMGR3:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_dump/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int thread_status_dump_header(const struct payload_header_s *ph)
{
    enum Exception { ISO_TIMESTAMP1
                     , ISO_TIMESTAMP2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_dump_header\n"));
    TRY {
        int i;
        char tmp_str_time[ISO_TIMESTAMP_BUFFER_SIZE];
        lixa_ser_xid_t xid_str = "";
        
        printf("\tTrnhdr/number of resource managers: %d\n", ph->n);
        if (ph->n > 0) {
            printf("\tTrnhdr/resource manager blocks are: ");
            for (i=0; i<ph->n; ++i)
                printf(UINT32_T_FORMAT " ", ph->block_array[i]);
            printf("\n");
        }
        printf("\tTrnhdr/previous and next branch blocks: " UINT32_T_FORMAT
               " " UINT32_T_FORMAT "\n", ph->prev_branch_block,
               ph->next_branch_block);
        if (LIXA_RC_OK != (ret_cod = lixa_utils_iso_timestamp(
                               &ph->arrival_time, tmp_str_time,
                               sizeof(tmp_str_time))))
            THROW(ISO_TIMESTAMP1);
        printf("\tTrnhdr/arrival time: %s\n", tmp_str_time);
        printf("\tTrnhdr/local socket address:port is %s:%hu\n",
               inet_ntoa(ph->local_sock_addr.sin_addr),
               ntohs(ph->local_sock_addr.sin_port));
        printf("\tTrnhdr/peer socket address:port is %s:%hu\n",
               inet_ntoa(ph->peer_sock_addr.sin_addr),
               ntohs(ph->peer_sock_addr.sin_port));
        printf("\tTrnhdr/config digest is '%s'\n", ph->config_digest);
        printf("\tTrnhdr/job is '%s'\n", lixa_job_get_raw(&ph->job));
        printf("\tTrnhdr/last (verb, step) are: [ ");
        for (i=0; i<PAYLOAD_HEADER_VERB_STEP; ++i) {
            printf("(%d,%d) ",
                   ph->last_verb_step[
                       PAYLOAD_HEADER_VERB_STEP-i-1].verb,
                   ph->last_verb_step[
                       PAYLOAD_HEADER_VERB_STEP-i-1].step);
        } /* for (i=0; ... */
        printf("]\n");
        lixa_xid_serialize(&ph->state.xid, xid_str);
        printf("\tTrnhdr/state/finished: %d\n"
               "\tTrnhdr/state/txstate: %d\n"
               "\tTrnhdr/state/will commit: %d\n"
               "\tTrnhdr/state/will rollback: %d\n"
               "\tTrnhdr/state/global recovery: %d\n"
               "\tTrnhdr/state/xid: '%s'\n", 
               ph->state.finished, ph->state.txstate, ph->state.will_commit,
               ph->state.will_rollback, ph->state.global_recovery, xid_str);
        if (LIXA_RC_OK != (ret_cod = lixa_utils_iso_timestamp(
                               &ph->recovery_failed_time, tmp_str_time,
                               sizeof(tmp_str_time))))
            THROW(ISO_TIMESTAMP1);
        printf("\tTrnhdr/recoverying block id: " UINT32_T_FORMAT "\n"
               "\tTrnhdr/recovery failed: %d\n"
               "\tTrnhdr/recovery failed time: %s\n"               
               "\tTrnhdr/recovery commit: %d\n",
               ph->recovering_block_id, ph->recovery_failed,
               tmp_str_time, ph->recovery_commit);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case ISO_TIMESTAMP1:
            case ISO_TIMESTAMP2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_dump_header/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int thread_status_dump_rsrmgr(const struct payload_rsrmgr_s *rm)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_dump_rsrmgr\n"));
    TRY {
        printf("\tRsrmgr/rmid: %d\n", rm->rmid);
        printf("\tRsrmgr/state/next_verb: %d\n"
               "\tRsrmgr/state/xa_r_state: %d\n"
               "\tRsrmgr/state/dynamic: %d\n"
               "\tRsrmgr/state/xa_td_state: %d\n"
               "\tRsrmgr/state/xa_s_state: %d\n",
               rm->state.next_verb, rm->state.xa_r_state, rm->state.dynamic,
               rm->state.xa_td_state, rm->state.xa_s_state);
        printf("\tRsrmgr/lixac_conf.xml name: '%s'\n"
               "\tRsrmgr/xa_name: '%s'\n"
               "\tRsrmgr/xa_open_info: '%s'\n"
               "\tRsrmgr/xa_open_flags: 0x%lx\n"
               "\tRsrmgr/xa_open_rc: %d\n",
               rm->name, rm->xa_name, rm->xa_open_info, rm->xa_open_flags,
               rm->xa_open_rc);
        printf("\tRsrmgr/xa_start_flags: 0x%lx\n"
               "\tRsrmgr/xa_start_rc: %d\n",
               rm->xa_start_flags, rm->xa_start_rc);
        printf("\tRsrmgr/xa_end_flags: 0x%lx\n"
               "\tRsrmgr/xa_end_rc: %d\n",
               rm->xa_end_flags, rm->xa_end_rc);
        printf("\tRsrmgr/xa_prepare_flags: 0x%lx\n"
               "\tRsrmgr/xa_prepare_rc: %d\n",
               rm->xa_prepare_flags, rm->xa_prepare_rc);
        printf("\tRsrmgr/xa_commit_flags: 0x%lx\n"
               "\tRsrmgr/xa_commit_rc: %d\n",
               rm->xa_commit_flags, rm->xa_commit_rc);
        printf("\tRsrmgr/xa_rollback_flags: 0x%lx\n"
               "\tRsrmgr/xa_rollback_rc: %d\n",
               rm->xa_rollback_flags, rm->xa_rollback_rc);
        printf("\tRsrmgr/xa_forget_flags: 0x%lx\n"
               "\tRsrmgr/xa_forget_rc: %d\n",
               rm->xa_forget_flags, rm->xa_forget_rc);
        printf("\tRsrmgr/ax_reg_flags: 0x%lx\n"
               "\tRsrmgr/ax_reg_rc: %d\n",
               rm->ax_reg_flags, rm->ax_reg_rc);
        printf("\tRsrmgr/ax_unreg_flags: 0x%lx\n"
               "\tRsrmgr/ax_unreg_rc: %d\n",
               rm->ax_unreg_flags, rm->ax_unreg_rc);
        printf("\tRsrmgr/recovery_rc: %d\n",
               rm->recovery_rc);
        
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
    LIXA_TRACE(("thread_status_dump_rsrmgr/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int thread_status_load_files(struct thread_status_s *ts,
                             const char *status_file_prefix,
                             const struct ts_dump_spec_s *tsds)
{
    enum Exception { STATUS_RECORD_LOAD_1_ERROR
                     , STATUS_RECORD_LOAD_2_ERROR
                     , DAMAGED_STATUS_FILES
                     , STATUS_RECORD_COPY_ERROR1
                     , STATUS_RECORD_COPY_ERROR2
                     , STATUS_RECORD_COPY_ERROR3
                     , STATUS_RECORD_COPY_ERROR4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_load_files\n"));
    TRY {
        int s1ii = FALSE, s2ii = FALSE;
        
        /* first file */
        ts->status1_filename = g_strconcat(status_file_prefix,
                                           STATUS_FILE_SUFFIX_1, NULL);
        LIXA_TRACE(("thread_status_load_files: first status file is '%s'\n",
                    ts->status1_filename));
        if (LIXA_RC_OK != (ret_cod = status_record_load(
                               &(ts->status1),
                               (const char *)ts->status1_filename,
                               &(ts->updated_records), tsds->dump)))
            THROW(STATUS_RECORD_LOAD_1_ERROR);
        if (LIXA_RC_OK != (ret_cod = status_record_check_integrity(
                               ts->status1))) {
            syslog(LOG_WARNING, LIXA_SYSLOG_LXD007W, ts->status1_filename);
        } else
            s1ii = TRUE;
        
        /* second file */
        ts->status2_filename = g_strconcat(status_file_prefix,
                                           STATUS_FILE_SUFFIX_2, NULL);
        LIXA_TRACE(("thread_status_load_files: second status file is '%s'\n",
                    ts->status2_filename));
        if (LIXA_RC_OK != (ret_cod = status_record_load(
                               &(ts->status2),
                               (const char *)ts->status2_filename,
                               &(ts->updated_records), tsds->dump)))
            THROW(STATUS_RECORD_LOAD_2_ERROR);
        if (LIXA_RC_OK != (ret_cod = status_record_check_integrity(
                               ts->status2))) {
            syslog(LOG_WARNING, LIXA_SYSLOG_LXD008W, ts->status2_filename);
        } else
            s2ii = TRUE;

        if (!s1ii && !s2ii) {
            /* two damaged files! */
            syslog(LOG_ERR, LIXA_SYSLOG_LXD009E);
            THROW(DAMAGED_STATUS_FILES);
        } else if (s1ii && s2ii) {
            /* two integral files, check timestamp */
            if ((ts->status1->sr.ctrl.last_sync.tv_sec ==
                 ts->status2->sr.ctrl.last_sync.tv_sec) &&
                (ts->status1->sr.ctrl.last_sync.tv_usec ==
                 ts->status2->sr.ctrl.last_sync.tv_usec)) {
                LIXA_TRACE(("thread_status_load_files: first and second "
                            "status file were synchronized at the same "
                            "time\n"));
                ts->curr_status = ts->status1;
            } else if ((ts->status1->sr.ctrl.last_sync.tv_sec <
                        ts->status2->sr.ctrl.last_sync.tv_sec) ||
                       ((ts->status1->sr.ctrl.last_sync.tv_sec ==
                         ts->status2->sr.ctrl.last_sync.tv_sec) &&
                        (ts->status1->sr.ctrl.last_sync.tv_usec <
                         ts->status2->sr.ctrl.last_sync.tv_usec))) {
                /* second file is newer */
                LIXA_TRACE(("thread_status_load_files: second status file is "
                            "the more recent\n"));
                if (tsds->dump)
                    ts->curr_status = ts->status2;
                else {
                    /* copying second file over first one, and point first as
                       the current file */
                    if (LIXA_RC_OK != (ret_cod =  status_record_copy(
                                           ts->status1, ts->status2, ts)))
                        THROW(STATUS_RECORD_COPY_ERROR1);
                    ts->curr_status = ts->status1;
                }
            } else {
                /* first file is newer */
                LIXA_TRACE(("thread_status_load_files: first status file is "
                            "the more recent\n"));
                if (tsds->dump)
                    ts->curr_status = ts->status1;
                else {
                    /* copying first file over second one, and point second as
                       the current file */
                    if (LIXA_RC_OK != (ret_cod = status_record_copy(
                                           ts->status2, ts->status1, ts)))
                        THROW(STATUS_RECORD_COPY_ERROR2);
                    ts->curr_status = ts->status2;
                }
            }
        } else if (s1ii) {
            /* only first file is integral */
            LIXA_TRACE(("thread_status_load_files: first status file is OK, "
                        "second status file is damanged: overriding it...\n"));
            /* copying first file over second one, and point second as
               the current file */
            if (tsds->dump)
                ts->curr_status = ts->status1;
            else {
                if (LIXA_RC_OK != (ret_cod = status_record_copy(
                                       ts->status2, ts->status1, ts)))
                    THROW(STATUS_RECORD_COPY_ERROR3);
                ts->curr_status = ts->status2;
            }
        } else {
            /* only second file is integral */
            LIXA_TRACE(("thread_status_load_files: second status file is OK, "
                        "first status file is damanged: overriding it...\n"));
            if (tsds->dump)
                ts->curr_status = ts->status2;
            else {
                /* copying second file over first one, and point first as
                   the current file */
                if (LIXA_RC_OK != (ret_cod = status_record_copy(
                                       ts->status1, ts->status2, ts)))
                    THROW(STATUS_RECORD_COPY_ERROR4);
                ts->curr_status = ts->status1;
            }
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case STATUS_RECORD_LOAD_1_ERROR:
            case STATUS_RECORD_LOAD_2_ERROR:
                break;
            case DAMAGED_STATUS_FILES:
                ret_cod = LIXA_RC_CORRUPTED_STATUS_FILE;
                break;
            case STATUS_RECORD_COPY_ERROR1:
            case STATUS_RECORD_COPY_ERROR2:
            case STATUS_RECORD_COPY_ERROR3:
            case STATUS_RECORD_COPY_ERROR4:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_load_files/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int thread_status_recovery(struct thread_status_s *ts,
                           srvr_rcvr_tbl_t *srt)
{
    enum Exception { NULL_SERVER_RECOVERY_TABLE
                     , CHECK_RECOVERY_PENDING_ERROR
                     , RECOVERY_TABLE_INSERT_ERROR
                     , INVALID_STATUS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_recovery\n"));
    TRY {
        uint32_t i;
        struct status_record_s *first_block = ts->curr_status;
        
        if (NULL == srt)
            THROW(NULL_SERVER_RECOVERY_TABLE);
        ts->recovery_table = srt;
        
        /* traverse used block list */
        i = first_block->sr.ctrl.first_used_block;
        while (i) {
            struct status_record_data_s *data = &ts->curr_status[i].sr.data;
            if (DATA_PAYLOAD_TYPE_RSRMGR == data->pld.type) {
                LIXA_TRACE(("thread_status_recovery: block # " UINT32_T_FORMAT
                            " is a transaction resource manager block, "
                            "skipping...\n", i));
            } else if (DATA_PAYLOAD_TYPE_HEADER == data->pld.type) {
                int branch_recovery_pending = FALSE;
                int global_recovery_pending = FALSE;
                LIXA_TRACE(("thread_status_recovery: block # " UINT32_T_FORMAT
                            " is a transaction header block\n", i));
                if (LIXA_RC_OK != (
                        ret_cod = thread_status_check_recovery_pending(
                            ts, data, &branch_recovery_pending,
                            &global_recovery_pending)))
                    THROW(CHECK_RECOVERY_PENDING_ERROR);
                if (branch_recovery_pending) {
                    struct srvr_rcvr_tbl_rec_s srtr;
                    LIXA_TRACE(("thread_status_recovery: block # "
                                UINT32_T_FORMAT " is related to a recovery "
                                "pending transaction\n", i));
                    srtr.job = &data->pld.ph.job;
                    srtr.tsid = ts->id;
                    srtr.block_id = i;
                    if (LIXA_RC_OK != (ret_cod = srvr_rcvr_tbl_insert(
                                           ts->recovery_table, &srtr)))
                        THROW(RECOVERY_TABLE_INSERT_ERROR);
                }
            } else {
                LIXA_TRACE(("thread_status_recovery: block # " UINT32_T_FORMAT
                            " is an unknown block (%d)\n", i, data->pld.type));
                THROW(INVALID_STATUS);
            }
            i = data->next_block;
        } /* for (i...) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_SERVER_RECOVERY_TABLE:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case CHECK_RECOVERY_PENDING_ERROR:
            case RECOVERY_TABLE_INSERT_ERROR:
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int thread_status_clean_failed(struct thread_status_s *ts)
{
    enum Exception { PAYLOAD_CHAIN_RELEASE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_clean_failed\n"));
    TRY {
        uint32_t i;
        struct status_record_ctrl_s *first_record =
            &(ts->curr_status->sr.ctrl);

        for (i=1; i<first_record->number_of_blocks; ++i) {
            struct status_record_data_s *record =
                &(ts->curr_status[i].sr.data);
            if (DATA_PAYLOAD_TYPE_HEADER == record->pld.type &&
                record->pld.ph.recovery_failed) {
                lixa_ser_xid_t ser_xid = "";
                lixa_xid_serialize(&record->pld.ph.state.xid, ser_xid);
                LIXA_TRACE(("thread_status_clean_failed: block # "
                            UINT32_T_FORMAT " contains a recovery failed "
                            "transaction ('%s'), cleaning it\n", i,
                            NULL != ser_xid ? ser_xid : ""));
                ret_cod = payload_chain_release(ts, i);
                switch (ret_cod) {
                    case LIXA_RC_OK:
                        syslog(LOG_INFO, LIXA_SYSLOG_LXD021I, ser_xid);
                        break;
                    case LIXA_RC_OBJ_NOT_FOUND:
                        LIXA_TRACE(("thread_status_clean_failed: block # "
                                    UINT32_T_FORMAT " is not in used "
                                    "chain\n", i));
                        break;
                    default:
                        THROW(PAYLOAD_CHAIN_RELEASE_ERROR);
                        break;
                }
            }
        } /* for (i=1; ... */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PAYLOAD_CHAIN_RELEASE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_clean_failed/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int thread_status_check_recovery_pending(
    const struct thread_status_s *ts,
    const struct status_record_data_s *data, int *branch, int *global)
{
    enum Exception { INVALID_HEADER_TYPE
                     , FINISHED_TRANSACTION
                     , RECOVERY_FAILED_TRANSACTION
                     , NOT_STARTED_TRANSACTION1
                     , NOT_STARTED_TRANSACTION2
                     , ALL_PREPARED
                     , NOT_STARTED_TRANSACTION3
                     , INVALID_VERB
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    *branch = TRUE; /* conservative behavior */
    *global = FALSE;
    
    LIXA_TRACE(("thread_status_check_recovery_pending\n"));
    TRY {
        const struct lixa_msg_verb_step_s *last = data->pld.ph.last_verb_step;
        
        if (DATA_PAYLOAD_TYPE_HEADER != data->pld.type) {
            LIXA_TRACE(("thread_status_check_recovery_pending: "
                        "data->pld.type=%d\n", data->pld.type));
            THROW(INVALID_HEADER_TYPE);
        }

        /* the logic of this function could be improved in the future, but
           at this time the algorithm is very conservative: probably some
           unnecessary recovery operations will be performed */

        /* check if a global recovery pending condition has already been set */
        LIXA_TRACE(("thread_status_check_recovery_pending: "
                    "data->pld.ph.state.global_recovery=%d\n",
                    data->pld.ph.state.global_recovery));
        *global = data->pld.ph.state.global_recovery;
        
        /* is the transaction already marked as finished? */
        if (data->pld.ph.state.finished) {
            LIXA_TRACE(("thread_status_check_recovery_pending: "
                        "data->pld.ph.state.finished=%d, returning FALSE\n",
                        data->pld.ph.state.finished));
            THROW(FINISHED_TRANSACTION);
        }
        /* was the transaction already recovered (with errors?) */
        if (data->pld.ph.recovery_failed) {
            LIXA_TRACE(("thread_status_check_recovery_pending: "
                        "data->pld.ph.recovery_failed=%d, returning FALSE\n",
                        data->pld.ph.recovery_failed));
            THROW(RECOVERY_FAILED_TRANSACTION);
        }
        /* check last verb & step */
        LIXA_TRACE(("thread_status_check_recovery_pending: "
                    "verb=%d, step=%d\n", last->verb, last->step));
        switch (last->verb) {
            case LIXA_MSG_VERB_NULL:
                LIXA_TRACE(("thread_status_check_recovery_pending: verb is "
                            "LIXA_MSG_VERB_NULL, nothing to do\n"));
                break;
            case LIXA_MSG_VERB_OPEN:
            case LIXA_MSG_VERB_CLOSE:
            case LIXA_MSG_VERB_UNREG:
                LIXA_TRACE(("thread_status_check_recovery_pending: "
                            "returning FALSE for last->verb=%d\n",
                            last->verb));
                THROW(NOT_STARTED_TRANSACTION1);
            case LIXA_MSG_VERB_START:
                /* check transaction manager state */
                if (TX_STATE_S3 != data->pld.ph.state.txstate &&
                    TX_STATE_S4 != data->pld.ph.state.txstate) {
                    LIXA_TRACE(("thread_status_check_recovery_pending: "
                                "returning FALSE for last->verb=START and "
                                "txstate=%d\n",
                                data->pld.ph.state.txstate));
                    THROW(NOT_STARTED_TRANSACTION2);
                }
                break;
            case LIXA_MSG_VERB_END:
            case LIXA_MSG_VERB_PREPARE:
            case LIXA_MSG_VERB_COMMIT:
            case LIXA_MSG_VERB_ROLLBACK:
            case LIXA_MSG_VERB_FORGET:
                break;
                /* check if all resource manager switched to "prepared" (S3)
                   state */
                /* 20180115 old stuff, remove me
                if (thread_status_check_all_prepared(ts, data))
                    THROW(ALL_PREPARED);
                break;
                */
            case LIXA_MSG_VERB_REG:
                /* check TX state */
                if (data->pld.ph.state.txstate != TX_STATE_S3 &&
                    data->pld.ph.state.txstate != TX_STATE_S4)
                    THROW(NOT_STARTED_TRANSACTION3);
                break;
            case LIXA_MSG_VERB_QRCVR:
                /* this is a crash after a crash: returning TRUE and keep the
                   status */
                break;
            case LIXA_MSG_VERB_TRANS:
                /* nothing to do, only a query */
                break;
            default:
                THROW(INVALID_VERB);
        }
        /* arrived here = possibly recovery pending... */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_HEADER_TYPE:
            case INVALID_VERB:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case FINISHED_TRANSACTION:
            case RECOVERY_FAILED_TRANSACTION:
            case NOT_STARTED_TRANSACTION1:
            case NOT_STARTED_TRANSACTION2:
            case ALL_PREPARED:
            case NOT_STARTED_TRANSACTION3:
                *branch = FALSE;
                ret_cod = LIXA_RC_OK;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_check_recovery_pending/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}




int thread_status_set_global_recovery(struct thread_status_s *ts,
                                      uint32_t block_id,
                                      int global_recovery)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("thread_status_set_global_recovery\n"));
    TRY {
        struct status_record_data_s *data =
            &(ts->curr_status[block_id].sr.data);
        status_record_update(ts->curr_status + block_id, block_id,
                             ts->updated_records);
        data->pld.ph.state.global_recovery = global_recovery;
        
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
    LIXA_TRACE(("thread_status_set_global_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int thread_status_sync_files(struct thread_status_s *ts)
{
    enum Exception { GETTIMEOFDAY_ERROR
                     , STATUS_RECORD_CHECK_INTEGRITY_ERROR
                     , MSYNC_ERROR
                     , MUNMAP_ERROR
                     , TRUNCATE_ERROR
                     , OPEN_ERROR
                     , MMAP_ERROR
                     , CLOSE_ERROR
                     , MEMCMP_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int alt_fd = LIXA_NULL_FD;
    
    LIXA_TRACE(("thread_status_sync_files\n"));
    TRY {
        status_record_t *alt_status;
        status_record_t **status_is;
        gchar *alt_filename;
        struct two_status_record_s tsr;
        off_t curr_status_size = 0;
        
        LIXA_CRASH(LIXA_CRASH_POINT_SERVER_BLOCK_COPY,
                   thread_status_get_crash_count(ts));
        
        if (LIXA_RC_OK != (ret_cod = gettimeofday(
                               &ts->curr_status->sr.ctrl.last_sync, NULL)))
            THROW(GETTIMEOFDAY_ERROR);
        status_record_update(ts->curr_status, 0, ts->updated_records);
        g_tree_foreach(ts->updated_records, traverse_and_sync,
                       ts->curr_status);
#ifdef LIXA_DEBUG
        if (LIXA_RC_OK != (ret_cod = status_record_check_integrity(
                               ts->curr_status)))
            THROW(STATUS_RECORD_CHECK_INTEGRITY_ERROR);
#endif /* LIXA_DEBUG */
        LIXA_TRACE(("thread_status_sync_files: before msync\n"));
        LIXA_CRASH(LIXA_CRASH_POINT_SERVER_BEFORE_MSYNC,
                   thread_status_get_crash_count(ts));
        
        if (-1 == msync(ts->curr_status,
                        ts->curr_status->sr.ctrl.number_of_blocks *
                        sizeof(status_record_t), MS_SYNC))
            THROW(MSYNC_ERROR);
        
        LIXA_TRACE(("thread_status_sync_files: after first msync\n"));
        LIXA_CRASH(LIXA_CRASH_POINT_SERVER_AFTER_MSYNC,
                   thread_status_get_crash_count(ts));
        
        /* current status file can become the baseline, discover and process
           alternate status file */
        if (ts->curr_status == ts->status1) {
            alt_status = ts->status2;
            status_is = &(ts->status2);
            alt_filename = ts->status2_filename;
        } else {
            alt_status = ts->status1;
            status_is = &(ts->status1);
            alt_filename = ts->status1_filename;
        }

        curr_status_size = sizeof(status_record_t) *
            ts->curr_status->sr.ctrl.number_of_blocks;
        
        /* compare size */
        if (ts->curr_status->sr.ctrl.number_of_blocks >
            alt_status->sr.ctrl.number_of_blocks) {
            off_t alt_status_size = sizeof(status_record_t) *
                alt_status->sr.ctrl.number_of_blocks;
            /* elarge alternate status file */
            LIXA_TRACE(("thread_status_sync_files: current status file "
                        "contains " UINT32_T_FORMAT
                        " blocks while alternate status file contains "
                        UINT32_T_FORMAT " blocks; I must perform file "
                        "enlargment before content copy\n",
                        ts->curr_status->sr.ctrl.number_of_blocks,
                        alt_status->sr.ctrl.number_of_blocks));
            /* reset the pointer in thread status structure... */
            *status_is = NULL;
            if (0 != munmap(alt_status, alt_status_size))
                THROW(MUNMAP_ERROR);
            if (-1 == truncate((const char *)alt_filename,
                               curr_status_size))
                THROW(TRUNCATE_ERROR);
            if (-1 == (alt_fd = open((const char *)alt_filename, O_RDWR)))
                THROW(OPEN_ERROR);
            if (NULL == (alt_status = mmap(NULL, curr_status_size,
                                           PROT_READ | PROT_WRITE,
                                           MAP_SHARED, alt_fd, 0)))
                THROW(MMAP_ERROR);
            if (0 != close(alt_fd)) {
                alt_fd = LIXA_NULL_FD;
                THROW(CLOSE_ERROR);
            }
            alt_fd = LIXA_NULL_FD;
        }
        /* copy modified records */
        tsr.first = ts->curr_status;
        tsr.second = alt_status;
        g_tree_foreach(ts->updated_records, traverse_and_copy, &tsr);
#ifdef LIXA_DEBUG
        /* the memory mapped status file must be equal */
        if (0 != memcmp(ts->curr_status, alt_status, curr_status_size)) {
            LIXA_TRACE(("thread_status_sync_files: memory mapped status "
                        "files are different after copy. INTERNAL ERROR\n"));
            syslog(LOG_CRIT, LIXA_SYSLOG_LXD010C);
            THROW(MEMCMP_ERROR);
        }
#endif /* LIXA_DEBUG */
        /* clean updated records set */
        g_tree_destroy(ts->updated_records);
        ts->updated_records = g_tree_new(size_t_compare_func);
        /* recover the pointer in thread status structure... */
        *status_is = alt_status;
        /* switch to alternate status mapped file */
        LIXA_TRACE(("thread_status_sync_files: switching current memory "
                    "mapped status file from %p...\n", ts->curr_status));
        ts->curr_status = alt_status;
        LIXA_TRACE(("thread_status_sync_files: ... switching current memory "
                    "mapped status file to %p...\n", ts->curr_status));
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case STATUS_RECORD_CHECK_INTEGRITY_ERROR:
                break;
            case MSYNC_ERROR:
                ret_cod = LIXA_RC_MSYNC_ERROR;
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case MEMCMP_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recovery actions */
        if (LIXA_NULL_FD  != alt_fd) {
            LIXA_TRACE(("thread_status_sync_files: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
            if (LIXA_NULL_FD != alt_fd) {
                LIXA_TRACE(("thread_status_sync_files: closing file "
                            "descriptor %d\n", alt_fd));
                close(alt_fd);
            }
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("thread_status_sync_files/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

        
