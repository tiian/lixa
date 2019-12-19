/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
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
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_syslog.h"
#include "server_status.h"
#include "server_thread_status.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



extern gboolean run_as_daemon;
struct thread_pipe_array_s tpa;



gboolean traverse_and_sync(gpointer key, gpointer value, gpointer data)
{
    /* ignore value, data is the thread status */
    status_record_t *sr = (status_record_t *)data + (uintptr_t)key;
    LIXA_TRACE(("traverse_and_sync: synchronizing block " UINTPTR_T_FORMAT
                " (%p)\n", (uintptr_t)key, sr));
    return status_record_sync(sr);
}



gboolean traverse_and_copy(gpointer key, gpointer value, gpointer data)
{
    struct two_status_record_s *tsr = (struct two_status_record_s *)data;
    status_record_t *src = tsr->first;
    status_record_t *dest = tsr->second;
    LIXA_TRACE(("traverse_and_copy: copying block # " UINTPTR_T_FORMAT
                " from source status file (%p) to destination status file "
                "(%p)\n", (uintptr_t)key, src, dest));
    memcpy(dest + (uintptr_t)key, src + (uintptr_t)key,
           sizeof(status_record_t));
    return FALSE;
}



int payload_header_init(struct status_record_data_s *srd, int fd)
{
    enum Exception { GETTIMEOFDAY_ERROR
                     , GETSOCKNAME_ERROR
                     , GETPEERNAME_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("payload_header_init\n"));
    TRY {
        socklen_t serv_addr_len;

        srd->pld.type = DATA_PAYLOAD_TYPE_HEADER;
        srd->pld.ph.n = 0;
        memset(&srd->pld.ph.block_array, 0, sizeof(uint32_t) * CHAIN_MAX_SIZE);
        srd->pld.ph.next_branch_block = srd->pld.ph.prev_branch_block = 0;
        memset(&srd->pld.ph.local_sock_addr, 0, sizeof(struct sockaddr_in));
        memset(&srd->pld.ph.peer_sock_addr, 0, sizeof(struct sockaddr_in));
        memset(&srd->pld.ph.config_digest, 0, sizeof(md5_digest_hex_t));
        lixa_job_reset(&srd->pld.ph.job);
        memset(&srd->pld.ph.last_verb_step, 0,
               sizeof(struct lixa_msg_verb_step_s) * PAYLOAD_HEADER_VERB_STEP);
        common_status_conthr_init(&srd->pld.ph.state);
        srd->pld.ph.recovering_block_id = 0;
        srd->pld.ph.recovery_failed = FALSE;
        srd->pld.ph.recovery_commit = FALSE;
        memset(&srd->pld.ph.recovery_failed_time, 0, sizeof(struct timeval));
        
        /* set the timestamp of the client arrival */
        if (0 != gettimeofday(&srd->pld.ph.arrival_time, NULL))
            THROW(GETTIMEOFDAY_ERROR);
        
        /* retrieve properties from TCP socket; this code will not work
           for IP6 based or LOCAL sockets */
        serv_addr_len = sizeof(struct sockaddr_in);
        if (0 != getsockname(fd,
                             (struct sockaddr *)&srd->pld.ph.local_sock_addr,
                             &serv_addr_len))
            THROW(GETSOCKNAME_ERROR);
        serv_addr_len = sizeof(struct sockaddr_in);
        if (0 != getpeername(fd,
                             (struct sockaddr *)&srd->pld.ph.peer_sock_addr,
                             &serv_addr_len))
            THROW(GETPEERNAME_ERROR);
        LIXA_TRACE(("payload_header_init: initialized header block "
                    "socket file descriptor = %d, "
                    "local address = '%s', local port = '%hu', "
                    "peer address = '%s', peer port = '%hu'\n",
                    fd, inet_ntoa(srd->pld.ph.local_sock_addr.sin_addr),
                    ntohs(srd->pld.ph.local_sock_addr.sin_port),
                    inet_ntoa(srd->pld.ph.peer_sock_addr.sin_addr),
                    ntohs(srd->pld.ph.peer_sock_addr.sin_port)));
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case GETSOCKNAME_ERROR:
                ret_cod = LIXA_RC_GETSOCKNAME_ERROR;
                break;
            case GETPEERNAME_ERROR:
                ret_cod = LIXA_RC_GETPEERNAME_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("payload_header_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int payload_header_store_verb_step(struct thread_status_s *ts,
                                   uint32_t block_id,
                                   const struct lixa_msg_verb_step_s *vs)
{
    enum Exception {
        INVALID_RECORD,
        THREAD_STATUS_MARK_BLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("payload_header_store_verb_step\n"));
    TRY {
        int i;
        struct status_record_data_payload_s *pld =
            &(thread_status_get_record4update(ts, block_id)->data.pld);

        /* check the record is a payload header */
        if (pld->type != DATA_PAYLOAD_TYPE_HEADER) {
            LIXA_TRACE(("payload_header_store_verb_step: block_id="
                        UINT32_T_FORMAT ", type=%d\n", block_id, pld->type));
            THROW(INVALID_RECORD);
        }
        for (i=0; i<PAYLOAD_HEADER_VERB_STEP-1; ++i) {
            LIXA_TRACE(("payload_header_store_verb_step:"
                        "(verb,step): (%d,%d)[%d] <-- (%d,%d)[%d]\n",
                        pld->ph.last_verb_step[
                            PAYLOAD_HEADER_VERB_STEP-i-1].verb,
                        pld->ph.last_verb_step[
                            PAYLOAD_HEADER_VERB_STEP-i-1].step,
                        PAYLOAD_HEADER_VERB_STEP-i-1,
                        pld->ph.last_verb_step[
                            PAYLOAD_HEADER_VERB_STEP-i-2].verb,
                        pld->ph.last_verb_step[
                            PAYLOAD_HEADER_VERB_STEP-i-2].step,
                        PAYLOAD_HEADER_VERB_STEP-i-2));
            pld->ph.last_verb_step[PAYLOAD_HEADER_VERB_STEP-i-1] =
                pld->ph.last_verb_step[PAYLOAD_HEADER_VERB_STEP-i-2];
        } /* for (i=0; ... */
        LIXA_TRACE(("payload_header_store_verb_step:"
                    "(verb,step): (%d,%d)[0] <--- (%d,%d)\n",
                    pld->ph.last_verb_step[0].verb,
                    pld->ph.last_verb_step[0].step,
                    vs->verb, vs->step));
        pld->ph.last_verb_step[0] = *vs;
        /* update the record */
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(
                               ts, block_id)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_RECORD:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("payload_header_store_verb_step/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int payload_chain_release(struct thread_status_s *ts, uint32_t block_id)
{
    enum Exception { BLOCK_ID_IS_ZERO
                     , INVALID_BLOCK_TYPE
                     , STATUS_RECORD_DELETE1
                     , STATUS_RECORD_DELETE2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("payload_chain_release\n"));
    TRY {
        int i;
        const struct status_record_data_payload_s *pld;
        
        if (block_id == 0)
            THROW(BLOCK_ID_IS_ZERO);
        
        pld = &(thread_status_get_record4read(ts, block_id)->data.pld);
        if (pld->type != DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_TYPE);
        /* release chained blocks */
        for (i = 0; i < pld->ph.n; ++i) {
            LIXA_TRACE(("payload_chain_release: child # %d, releasing chained "
                        "block " UINT32_T_FORMAT "\n", i,
                        pld->ph.block_array[i]));
            if (LIXA_RC_OK != (
                    ret_cod = status_record_delete(
                        ts, pld->ph.block_array[i])))
                THROW(STATUS_RECORD_DELETE1);
        }
        /* release current block */
        LIXA_TRACE(("payload_chain_release: releasing header block "
                    UINT32_T_FORMAT "\n", block_id));
        if (LIXA_RC_OK != (ret_cod = status_record_delete(ts, block_id)))
            THROW(STATUS_RECORD_DELETE2);
        /* PAY ATTENTION NO DATA IS CLEANED: ONLY BLOCK REMOVAL HAPPENS
           THIS IS INTENTIONALLY DID BECAUSE WE DON'T WANT TO ERASE THE
           INFORMATION CONTAINED IN THE BLOCKS IMMEDIATELY
           THIS IS A CHALLENGE FOR THE PROGRAM BECAUSE A REMOVED BLOCK CAN
           BE DISTINGUISHED ONLY BECAUSE IT'S IN THE FREE BLOCK LIST INSTEAD
           OF USED BLOCK LIST */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BLOCK_ID_IS_ZERO:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case INVALID_BLOCK_TYPE:
                ret_cod = LIXA_RC_OBJ_NOT_INITIALIZED;
                break;
            case STATUS_RECORD_DELETE1:
            case STATUS_RECORD_DELETE2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("payload_chain_release/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int payload_chain_allocate(struct thread_status_s *ts, uint32_t block_id,
                           int size)
{
    enum Exception {
        OUT_OF_RANGE,
        INVALID_BLOCK_TYPE,
        STATUS_RECORD_INSERT_ERROR,
        THREAD_STATUS_MARK_BLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("payload_chain_allocate\n"));
    TRY {
        int i;
        struct status_record_data_payload_s *pld;
        
        /* check the request can be feasible */
        if (size > CHAIN_MAX_SIZE) {
            LIXA_TRACE(("payload_chain_allocate: size=%d, max=%d\n",
                        size, CHAIN_MAX_SIZE));
            THROW(OUT_OF_RANGE);
        }

        /* check this is a payload header */
        pld = &(thread_status_get_record4update(ts, block_id)->data.pld);
        if (pld->type != DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_TYPE);
        
        /* allocate the blocks */
        for (i=0; i<size; ++i) {
            uint32_t new_slot;
            struct status_record_data_payload_s *pld2;
            if (LIXA_RC_OK != (ret_cod = status_record_insert(
                                   ts, &new_slot))) {
                LIXA_TRACE(("payload_chain_allocate: error while allocating "
                            "%d of %d slot\n", i, size));
                THROW(STATUS_RECORD_INSERT_ERROR);
            }
            /* reset block payload content */
            pld2 = &(thread_status_get_record4update(ts, new_slot)->data.pld);
            memset(pld2, 0, sizeof(struct status_record_data_payload_s));
            pld2->type = DATA_PAYLOAD_TYPE_RSRMGR;
            /* point the new block from chain */
            pld = &(thread_status_get_record4update(ts, block_id)->data.pld);
            pld->ph.block_array[i] = new_slot;
            pld->ph.n++;
            
            LIXA_TRACE(("payload_chain_allocate: number of children is now "
                        "%d, last children is " UINT32_T_FORMAT "\n",
                        pld->ph.n, pld->ph.block_array[i]));
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(
                                   ts, block_id)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR);
        }
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case INVALID_BLOCK_TYPE:
                ret_cod = LIXA_RC_OBJ_NOT_INITIALIZED;
                break;
            case STATUS_RECORD_INSERT_ERROR:
                LIXA_TRACE(("payload_chain_allocate: unable to allocate "
                            "%d children blocks, releasing all...\n", size));
                if (LIXA_RC_OK == (ret_cod =
                                   payload_chain_release(ts, block_id)))
                    ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("payload_chain_allocate/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int status_record_load(status_record_t **sr,
                       const char *status_file,
                       GTree **updated_records,
                       int readonly)
{
    enum Exception {
        FILE_NOT_EXISTS,
        OPEN_ERROR,
        STATUS_RECORD_CREATE_FILE_ERROR,
        FSTAT_ERROR,
        MMAP_ERROR,
        CLOSE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = LIXA_NULL_FD;
    status_record_t *tmp_sra = NULL;
    
    LIXA_TRACE(("status_record_load\n"));
    TRY {
        struct stat fd_stat;
        LIXA_TRACE(("status_record_load: trying to open '%s' status file...\n",
                    status_file));
        if (-1 == (fd = open(status_file, readonly ? O_RDONLY : O_RDWR))) {

            if (ENOENT == errno) {
                LIXA_TRACE(("status_record_load: status file '%s' does not "
                            "exist\n", status_file));
                if (readonly)
                    THROW(FILE_NOT_EXISTS);
            } else
                THROW(OPEN_ERROR);
            /* the file does not exist and must be created */
            if (LIXA_RC_OK != (ret_cod = status_record_create_file(
                                   status_file, &fd)))
                THROW(STATUS_RECORD_CREATE_FILE_ERROR);
        }
        /* clean updated records set */
        g_tree_destroy(*updated_records);
        *updated_records = g_tree_new(size_t_compare_func);
        
        /* retrieve size */
        if (0 != fstat(fd, &fd_stat))
            THROW(FSTAT_ERROR);

        if (NULL == (tmp_sra = mmap(NULL, fd_stat.st_size,
                                    PROT_READ | PROT_WRITE,
                                    readonly ? MAP_PRIVATE : MAP_SHARED,
                                    fd, 0)))
            THROW(MMAP_ERROR);
        LIXA_TRACE(("status_record_load: status file '%s' mapped at "
                    "address %p\n", status_file, tmp_sra));

        /* point the status record array to the prepared memory mapped file */
        *sr = tmp_sra;
        
        if (0 != close(fd))
            THROW(CLOSE_ERROR);
        fd = LIXA_NULL_FD;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case FILE_NOT_EXISTS:
                ret_cod = LIXA_RC_FILE_NOT_EXISTS;
                break;
            case OPEN_ERROR:
                if (!run_as_daemon)
                    fprintf(stderr, "Error while opening file '%s' "
                            "(errno=%d '%s')\n",
                            status_file, errno, strerror(errno));
                else
                    LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD025E, status_file,
                                 errno, strerror(errno)));
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case STATUS_RECORD_CREATE_FILE_ERROR:
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recovery actions */
        if (NONE != excp) {
            LIXA_TRACE(("status_record_load: values before recovery actions "
                        "excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod,
                        errno));
            if (LIXA_NULL_FD != fd) {
                LIXA_TRACE(("status_record_load: closing file descriptor %d\n",
                            fd));
                close(fd);
            }
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("status_record_load/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int status_record_create_file(const char *status_file_name,
                              int *fd)
{
    enum Exception {
        OPEN_ERROR,
        GETTIMEOFDAY_ERROR,
        STATUS_RECORD_SYNC_ERROR,
        WRITE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("status_record_create_file\n"));
    TRY {
        int i;
        if (-1 == (*fd = open(status_file_name, O_RDWR | O_CREAT | O_EXCL,
                              S_IRUSR | S_IWUSR | S_IRGRP)))
                THROW(OPEN_ERROR);
        LIXA_TRACE(("status_record_create_file: created new status file '%s' "
                    "with file descriptor %d\n",
                    status_file_name, *fd));
        for (i = 0; i < STATUS_FILE_INIT_SIZE; ++i) {
            struct status_record_s tmp_sr;

            memset(&tmp_sr, 0, sizeof(tmp_sr));
            tmp_sr.counter = 1;
            if (!i) {
                /* write control record */
                tmp_sr.sr.ctrl.magic_number = STATUS_FILE_MAGIC_NUMBER;
                tmp_sr.sr.ctrl.level = STATUS_FILE_LEVEL;
                if (LIXA_RC_OK != (ret_cod = gettimeofday(
                                       &tmp_sr.sr.ctrl.last_sync, NULL)))
                    THROW(GETTIMEOFDAY_ERROR);
                tmp_sr.sr.ctrl.number_of_blocks = STATUS_FILE_INIT_SIZE;
                tmp_sr.sr.ctrl.first_used_block = 0;
                tmp_sr.sr.ctrl.first_free_block = 1;
            } else {
                if (i == STATUS_FILE_INIT_SIZE - 1)
                    tmp_sr.sr.data.next_block = 0;
                else
                    tmp_sr.sr.data.next_block = i + 1;
            }
            if (LIXA_RC_OK != (ret_cod = status_record_sync(&tmp_sr)))
                THROW(STATUS_RECORD_SYNC_ERROR);
            if (sizeof(tmp_sr) != write(*fd, &tmp_sr, sizeof(tmp_sr)))
                THROW(WRITE_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_ERROR:
                if (!run_as_daemon)
                    fprintf(stderr, "Error while opening file '%s' "
                            "(errno=%d '%s')\n",
                            status_file_name, errno, strerror(errno));
                else
                    LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD025E,
                                 status_file_name, errno, strerror(errno)));
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case STATUS_RECORD_SYNC_ERROR:
                break;
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
    LIXA_TRACE(("status_record_create_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int status_record_check_integrity(status_record_t *sr)
{
    enum Exception { FIRST_BLOCK_COUNTER_IS_ODD
                     , G_CHECKSUM_NEW_ERROR1
                     , DIGEST_SIZE_ERROR
                     , DIGEST_DOES_NOT_MATCH1
                     , BLOCK_COUNTER_IS_ODD
                     , G_CHECKSUM_NEW_ERROR2
                     , DIGEST_DOES_NOT_MATCH2
                     , NEXT_BLOCK_OUT_OF_RANGE
                     , DAMAGED_CHAINS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    GChecksum *checksum = NULL;

    LIXA_TRACE(("status_record_check_integrity\n"));
    TRY {
        struct status_record_s *first_block = sr;
        md5_digest_t digest;
        gsize digest_len = MD5_DIGEST_LENGTH;
        uint32_t i, free_blocks, used_blocks;
        
        /* check the integrity of the first block */
        /* check block counter is even */
        LIXA_TRACE(("status_record_check_integrity: checking first block "
                    "parity...\n"));
        if (first_block->counter%2) {
            LIXA_TRACE(("status_record_check_integrity: first block counter "
                        "is odd (" UINT32_T_FORMAT ") the status file was "
                        "NOT synchronized\n", first_block->counter));
            THROW(FIRST_BLOCK_COUNTER_IS_ODD);
        }
        LIXA_TRACE(("status_record_check_integrity: checking first block "
                    "signature...\n"));
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR1);
        g_checksum_update(checksum, (const guchar *)first_block,
                          STATUS_RECORD_CHECKSUM_SIZE);
        g_checksum_get_digest(checksum, digest, &digest_len);
#ifdef LIXA_DEBUG
        if (digest_len != MD5_DIGEST_LENGTH) {
            LIXA_TRACE(("status_record_check_integrity: internal error in "
                        "digest size expected=" SIZE_T_FORMAT ", returned="
                        SIZE_T_FORMAT "\n", MD5_DIGEST_LENGTH, digest_len));
            THROW(DIGEST_SIZE_ERROR);
        }
#endif /* LIXA_DEBUG */
        /* this g_checksum_free could be substituted with g_checksum_reset if
           it was available */
        g_checksum_free(checksum);
        checksum = NULL;
        LIXA_TRACE_HEX_DATA("status_record_check_integrity: "
                            "first block digest is: ",
                            first_block->digest, digest_len);
        LIXA_TRACE_HEX_DATA("status_record_check_integrity: "
                            "checksum digest is:    ", digest, digest_len);
        /* check digest */
        if (memcmp(digest, first_block->digest, digest_len)) {
            LIXA_TRACE(("status_record_check_integrity: the signature of the "
                        "first block is corrupted\n"));
            THROW(DIGEST_DOES_NOT_MATCH2);
        }
        LIXA_TRACE(("status_record_check_integrity: first block contains:\n"));
        LIXA_TRACE(("status_record_check_integrity:   [magic_number] = "
                    UINT32_T_FORMAT "\n", first_block->sr.ctrl.magic_number));
        LIXA_TRACE(("status_record_check_integrity:   [level] = "
                    UINT32_T_FORMAT "\n", first_block->sr.ctrl.level));
        LIXA_TRACE_HEX_DATA("status_record_check_integrity:   [last_sync] = ",
                            (byte_t *)&(first_block->sr.ctrl.last_sync),
                            sizeof(first_block->sr.ctrl.last_sync));
        LIXA_TRACE(("status_record_check_integrity:   [number_of_blocks] = "
                    UINT32_T_FORMAT "\n",
                    first_block->sr.ctrl.number_of_blocks));
        LIXA_TRACE(("status_record_check_integrity:   [first_used_block] = "
                    UINT32_T_FORMAT "\n",
                    first_block->sr.ctrl.first_used_block));
        LIXA_TRACE(("status_record_check_integrity:   [first_free_block] = "
                    UINT32_T_FORMAT "\n",
                    first_block->sr.ctrl.first_free_block));
        for (i=1; i<first_block->sr.ctrl.number_of_blocks; ++i) {
            struct status_record_s *curr_block = sr + i;
            LIXA_TRACE(("status_record_check_integrity: checking block # "
                        UINT32_T_FORMAT " address %p\n", i, curr_block));
            LIXA_TRACE(("status_record_check_integrity: checking block # "
                        UINT32_T_FORMAT " parity...\n"));
            if (curr_block->counter%2) {
                LIXA_TRACE(("status_record_check_integrity: block # "
                            UINT32_T_FORMAT " counter "
                            "is odd (" UINT32_T_FORMAT ") the status file was "
                            "NOT synchronized\n", i, curr_block->counter));
                THROW(BLOCK_COUNTER_IS_ODD);
            }
            LIXA_TRACE(("status_record_check_integrity: checking block # "
                        UINT32_T_FORMAT " signature...\n"));
            if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
                THROW(G_CHECKSUM_NEW_ERROR2);
            g_checksum_update(checksum, (const guchar *)curr_block,
                              STATUS_RECORD_CHECKSUM_SIZE);
            g_checksum_get_digest(checksum, digest, &digest_len);
            /* this g_checksum_free could be substituted with g_checksum_reset
               if it was available */
            g_checksum_free(checksum);
            checksum = NULL;
            
            LIXA_TRACE(("status_record_check_integrity: checking block # "
                        UINT32_T_FORMAT "\n", i));
            LIXA_TRACE_HEX_DATA("status_record_check_integrity: digest is: ",
                                curr_block->digest, digest_len);        
            LIXA_TRACE_HEX_DATA("status_record_check_integrity: checksum "
                                "digest is: ", digest, digest_len);        
            /* check digest */
            if (memcmp(digest, curr_block->digest, digest_len)) {
                LIXA_TRACE(("status_record_check_integrity: block # "
                            UINT32_T_FORMAT " is corrupted (signatures do not "
                            "match)\n", i));
                THROW(DIGEST_DOES_NOT_MATCH2);
            }
            LIXA_TRACE(("status_record_check_integrity:   [next_block] = "
                        UINT32_T_FORMAT "\n",
                        curr_block->sr.data.next_block));
            if (curr_block->sr.data.next_block >=
                first_block->sr.ctrl.number_of_blocks) {
                LIXA_TRACE(("status_record_check_integrity: next_block "
                            "is out of range (max = " UINT32_T_FORMAT ")\n",
                            first_block->sr.ctrl.number_of_blocks - 1));
                THROW(NEXT_BLOCK_OUT_OF_RANGE);
            }
        }
        /* check used blocks chain */
        LIXA_TRACE(("status_record_check_integrity: checking used blocks "
                    "chain...\n"));
        used_blocks = 0;
        if (0 != (i = first_block->sr.ctrl.first_used_block)) {
            used_blocks++;
            /* traverse the chain */
            while (0 != (i = sr[i].sr.data.next_block))
                used_blocks++;
        }
        LIXA_TRACE(("status_record_check_integrity: there are "
                    UINT32_T_FORMAT " data used blocks\n", used_blocks));
        
        /* check free blocks chain */
        LIXA_TRACE(("status_record_check_integrity: checking free blocks "
                    "chain...\n"));
        free_blocks = 0;
        if (0 != (i = first_block->sr.ctrl.first_free_block)) {
            free_blocks++;
            /* traverse the chain */
            while (0 != (i = sr[i].sr.data.next_block))
                free_blocks++;
        }
        LIXA_TRACE(("status_record_check_integrity: there are "
                    UINT32_T_FORMAT " data free blocks\n", free_blocks));
        if (used_blocks + free_blocks + 1 !=
            first_block->sr.ctrl.number_of_blocks) {
            LIXA_TRACE(("status_record_check_integrity: used blocks ("
                        UINT32_T_FORMAT ") + free blocks (" UINT32_T_FORMAT
                        ") + 1 != number of blocks (" UINT32_T_FORMAT ")\n",
                        used_blocks, free_blocks,
                        first_block->sr.ctrl.number_of_blocks));
            THROW(DAMAGED_CHAINS);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case FIRST_BLOCK_COUNTER_IS_ODD:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case G_CHECKSUM_NEW_ERROR1:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
#ifdef LIXA_DEBUG
            case DIGEST_SIZE_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
#endif /* LIXA_DEBUG */
            case DIGEST_DOES_NOT_MATCH1:
            case NEXT_BLOCK_OUT_OF_RANGE:
            case BLOCK_COUNTER_IS_ODD:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case G_CHECKSUM_NEW_ERROR2:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
            case DIGEST_DOES_NOT_MATCH2:
            case DAMAGED_CHAINS:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* memory recovery */
        if (NULL != checksum)
            g_checksum_free(checksum);
    } /* TRY-CATCH */
    LIXA_TRACE(("status_record_check_integrity/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void status_record_display_chains(const status_record_t *sr)
{
    uint32_t i, used_blocks = 0, free_blocks  = 0;

    LIXA_TRACE(("status_record_display_chains\n"));
        
    LIXA_TRACE(("status_record_display_chains: first block contains:\n"));
    LIXA_TRACE(("status_record_display_chains:   [magic_number] = "
                UINT32_T_FORMAT "\n", sr->sr.ctrl.magic_number));
    LIXA_TRACE(("status_record_display_chains:   [level] = "
                UINT32_T_FORMAT "\n", sr->sr.ctrl.level));
    LIXA_TRACE_HEX_DATA("status_record_display_chains:   [last_sync] = ",
                        (byte_t *)&(sr->sr.ctrl.last_sync),
                        sizeof(sr->sr.ctrl.last_sync));
    LIXA_TRACE(("status_record_display_chains:   [number_of_blocks] = "
                UINT32_T_FORMAT "\n",
                sr->sr.ctrl.number_of_blocks));
    LIXA_TRACE(("status_record_display_chains:   [first_used_block] = "
                UINT32_T_FORMAT "\n",
                sr->sr.ctrl.first_used_block));
    LIXA_TRACE(("status_record_display_chains:   [first_free_block] = "
                UINT32_T_FORMAT "\n",
                sr->sr.ctrl.first_free_block));
    /* display used blocks chain */
    LIXA_TRACE(("status_record_display_chains: traversing used blocks "
                "chain...\n"));
    if (0 != (i = sr->sr.ctrl.first_used_block)) {
        LIXA_TRACE(("status_record_display_chains: block = " UINT32_T_FORMAT
                    " next_block = " UINT32_T_FORMAT "\n",
                    i, sr[i].sr.data.next_block));
        used_blocks++;
        /* traverse the chain */
        while (0 != (i = sr[i].sr.data.next_block)) {
            LIXA_TRACE(("status_record_display_chains: block = "
                        UINT32_T_FORMAT " next_block = " UINT32_T_FORMAT "\n",
                        i, sr[i].sr.data.next_block));
            used_blocks++;
        }
    }
    LIXA_TRACE(("status_record_display_chains: there are "
                UINT32_T_FORMAT " data used blocks\n", used_blocks));
    
    /* display free blocks chain */
    LIXA_TRACE(("status_record_display_chains: traversing free blocks "
                "chain...\n"));
    if (0 != (i = sr->sr.ctrl.first_free_block)) {
        LIXA_TRACE(("status_record_display_chains: block = " UINT32_T_FORMAT
                    " next_block = " UINT32_T_FORMAT "\n",
                    i, sr[i].sr.data.next_block));
        free_blocks++;
        /* traverse the chain */
        while (0 != (i = sr[i].sr.data.next_block)) {
            LIXA_TRACE(("status_record_display_chains: block = "
                        UINT32_T_FORMAT " next_block = " UINT32_T_FORMAT "\n",
                        i, sr[i].sr.data.next_block));
            free_blocks++;
        }
    }
    LIXA_TRACE(("status_record_display_chains: there are "
                UINT32_T_FORMAT " data free blocks\n", free_blocks));
}



int status_record_insert(struct thread_status_s *ts,
                         uint32_t *slot)
{
    enum Exception {
        OPEN_ERROR,
        FSTAT_ERROR,
        MUNMAP_ERROR,
        CONTAINER_FULL,
        WRITE_ERROR,
        MMAP_ERROR,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        CLOSE_ERROR,
        THREAD_STATUS_MARK_BLOCK_ERROR3,
        THREAD_STATUS_MARK_BLOCK_ERROR4,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int fd = LIXA_NULL_FD;
    status_record_t *tmp_sra = NULL;
    
    LIXA_TRACE(("status_record_insert\n"));
    TRY {
        status_record_t *csr = ts->curr_status;
        gchar *status_filename = NULL;
        status_record_t **status_is = NULL;

        if (csr == ts->status1) {
            status_is = &(ts->status1);
            status_filename = ts->status1_filename;
        } else if (csr == ts->status2) {
            status_is = &(ts->status2);
            status_filename = ts->status2_filename;
        }
        
        if (csr[0].sr.ctrl.first_free_block == 0) {
            struct stat fd_stat;
            off_t curr_size, new_size, delta_size, i;

            LIXA_TRACE(("status_record_insert: free block list is "
                        "empty, status file resize in progres...\n"));
            
            /* open the file for append: we must add new records */
            if (-1 == (fd = open(status_filename, O_RDWR | O_APPEND))) {
                LIXA_TRACE(("status_record_insert: error while opening status "
                            "file '%s' (errno=%d)\n", status_filename, errno));
                THROW(OPEN_ERROR);
            }
            /* retrieve size */
            if (0 != fstat(fd, &fd_stat))
                THROW(FSTAT_ERROR);
            /* unmap the current mapping */
            if (0 != munmap(csr, fd_stat.st_size))
                THROW(MUNMAP_ERROR);
            csr = NULL;
            ts->curr_status = NULL;
            *status_is = NULL;

            curr_size = fd_stat.st_size / sizeof(status_record_t);
            delta_size = fd_stat.st_size / sizeof(status_record_t) *
                STATUS_FILE_DELTA_SIZE / 100;
            new_size = curr_size + delta_size;
            LIXA_TRACE(("status_record_insert: curr_size = " OFF_T_FORMAT
                        ", new_size = " OFF_T_FORMAT "\n",
                        curr_size, new_size));
            if (new_size > UINT32_MAX) {
                LIXA_TRACE(("status_record_insert: new size after resizing "
                            "would exceed " UINT32_T_FORMAT " max value; "
                            "resized to max value only\n", UINT32_MAX));
                new_size = UINT32_MAX;
                delta_size = new_size - curr_size;
                if (delta_size == 0)
                    THROW(CONTAINER_FULL);
            }
            for (i = 0; i < delta_size; ++i) {
                struct status_record_s tmp_sr;

                memset(&tmp_sr, 0, sizeof(tmp_sr));
                if (i == delta_size - 1)
                    tmp_sr.sr.data.next_block = 0;
                else
                    tmp_sr.sr.data.next_block = curr_size + i + 1;
                LIXA_TRACE(("status_record_insert: new block "
                            UINT32_T_FORMAT " (" UINT32_T_FORMAT ") "
                            "next_block = " UINT32_T_FORMAT "\n",
                            i, i + curr_size, tmp_sr.sr.data.next_block));
                if (sizeof(tmp_sr) != write(fd, &tmp_sr, sizeof(tmp_sr)))
                    THROW(WRITE_ERROR);
            } /* for (i = 0; i < delta_size; ++i) */
            /* map the status file again */
            if (NULL == (tmp_sra = mmap(NULL,
                                        new_size * sizeof(status_record_t),
                                        PROT_READ | PROT_WRITE,
                                        MAP_SHARED, fd, 0)))
                THROW(MMAP_ERROR);

            /* update free block list */
            tmp_sra[0].sr.ctrl.first_free_block = curr_size;
            tmp_sra[0].sr.ctrl.number_of_blocks = new_size;
            LIXA_TRACE(("status_record_insert: status file '%s' mapped at "
                        "address %p\n", ts->status1_filename, tmp_sra));
            *status_is = ts->curr_status = csr = tmp_sra;

            /* mark the first record for change */
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, 0)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
            /* mark all the new blocks for change */
            for (i=0; i<delta_size; ++i) {
                if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(
                                       ts, curr_size+i)))
                    THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
            } /* for (i=0; i<delta_size; ++i) */
             
            if (0 != close(fd))
                THROW(CLOSE_ERROR);
            fd = LIXA_NULL_FD;            
        }

        /* update pointer */
        LIXA_TRACE(("status_record_insert: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT "\n",
                    csr[0].sr.ctrl.first_free_block,
                    csr[0].sr.ctrl.first_used_block));
        *slot = csr[0].sr.ctrl.first_free_block;
        csr[0].sr.ctrl.first_free_block = csr[*slot].sr.data.next_block;
        csr[*slot].sr.data.next_block = csr[0].sr.ctrl.first_used_block;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, *slot)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR3);
        csr[0].sr.ctrl.first_used_block = *slot;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, 0)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR4);
        LIXA_TRACE(("status_record_insert: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT ", last inserted next block = "
                    UINT32_T_FORMAT "\n",
                    csr[0].sr.ctrl.first_free_block,
                    csr[0].sr.ctrl.first_used_block,
                    csr[*slot].sr.data.next_block));

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case CONTAINER_FULL:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
            case THREAD_STATUS_MARK_BLOCK_ERROR2:
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR3:
            case THREAD_STATUS_MARK_BLOCK_ERROR4:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (NONE != excp) {
            LIXA_TRACE(("status_record_insert: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
            if (LIXA_NULL_FD != fd) {
                LIXA_TRACE(("status_record_insert: closing file "
                            "descriptor %d\n", fd));
                close(fd);
            }
            
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("status_record_insert/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int status_record_delete(struct thread_status_s *ts,
                         uint32_t slot)
{
    enum Exception {
        USED_BLOCK_NOT_FOUND,
        OBJ_CORRUPTED,
        THREAD_STATUS_MARK_BLOCK_ERROR1,
        THREAD_STATUS_MARK_BLOCK_ERROR2,
        THREAD_STATUS_MARK_BLOCK_ERROR3,
        THREAD_STATUS_MARK_BLOCK_ERROR4,
        THREAD_STATUS_MARK_BLOCK_ERROR5,
        THREAD_STATUS_MARK_BLOCK_ERROR6,
        THREAD_STATUS_MARK_BLOCK_ERROR7,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("status_record_delete\n"));
    TRY {
        status_record_t *csr = ts->curr_status;
        uint32_t ul; /* used list left block */
        uint32_t ur; /* used list right block */
        uint32_t fl; /* free list left block */
        uint32_t fr; /* free list right block */
        
        /* find ul and ur positions */
        ul = 0;
        ur = csr[0].sr.ctrl.first_used_block;
        LIXA_TRACE(("status_record_delete: ul=" UINT32_T_FORMAT
                    ", ur=" UINT32_T_FORMAT "\n", ul, ur));
        while (ur > 0) {
            if (ur == slot)
                break;
            ul = ur;
            ur = csr[ur].sr.data.next_block;
#ifdef LIXA_DEBUG
            LIXA_TRACE(("status_record_delete: ul=" UINT32_T_FORMAT
                        ", ur=" UINT32_T_FORMAT "\n", ul, ur));
#endif /* LIXA_DEBUG */
        }
        if (ur == 0)
            THROW(USED_BLOCK_NOT_FOUND);
        
        /* find fl and fr positions */
        fl = 0;
        fr = csr[0].sr.ctrl.first_free_block;
        while (fr > 0) {
            if (fr > slot)
                break;
            else if (fr == slot)
                THROW(OBJ_CORRUPTED);
            fl = fr;
            fr = csr[fr].sr.data.next_block;
        }

        LIXA_TRACE(("status_record_delete: ul=" UINT32_T_FORMAT
                    ", ur=" UINT32_T_FORMAT ", fl=" UINT32_T_FORMAT
                    ", fr=" UINT32_T_FORMAT "\n", ul, ur, fl, fr));
        
        /* remove block from used block list */
        if (ul == 0) {
            /* first block in used block list */
            csr[0].sr.ctrl.first_used_block = csr[ur].sr.data.next_block;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, 0)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR1);
        } else {
            /* central or last block in used block list */
            csr[ul].sr.data.next_block = csr[ur].sr.data.next_block;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, ul)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR2);
        }
        csr[ur].sr.data.next_block = 0;
        if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, ur)))
            THROW(THREAD_STATUS_MARK_BLOCK_ERROR3);

        /* insert block in free block list */
        if (fl == 0) {
            /* insertion happens at list head or list is empty */
            if (fr != 0) {
                /* list is not empty */
                csr[ur].sr.data.next_block = fr;
                if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, ur)))
                    THROW(THREAD_STATUS_MARK_BLOCK_ERROR4);
            }
            csr[0].sr.ctrl.first_free_block = ur;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, 0)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR5);
        } else {
            /* insertion happens in the middle or at list tail */
            csr[ur].sr.data.next_block = fr;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, ur)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR6);
            csr[fl].sr.data.next_block = ur;
            if (LIXA_RC_OK != (ret_cod = thread_status_mark_block(ts, fl)))
                THROW(THREAD_STATUS_MARK_BLOCK_ERROR7);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case USED_BLOCK_NOT_FOUND:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case THREAD_STATUS_MARK_BLOCK_ERROR1:
            case THREAD_STATUS_MARK_BLOCK_ERROR2:
            case THREAD_STATUS_MARK_BLOCK_ERROR3:
            case THREAD_STATUS_MARK_BLOCK_ERROR4:
            case THREAD_STATUS_MARK_BLOCK_ERROR5:
            case THREAD_STATUS_MARK_BLOCK_ERROR6:
            case THREAD_STATUS_MARK_BLOCK_ERROR7:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("status_record_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int status_record_sync(status_record_t *sr)
{
    enum Exception { G_CHECKSUM_NEW_ERROR
                     , DIGEST_SIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("status_record_sync\n"));
    TRY {
        GChecksum *checksum;
        gsize digest_len = MD5_DIGEST_LENGTH;
        if (sr->counter%2) {
            sr->counter++;
        } else {
            LIXA_TRACE(("status_record_sync: WARNING! record %p is already "
                        "even (it was NOT updated before!)\n", sr));
        }
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR);
        g_checksum_update(checksum, (const guchar *)sr,
                          STATUS_RECORD_CHECKSUM_SIZE);
        g_checksum_get_digest(checksum, sr->digest, &digest_len);
#ifdef LIXA_DEBUG
        if (digest_len != MD5_DIGEST_LENGTH) {
            LIXA_TRACE(("status_record_sync: internal error in digest size "
                        "expected=" SIZE_T_FORMAT ", returned=" SIZE_T_FORMAT
                        "\n", MD5_DIGEST_LENGTH, digest_len));
            THROW(DIGEST_SIZE_ERROR);
        }
#endif /* LIXA_DEBUG */
        g_checksum_free(checksum);
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_CHECKSUM_NEW_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
#ifdef LIXA_DEBUG
            case DIGEST_SIZE_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;         
#endif /* LIXA_DEBUG */
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("status_record_sync/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int status_record_copy(status_record_t *dest, const status_record_t *src,
                       struct thread_status_s *ts)
{
    enum Exception { DEST_EQUAL_SRC
                     , DEST_DOES_NOT_POINT_STATUS
                     , STAT_ERROR
                     , MUNMAP_ERROR
                     , TRUNCATE_ERROR
                     , OPEN_ERROR
                     , MMAP_ERROR
                     , CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    int dest_fd = LIXA_NULL_FD;

    LIXA_TRACE(("status_record_copy\n"));
    TRY {
        gchar *dest_filename = NULL;
        struct stat fstat;
        off_t src_size = 0;
        if (dest == src)
            THROW(DEST_EQUAL_SRC);
        if (dest == ts->status1) {
            dest_filename = ts->status1_filename;
        } else if (dest == ts->status2) {
            dest_filename = ts->status2_filename;
        } else
            THROW(DEST_DOES_NOT_POINT_STATUS);
        /* retrieve destination file size */
        if (-1 == stat((const char *)dest_filename, &fstat))
            THROW(STAT_ERROR);
        src_size = src[0].sr.ctrl.number_of_blocks * sizeof(status_record_t);
        if (fstat.st_size != src_size) {
            LIXA_TRACE(("status_record_copy: source status file is "
                        OFF_T_FORMAT " bytes long (" UINT32_T_FORMAT
                        " blocks) while destination status file is "
                        OFF_T_FORMAT " bytes long; I must perform file "
                        "enlargment before content copy\n",
                        src_size, src[0].sr.ctrl.number_of_blocks,
                        fstat.st_size));
            /* reset the pointer in thread status structure... */
            if (0 != munmap(dest, fstat.st_size))
                THROW(MUNMAP_ERROR);
            if (-1 == truncate((const char *)dest_filename, src_size))
                THROW(TRUNCATE_ERROR);
            if (-1 == (dest_fd = open((const char *)dest_filename, O_RDWR)))
                THROW(OPEN_ERROR);
            if (NULL == (dest = mmap(NULL, src_size, PROT_READ | PROT_WRITE,
                                     MAP_SHARED, dest_fd, 0)))
                THROW(MMAP_ERROR);
            /* recover the pointer in thread status structure... */
            if (0 != close(dest_fd)) {
                dest_fd = LIXA_NULL_FD;
                THROW(CLOSE_ERROR);
            }
            dest_fd = LIXA_NULL_FD;
        }
        LIXA_TRACE(("status_record_copy: copying " OFF_T_FORMAT " bytes from "
                    "source (%p) to destination (%p)\n", src_size, src, dest));
        memcpy(dest, src, src_size);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case DEST_EQUAL_SRC:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case DEST_DOES_NOT_POINT_STATUS:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case STAT_ERROR:
                ret_cod = LIXA_RC_STAT_ERROR;
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recovery actions */
        if (LIXA_NULL_FD  != dest_fd) {
            LIXA_TRACE(("status_record_copy: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
            if (LIXA_NULL_FD != dest_fd) {
                LIXA_TRACE(("status_record_copy: closing file "
                            "descriptor %d\n", dest_fd));
                close(dest_fd);
            }
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("status_record_copy/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int size_t_compare_func(gconstpointer a, gconstpointer b) {
    if (a < b)
        return -1;
    else if (a > b)
        return 1;
    else
        return 0;
}



void server_client_status_init(struct server_client_status_s *scs)
{
    lixa_session_reset(&scs->session);
    scs->output_buffer = NULL;
    scs->output_buffer_size = 0;
    scs->last_verb_step.verb = 0;
    scs->last_verb_step.step = 0;
    scs->branch_join = CLIENT_BRANCH_JOIN_NULL;
    thread_status_switch_init(&scs->switch_thread);
}
