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
#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
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
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



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
        
        srd->next_block = 0;
        srd->pld.type = DATA_PAYLOAD_TYPE_HEADER;
        srd->pld.ph.n = 0;
        memset(&srd->pld.ph.block_array, 0, sizeof(uint32_t) * CHAIN_MAX_SIZE);
        memset(&srd->pld.ph.local_sock_addr, 0, sizeof(struct sockaddr_in));
        memset(&srd->pld.ph.peer_sock_addr, 0, sizeof(struct sockaddr_in));

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



int payload_chain_release(status_record_t **sr, uint32_t slot)
{
    enum Exception { SLOT_IS_ZERO
                     , INVALID_BLOCK_TYPE
                     , STATUS_RECORD_DELETE1
                     , STATUS_RECORD_DELETE2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("payload_chain_release\n"));
    TRY {
        int i;
        status_record_t *csr = *sr;
        
        if (slot == 0)
            THROW(SLOT_IS_ZERO);
        if (csr[slot].sr.data.pld.type != DATA_PAYLOAD_TYPE_HEADER)
            THROW(INVALID_BLOCK_TYPE);
        /* release chained blocks */
        for (i = 0; i < csr[slot].sr.data.pld.ph.n; ++i) {
#ifndef NDEBUG
            LIXA_TRACE(("payload_chain_release: releasing chained block "
                        UINT32_T_FORMAT "\n",
                        csr[slot].sr.data.pld.ph.block_array[i]));
#endif            
            if (LIXA_RC_OK != (ret_cod = status_record_delete(
                                   sr,
                                   csr[slot].sr.data.pld.ph.block_array[i])))
                THROW(STATUS_RECORD_DELETE1);
        }
        /* release current block */
        LIXA_TRACE(("payload_chain_release: releasing header block "
                    UINT32_T_FORMAT "\n", slot));
        if (LIXA_RC_OK != (ret_cod = status_record_delete(sr, slot)))
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
            case SLOT_IS_ZERO:
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



int status_record_load(status_record_t **sr,
                       const char *status_file)
{
    enum Exception { OPEN_ERROR1
                     , OPEN_ERROR2
                     , WRITE_ERROR
                     , FSTAT_ERROR
                     , MMAP_ERROR
                     , CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = LIXA_NULL_FD;
    status_record_t *tmp_sra = NULL;
    
    LIXA_TRACE(("status_record_load\n"));
    TRY {
        struct stat fd_stat;
        LIXA_TRACE(("status_record_load: trying to open '%s' status file...\n",
                    status_file));
        if (-1 == (fd = open(status_file, O_RDWR))) {
            int i;
            
            LIXA_TRACE(("status_record_load: status file '%s' does not "
                        "exist\n", status_file));
            if (ENOENT != errno)
                THROW(OPEN_ERROR1);
            /* the file does not exist and must be created */
            if (-1 == (fd = open(status_file, O_RDWR | O_CREAT | O_EXCL,
                                 S_IRUSR | S_IWUSR | S_IRGRP)))
                THROW(OPEN_ERROR2);
            LIXA_TRACE(("status_record_load: created new status file '%s' "
                        "with file descriptor %d\n",
                        status_file, fd));
            for (i = 0; i < STATUS_FILE_INIT_SIZE; ++i) {
                union status_record_u tmp_sr;

                memset(&tmp_sr, 0, sizeof(tmp_sr));
                if (i == 0) {
                    /* write control record */
                    tmp_sr.ctrl.magic_number = STATUS_FILE_MAGIC_NUMBER;
                    tmp_sr.ctrl.level = STATUS_FILE_LEVEL;
                    tmp_sr.ctrl.first_used_block = 0;
                    tmp_sr.ctrl.first_free_block = 1;
                } else {
                    if (i == STATUS_FILE_INIT_SIZE - 1)
                        tmp_sr.data.next_block = 0;
                    else
                        tmp_sr.data.next_block = i + 1;
                }
                if (sizeof(tmp_sr) != write(fd, &tmp_sr, sizeof(tmp_sr)))
                    THROW(WRITE_ERROR);
            }
        }

        /* retrieve size */
        if (0 != fstat(fd, &fd_stat))
            THROW(FSTAT_ERROR);

        if (NULL == (tmp_sra = mmap(NULL, fd_stat.st_size,
                                    PROT_READ | PROT_WRITE,
                                    MAP_SHARED, fd, 0)))
            THROW(MMAP_ERROR);
        /* set the status file name reference for future usage */
        tmp_sra[0].sr.ctrl.status_file = status_file;
        
        LIXA_TRACE(("status_record_load: status file '%s' mapped at "
                    "address %p\n", status_file, tmp_sra));
        *sr = tmp_sra;

        if (0 != close(fd))
            THROW(CLOSE_ERROR);
        fd = LIXA_NULL_FD;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_ERROR1:
            case OPEN_ERROR2:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
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



int status_record_insert(status_record_t **sr,
                         uint32_t *slot)
{
    enum Exception { OPEN_ERROR
                     , FSTAT_ERROR
                     , MUNMAP_ERROR
                     , CONTAINER_FULL
                     , WRITE_ERROR
                     , MMAP_ERROR
                     , CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int fd = LIXA_NULL_FD;
    status_record_t *tmp_sra = NULL;
    
    LIXA_TRACE(("status_record_insert\n"));
    TRY {
        status_record_t *csr = *sr;
        if (csr[0].sr.ctrl.first_free_block == 0) {
            struct stat fd_stat;
            off_t curr_size, new_size, delta_size, i;
            const char *status_file = csr[0].sr.ctrl.status_file;

            LIXA_TRACE(("status_record_insert: free block list is "
                        "empty, status file resize in progres...\n"));
            
            /* open the file for append: we must add new records */
            if (-1 == (fd = open(status_file, O_RDWR | O_APPEND)))
                THROW(OPEN_ERROR);
            /* retrieve size */
            if (0 != fstat(fd, &fd_stat))
                THROW(FSTAT_ERROR);
            /* unmap the current mapping */
            if (0 != munmap(*sr, fd_stat.st_size))
                THROW(MUNMAP_ERROR);
            *sr = NULL;
            curr_size = fd_stat.st_size / sizeof(union status_record_u);
            delta_size = fd_stat.st_size / sizeof(union status_record_u) *
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
                if (curr_size == UINT32_MAX)
                    THROW(CONTAINER_FULL)
            }
            for (i = 0; i < delta_size; ++i) {
                union status_record_u tmp_sr;

                memset(&tmp_sr, 0, sizeof(tmp_sr));
                if (i == delta_size - 1)
                    tmp_sr.data.next_block = 0;
                else
                    tmp_sr.data.next_block = curr_size + i + 1;
                if (sizeof(tmp_sr) != write(fd, &tmp_sr, sizeof(tmp_sr)))
                    THROW(WRITE_ERROR);
            }
            /* map the status file again */
            if (NULL == (tmp_sra = mmap(NULL, fd_stat.st_size,
                                        PROT_READ | PROT_WRITE,
                                        MAP_SHARED, fd, 0)))
                THROW(MMAP_ERROR);
            /* update free block list */
            tmp_sra[0].sr.ctrl.first_free_block = curr_size;
            LIXA_TRACE(("status_record_insert: status file '%s' mapped at "
                        "address %p\n", status_file, tmp_sra));
            *sr = tmp_sra;
            
            if (0 != close(fd))
                THROW(CLOSE_ERROR);
            fd = LIXA_NULL_FD;            
        }

        /* update pointer */
        csr = *sr;
        LIXA_TRACE(("status_record_insert: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT "\n",
                    csr[0].sr.ctrl.first_free_block,
                    csr[0].sr.ctrl.first_used_block));
        *slot = csr[0].sr.ctrl.first_free_block;
        csr[0].sr.ctrl.first_free_block = csr[*slot].sr.data.next_block;
        csr[*slot].sr.data.next_block = csr[0].sr.ctrl.first_used_block;
        csr[0].sr.ctrl.first_used_block = *slot;
        LIXA_TRACE(("status_record_insert: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT "\n",
                    csr[0].sr.ctrl.first_free_block,
                        csr[0].sr.ctrl.first_used_block));
        
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
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
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



int status_record_delete(status_record_t **sr,
                         uint32_t slot)
{
    enum Exception { USED_BLOCK_NOT_FOUND
                     , OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("status_record_delete\n"));
    TRY {
        status_record_t *csr = *sr;
        uint32_t ul; /* used list left block */
        uint32_t ur; /* used list right block */
        uint32_t fl; /* free list left block */
        uint32_t fr; /* free list right block */
        
        /* find ul and ur positions */
        ul = 0;
        ur = csr[0].sr.ctrl.first_used_block;
        while (ur > 0) {
            if (ur == slot)
                break;
            ul = ur;
            ur = csr[ur].sr.data.next_block;
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

        LIXA_TRACE(("status_record_delete: ul = " UINT32_T_FORMAT
                    ", ur = " UINT32_T_FORMAT ", fl = " UINT32_T_FORMAT
                    ", fr = " UINT32_T_FORMAT "\n", ul, ur, fl, fr));
        
        /* remove block from used block list */
        if (ul == 0) {
            /* first block in used block list */
            csr[0].sr.ctrl.first_used_block = csr[ur].sr.data.next_block;
        } else {
            /* central or last block in used block list */
            csr[ul].sr.data.next_block = csr[ur].sr.data.next_block;
        }
        csr[ur].sr.data.next_block = 0;

        /* insert block in free block list */
        if (fl == 0) {
            /* insertion happens at list head or list is empty */
            if (fr != 0)
                /* list is not empty */
                csr[ur].sr.data.next_block = fr;
            csr[0].sr.ctrl.first_free_block = ur;
        } else {
            /* insertion happens in the middle or at list tail */
            csr[ur].sr.data.next_block = fr;
            csr[fl].sr.data.next_block = ur;
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
            LIXA_TRACE(("status_record_sync: WARNING! record %p already even "
                        "it was NOT updated before!\n", sr));
        }
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR);
        g_checksum_update(checksum, (const guchar *)sr,
                          STATUS_RECORD_CHECKSUM_SIZE);
        g_checksum_get_digest(checksum, sr->digest, &digest_len);
#ifndef NDEBUG
        if (digest_len != MD5_DIGEST_LENGTH) {
            LIXA_TRACE(("status_record_sync: internal error in digest size "
                        "expected=" SIZE_T_FORMAT ", returned=" SIZE_T_FORMAT
                        "\n", MD5_DIGEST_LENGTH, digest_len));
            THROW(DIGEST_SIZE_ERROR);
        }
#endif /* NDEBUG */
        g_checksum_free(checksum);
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_CHECKSUM_NEW_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
#ifndef NDEBUG
            case DIGEST_SIZE_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;         
#endif /* NDEBUG */
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

