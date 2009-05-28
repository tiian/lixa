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



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



void payload_header_reset(struct payload_header *ph)
{
    ph->n = 0;
    memset(&ph->block_array, 0, sizeof(uint32_t) * CHAIN_MAX_SIZE);
    memset(&ph->serv_addr, 0, sizeof(struct sockaddr_in));
}



int status_record_load(union status_record_u **sr,
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
    union status_record_u *tmp_sra = NULL;
    
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
        tmp_sra[0].ctrl.status_file = status_file;
        
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



int status_record_insert(union status_record_u **sr,
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
    union status_record_u *tmp_sra = NULL;
    
    LIXA_TRACE(("status_record_insert\n"));
    TRY {
        union status_record_u *csr = *sr;
        if (csr[0].ctrl.first_free_block == 0) {
            struct stat fd_stat;
            off_t curr_size, new_size, delta_size, i;
            const char *status_file = csr[0].ctrl.status_file;

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
            tmp_sra[0].ctrl.first_free_block = curr_size;
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
                    csr[0].ctrl.first_free_block,
                    csr[0].ctrl.first_used_block));
        *slot = csr[0].ctrl.first_free_block;
        csr[0].ctrl.first_free_block = csr[*slot].data.next_block;
        csr[*slot].data.next_block = csr[0].ctrl.first_used_block;
        csr[0].ctrl.first_used_block = *slot;
        LIXA_TRACE(("status_record_insert: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT "\n",
                    csr[0].ctrl.first_free_block,
                        csr[0].ctrl.first_used_block));
        
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



int status_record_delete(union status_record_u **sr,
                         uint32_t slot)
{
    enum Exception { USED_BLOCK_NOT_FOUND
                     , OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("status_record_delete\n"));
    TRY {
        union status_record_u *csr = *sr;
        uint32_t ul; /* used list left block */
        uint32_t ur; /* used list right block */
        uint32_t fl; /* free list left block */
        uint32_t fr; /* free list right block */
        
        /* find ul and ur positions */
        ul = 0;
        ur = csr[0].ctrl.first_used_block;
        while (ur > 0) {
            if (ur == slot)
                break;
            ul = ur;
            ur = csr[ur].data.next_block;
        }
        if (ur == 0)
            THROW(USED_BLOCK_NOT_FOUND);
        
        /* find fl and fr positions */
        fl = 0;
        fr = csr[0].ctrl.first_free_block;
        while (fr > 0) {
            if (fr > slot)
                break;
            else if (fr == slot)
                THROW(OBJ_CORRUPTED);
            fl = fr;
            fr = csr[fr].data.next_block;
        }

        LIXA_TRACE(("status_record_delete: ul = " UINT32_T_FORMAT
                    ", ur = " UINT32_T_FORMAT ", fl = " UINT32_T_FORMAT
                    ", fr = " UINT32_T_FORMAT "\n", ul, ur, fl, fr));
        
        /* remove block from used block list */
        if (ul == 0) {
            /* first block in used block list */
            csr[0].ctrl.first_used_block = csr[ur].data.next_block;
        } else {
            /* central or last block in used block list */
            csr[ul].data.next_block = csr[ur].data.next_block;
        }
        csr[ur].data.next_block = 0;

        /* insert block in free block list */
        if (fl == 0) {
            /* insertion happens at list head or list is empty */
            if (fr != 0)
                /* list is not empty */
                csr[ur].data.next_block = fr;
            csr[0].ctrl.first_free_block = ur;
        } else {
            /* insertion happens in the middle or at list tail */
            csr[ur].data.next_block = fr;
            csr[fl].data.next_block = ur;
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

