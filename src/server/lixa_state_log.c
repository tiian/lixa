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


#define _GNU_SOURCE /* necessary for O_DIRECT extension */
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
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



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_state_log.h"
#include "lixa_syslog.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



size_t LIXA_STATE_LOG_RECORDS_PER_PAGE;



int lixa_state_log_init(lixa_state_log_t *this,
                        const char *path_prefix,
                        size_t max_buffer_size,
                        int o_direct_bool,
                        int o_dsync_bool,
                        int o_rsync_bool,
                        int o_sync_bool)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        MALLOC_ERROR1,
        STATE_LOG_FILE_INIT_ERROR,
        BUFFER_OVERFLOW,
        POSIX_MEMALIGN_ERROR1,
        MALLOC_ERROR2,
        PTHREAD_MUTEX_INIT_ERROR,
        PTHREAD_COND_INIT_ERROR,
        POSIX_MEMALIGN_ERROR2,
        PTHREAD_CREATE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    char *pathname = NULL;
    
    LIXA_TRACE(("lixa_state_log_init: path_prefix='%s', o_direct_bool=%d, "
                "o_dsync_bool=%d, orsync_bool=%d, osync_bool=%d\n",
                path_prefix, o_direct_bool, o_dsync_bool, o_rsync_bool,
                o_sync_bool));
    TRY {
        int error, i;
        int pers_flags;
        size_t pathname_len;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == path_prefix)
            THROW(NULL_OBJECT2);
        if (0 == (pathname_len = strlen(path_prefix)))
            THROW(INVALID_OPTION);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_log_t));
        /* try to open the already existent files */
        pers_flags = O_RDWR;
        if (o_direct_bool) pers_flags |= O_DIRECT;
        if (o_dsync_bool)  pers_flags |= O_DSYNC;
        if (o_rsync_bool)  pers_flags |= O_RSYNC;
        if (o_sync_bool)   pers_flags |= O_SYNC;
        /* allocate a buffer for the file names */
        pathname_len += strlen(LIXA_STATE_LOG_FILE_SUFFIX) + 100;
        if (NULL == (pathname = (char *)malloc(pathname_len)))
            THROW(MALLOC_ERROR1);
        /* initialize the file objects */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_LOG_FILE_SUFFIX);
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_file_init(
                                   &(this->files[i]), pathname, pers_flags)))
                THROW(STATE_LOG_FILE_INIT_ERROR);
        } /* for (i=0; i<LIXA_STATE_TABLES; ++i) */
        /* compute the number of records per page */
        LIXA_STATE_LOG_RECORDS_PER_PAGE = LIXA_SYSTEM_PAGE_SIZE /
            sizeof(struct lixa_state_log_record_s);
        /* check the system page is not too small... */
        if (0 == LIXA_STATE_LOG_RECORDS_PER_PAGE)
            THROW(BUFFER_OVERFLOW);
        /* allocate the single memory page */
        if (0 != (error = posix_memalign(&this->single_page,
                                         LIXA_SYSTEM_PAGE_SIZE,
                                         LIXA_SYSTEM_PAGE_SIZE))) {
            LIXA_TRACE(("lixa_state_log_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR1);
        }
        memset(this->single_page, 0, LIXA_SYSTEM_PAGE_SIZE);
        if (max_buffer_size < LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT) {
            LIXA_TRACE(("lixa_state_log_init: max_buffer_size ("
                        SIZE_T_FORMAT ") is less then "
                        "default (" SIZE_T_FORMAT "), using default\n",
                        max_buffer_size, LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT));
            max_buffer_size = LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT;
        }
        /* max number of records in buffer, config limit */
        this->max_number_of_block_ids = lixa_state_log_buffer2blocks(
            max_buffer_size);
        /* initial size of the buffer */
        this->size_of_block_ids = lixa_state_log_buffer2blocks(
            LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT);
        /* buffer is empty */
        this->number_of_block_ids = 0;
        /* allocate the array for block_ids */
        if (NULL == (this->block_ids = (uint32_t *)malloc(
                         sizeof(uint32_t) * this->size_of_block_ids)))
            THROW(MALLOC_ERROR2);
        memset(this->block_ids, 0, sizeof(uint32_t) * this->size_of_block_ids);
        /* initialize the synchronized internal structure shared with the
           flusher thread */
        if (0 != (pte = pthread_mutex_init(&this->flusher.mutex, NULL)))
            THROW(PTHREAD_MUTEX_INIT_ERROR);
        if (0 != (pte = pthread_cond_init(&this->flusher.cond, NULL)))
            THROW(PTHREAD_COND_INIT_ERROR);
        this->flusher.operation = STATE_LOG_FLUSHER_WAIT;
        this->flusher.file_pos = 0;
        this->flusher.to_be_flushed = FALSE;
        this->flusher.number_of_pages = 0;
        /* allocate the buffer for I/O */
        this->flusher.buffer_size = lixa_state_log_blocks2buffer(
            this->size_of_block_ids);
        if (0 != (error = posix_memalign(&this->flusher.buffer,
                                         LIXA_SYSTEM_PAGE_SIZE,
                                         this->flusher.buffer_size))) {
            LIXA_TRACE(("lixa_state_log_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR2);
        } else {
            LIXA_TRACE(("lixa_state_log_init: size_of_block_ids="
                        SIZE_T_FORMAT ", "
                        "number_of_records_per_page= " SIZE_T_FORMAT ", "
                        "buffer_size= " SIZE_T_FORMAT "\n",
                        this->size_of_block_ids,
                        LIXA_STATE_LOG_RECORDS_PER_PAGE,
                        this->flusher.buffer_size));
        }
        memset(this->flusher.buffer, 0, this->flusher.buffer_size);
        /* activate flusher thread */
        if (0 != (pte = pthread_create(&this->flusher.thread, NULL,
                                       lixa_state_log_flusher,
                                       (void *)this)))
            THROW(PTHREAD_CREATE_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_OPTION:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case STATE_LOG_FILE_INIT_ERROR:
                break;
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case POSIX_MEMALIGN_ERROR1:
            case POSIX_MEMALIGN_ERROR2:
                ret_cod = LIXA_RC_POSIX_MEMALIGN_ERROR;
                break;
            case MALLOC_ERROR1:
            case MALLOC_ERROR2:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case PTHREAD_MUTEX_INIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_INIT_ERROR;
                break;
            case PTHREAD_COND_INIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_INIT_ERROR;
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
        /* recover memory in the event of error */
        if (NULL != pathname)
            free(pathname);
        if (NONE != excp) { 
            if (excp > POSIX_MEMALIGN_ERROR1 && NULL != this->single_page) {
                free(this->single_page);
                this->single_page = NULL;
            }
            if (excp > MALLOC_ERROR2 && NULL != this->block_ids) {
                free(this->block_ids);
                this->block_ids = NULL;
            }
            if (excp > POSIX_MEMALIGN_ERROR2 && NULL != this->flusher.buffer) {
                free(this->flusher.buffer);
                this->flusher.buffer = NULL;
            }
        } /* if (NONE != excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_init/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_clean(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        PTHREAD_JOIN_ERROR,
        PTHREAD_COND_DESTROY_ERROR,
        PTHREAD_MUTEX_DESTROY_ERROR,
        LOG_FILE_CLEAN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    
    LIXA_TRACE(("lixa_state_log_clean\n"));
    TRY {
        size_t i;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* obtain the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->flusher.mutex)))
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        /* ask flusher termination */
        LIXA_TRACE(("lixa_state_log_clean: sending to flusher thread "
                    "exit request...\n"));
        this->flusher.operation = STATE_LOG_FLUSHER_EXIT;
        if (0 != (pte = pthread_cond_signal(&this->flusher.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR);
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(&this->flusher.mutex)))
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        /* wait flusher termination */
        LIXA_TRACE(("lixa_state_log_clean: waiting flusher thread "
                    "termination...\n"));
        if (0 != (pte = pthread_join(this->flusher.thread, NULL)))
            THROW(PTHREAD_JOIN_ERROR);
        /* mutex and condition are no more necessary */
        if (0 != (pte = pthread_cond_destroy(&this->flusher.cond)))
            THROW(PTHREAD_COND_DESTROY_ERROR);
        if (0 != (pte = pthread_mutex_destroy(&this->flusher.mutex)))
            THROW(PTHREAD_MUTEX_DESTROY_ERROR);
        /* clean all the log files objects */
        for (i=0; i<LIXA_STATE_TABLES; ++i)
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_file_clean(
                                   &this->files[i])))
                THROW(LOG_FILE_CLEAN_ERROR);
        if (NULL != this->flusher.buffer)
            free(this->flusher.buffer);
        if (NULL != this->single_page)
            free(this->single_page);
        if (NULL != this->block_ids)
            free(this->block_ids);
        /* reset everything, bye bye... */
        memset(this, 0, sizeof(lixa_state_log_t));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case PTHREAD_COND_SIGNAL_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_SIGNAL_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case PTHREAD_JOIN_ERROR:
                ret_cod = LIXA_RC_PTHREAD_JOIN_ERROR;
                break;
            case PTHREAD_COND_DESTROY_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_DESTROY_ERROR;
                break;
            case PTHREAD_MUTEX_DESTROY_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_DESTROY_ERROR;
                break;
            case LOG_FILE_CLEAN_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_clean/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_flush(lixa_state_log_t *this,
                         status_record_t *status_records)
{
    enum Exception {
        PTHREAD_MUTEX_LOCK_ERROR,
        BUFFER_OVERFLOW1,
        BUFFER_OVERFLOW2,
        GETTIMEOFDAY_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    
    LIXA_TRACE(("lixa_state_log_flush\n"));
    TRY {
        size_t number_of_pages;
        size_t number_of_pages_in_buffer;
        size_t pos_in_page, filled_pages, r;
        size_t log_free_pages;
        struct timeval timestamp;

        /* acquire the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->flusher.mutex)))
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        /* compute the number of buffer pages */
        number_of_pages = lixa_state_log_blocks2pages(
            this->number_of_block_ids);
        /* compute the number of page in the buffer */
        number_of_pages_in_buffer =
            this->flusher.buffer_size / LIXA_SYSTEM_PAGE_SIZE;
        /* this should never be true */
        if (number_of_pages > number_of_pages_in_buffer)
            THROW(BUFFER_OVERFLOW1);
        /* check the amount of free space in the log */
        log_free_pages = lixa_state_common_buffer2pages(
            lixa_state_log_file_get_total_size(
                &this->files[this->used_file]) -
            lixa_state_log_file_get_offset(
                &this->files[this->used_file]));
        /* this should never be true */
        if (number_of_pages > log_free_pages)
            THROW(BUFFER_OVERFLOW2);
        LIXA_TRACE(("lixa_state_log_flush: "
                    "number_of_block_ids=" UINT32_T_FORMAT ", "
                    "number_of_records_per_page=" SIZE_T_FORMAT ", "
                    "number_of_pages=" SIZE_T_FORMAT ", "
                    "number_of_pages_in_buffer=" SIZE_T_FORMAT ", "
                    "log_free_pages=" SIZE_T_FORMAT "\n",
                    this->number_of_block_ids,
                    LIXA_STATE_LOG_RECORDS_PER_PAGE,
                    number_of_pages, number_of_pages_in_buffer,
                    log_free_pages));
        /* retrieve the timestamp associated to this flush operation */
        if (0 != gettimeofday(&timestamp, NULL))
            THROW(GETTIMEOFDAY_ERROR);
        /* reset the buffer */
        memset(this->flusher.buffer, 0,
               number_of_pages * LIXA_SYSTEM_PAGE_SIZE);
        /* loop on the number of records */
        pos_in_page = 0;
        filled_pages = 0;
        for (r=0; r<this->number_of_block_ids; ++r) {
            struct lixa_state_log_record_s *log_record =
                (struct lixa_state_log_record_s *)
                (this->flusher.buffer + filled_pages * LIXA_SYSTEM_PAGE_SIZE +
                 pos_in_page * sizeof(struct lixa_state_log_record_s));
            union status_record_u *record =
                (union status_record_u *)
                &((status_records + this->block_ids[r])->sr);
            
            log_record->id = ++(this->last_record_id);
            if (0 == log_record->id) /* again, 0 is reserved for null */
                log_record->id = ++(this->last_record_id);
            log_record->timestamp = timestamp;
            memcpy(&(log_record->record), record,
                   sizeof(union status_record_u));
            /* compute the CRC32 code */
            log_record->crc32 = lixa_crc32(
                (const uint8_t *)log_record,
                sizeof(struct lixa_state_log_record_s) - sizeof(uint32_t));
            LIXA_TRACE(("lixa_state_log_flush: filled_page = " SIZE_T_FORMAT
                        ", pos_in_page = " SIZE_T_FORMAT
                        ", log_record->id = " UINT32_T_FORMAT
                        ", log_record->crc32 = 0x%08x\n",
                        filled_pages, pos_in_page, log_record->id,
                        log_record->crc32));
            pos_in_page++;
            if (pos_in_page % LIXA_STATE_LOG_RECORDS_PER_PAGE == 0) {
                pos_in_page = 0;
                filled_pages++;
            }
        } /* for (r=0; r<number_of_records_to_be_flushed; ++r) */
        /*
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_file_write(
                               &this->files[this->used_file],
                               this->flusher.buffer, number_of_pages)))
            THROW(LOG_FILE_WRITE_ERROR);
        */
        /* ask to flusher thread to perform the flushing */
        LIXA_TRACE(("lixa_state_log_flush: asking flusher thread to flush "
                    "the buffer to file\n"));
        this->flusher.operation = STATE_LOG_FLUSHER_FLUSH;
        this->flusher.file_pos = this->used_file;
        this->flusher.to_be_flushed = TRUE;
        this->flusher.number_of_pages = number_of_pages;
        if (0 != (pte = pthread_cond_signal(&this->flusher.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR);
        if (0 != (pte = pthread_mutex_unlock(&this->flusher.mutex)))
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        /* reset block_ids */
        this->number_of_block_ids = 0;
        memset(this->block_ids, 0,
               sizeof(uint32_t) * this->size_of_block_ids);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case BUFFER_OVERFLOW1:
            case BUFFER_OVERFLOW2:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case PTHREAD_COND_SIGNAL_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_SIGNAL_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_flush/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_mark_block(lixa_state_log_t *this,
                              uint32_t block_id)
{
    enum Exception {
        NULL_OBJECT,
        BUFFER_OVERFLOW,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_mark_block\n"));
    TRY {
        /* check the object */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check there is space in the buffer */
        if (this->size_of_block_ids == this->number_of_block_ids) {
            /* this is an internal error because it should never happen! */
            LIXA_TRACE(("lixa_state_log_mark_block: size_of_block_ids = "
                        "number_of_block_ids = " UINT32_T_FORMAT "\n",
                        this->size_of_block_ids));
            THROW(BUFFER_OVERFLOW);
        }
        /* keep track of the passed block_id */
        this->block_ids[this->number_of_block_ids++] = block_id;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_mark_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_check_actions(lixa_state_log_t *this, int *must_flush,
                                 int *must_switch)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        BUFFER_OVERFLOW,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    *must_flush = FALSE;
    *must_switch = FALSE;
    
    LIXA_TRACE(("lixa_state_log_check_actions\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check buffer size and space availability */
        if (this->number_of_block_ids == this->size_of_block_ids) {
            /* all the slots of the current buffer have been used */
            if (this->size_of_block_ids >= this->max_number_of_block_ids) {
                /* upper limit reached, it can not be expanded */
                *must_flush = TRUE;
                /* buffer_size is read without locking the mutex, but this is
                   safe because buffer_size is not modified by the flusher
                   thread */
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD050N,
                             this->flusher.buffer_size));
            } else {
                /* upper limit not reached, trying to extend it */
                size_t new_buffer_size;
                size_t new_number_of_block_ids;
                void *new_buffer = NULL;
                uint32_t *new_block_ids = NULL;
                int error;

                /* add 20% */
                new_number_of_block_ids = this->number_of_block_ids * 12 / 10;
                /* at least one page must be added, 20% can be 0 for small
                   values*/
                if (new_number_of_block_ids == this->number_of_block_ids)
                    new_number_of_block_ids++;
                /* check max size is not exceeded */
                if (new_number_of_block_ids > this->max_number_of_block_ids)
                    new_number_of_block_ids = this->max_number_of_block_ids;
                /* compute the buffer size */
                new_buffer_size = lixa_state_log_blocks2buffer(
                    new_number_of_block_ids);
                /* re-compute the number of blocks to avoid buffer unused
                   space */
                new_number_of_block_ids = lixa_state_log_buffer2blocks(
                    new_buffer_size);
                /* allocate the new areas */
                error = posix_memalign(&new_buffer, LIXA_SYSTEM_PAGE_SIZE,
                                       new_buffer_size);
                new_block_ids = (uint32_t *)malloc(
                    sizeof(uint32_t) * new_number_of_block_ids);
                /* check both allocation together */
                if (error != 0 || new_block_ids == NULL) {
                    if (NULL == new_block_ids) {
                        LIXA_TRACE(("lixa_state_log_check_actions/"
                                    "malloc: returned null!\n"));
                        free(new_buffer);
                        new_buffer = NULL;
                    }
                    if (error != 0) {
                        LIXA_TRACE(("lixa_state_log_check_actions/"
                                    "posix_memalign: error=%d\n", error));
                        if (NULL != new_block_ids) {
                            free(new_block_ids);
                            new_block_ids = NULL;
                        }
                        if (NULL != new_buffer) { /* it should never happen */
                            free(new_buffer);
                            new_buffer = NULL;
                        }
                    }
                    /* unable to expand the buffer, it can not be expanded */
                    *must_flush = TRUE;
                    LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD052W));
                } else {
                    /* buffer has been expanded, swtching to the new buffer
                       and array, access to buffer is protected by the mutex */
                    if (0 != (pte = pthread_mutex_lock(&this->flusher.mutex)))
                        THROW(PTHREAD_MUTEX_LOCK_ERROR);
                    LIXA_TRACE(("lixa_state_log_check_actions: "
                                "buffer expanded from " SIZE_T_FORMAT
                                " to " SIZE_T_FORMAT " bytes, "
                                "size_of_block_ids expanded from "
                                SIZE_T_FORMAT " to " SIZE_T_FORMAT
                                " records (max_number_of_block_ids = "
                                SIZE_T_FORMAT ")\n",
                                this->flusher.buffer_size, new_buffer_size,
                                this->size_of_block_ids,
                                new_number_of_block_ids,
                                this->max_number_of_block_ids));
                    memset(new_block_ids, 0,
                           sizeof(uint32_t) * new_number_of_block_ids);
                    memcpy(new_block_ids, this->block_ids,
                           this->number_of_block_ids * sizeof(uint32_t));
                    free(this->block_ids);
                    this->block_ids = new_block_ids;
                    this->size_of_block_ids = new_number_of_block_ids;
                    memset(new_buffer, 0, new_buffer_size);
                    free(this->flusher.buffer);
                    this->flusher.buffer = new_buffer;
                    this->flusher.buffer_size = new_buffer_size;
                    if (0 != (pte = pthread_mutex_unlock(
                                  &this->flusher.mutex)))
                        THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
                } /* if (error != 0 || new_block_ids == NULL) */
            } /* if (this->size_of_block_ids == this->max_number_of_ ... */
        } /* if (this->number_of_block_ids == this->size_of_block_ids) */
        /* check again size because it could have been expanded in the
           previous step*/
        if (this->number_of_block_ids != this->size_of_block_ids) {
            /* there are free slots in the current buffer */
            uint32_t current_needed_pages = lixa_state_log_needed_pages(
                this, this->number_of_block_ids);
            uint32_t future_needed_pages = lixa_state_log_needed_pages(
                this, this->number_of_block_ids+1);
            size_t available_pages =
                (lixa_state_log_file_get_total_size(
                    &this->files[this->used_file]) -
                 lixa_state_log_file_get_offset(
                    &this->files[this->used_file])) /
                LIXA_SYSTEM_PAGE_SIZE;
            LIXA_TRACE(("lixa_state_log_check_actions: "
                        "number_of_block_ids=" UINT32_T_FORMAT ", "
                        "current_needed_pages= " UINT32_T_FORMAT ", "
                        "future_needed_pages=" UINT32_T_FORMAT ", "
                        "available_pages=" SIZE_T_FORMAT "\n",
                        this->number_of_block_ids,
                        current_needed_pages, future_needed_pages,
                        available_pages));
            if (current_needed_pages > available_pages)
                /* this is a severe internal error, a bug */
                THROW(BUFFER_OVERFLOW);
            if (current_needed_pages == available_pages &&
                future_needed_pages > available_pages) {
                *must_flush = TRUE;
                *must_switch = TRUE;
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD051I,
                             lixa_state_log_file_get_pathname(
                                 &this->files[this->used_file])));
            }
        } /* if (this->number_of_block_ids != this->size_of_block_ids) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_check_actions/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



void *lixa_state_log_flusher(void *data)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_WAIT_ERROR,
        OUT_OF_RANGE,
        LOG_FILE_WRITE_ERROR,
        INTERNAL_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0; /* pthread_error */
    lixa_state_log_t *log = (lixa_state_log_t *)data;
    
    LIXA_TRACE(("lixa_state_log_flusher\n"));
    TRY {
        int exit = FALSE;

        if (NULL == log)
            THROW(NULL_OBJECT);
        
        while (!exit) {
            LIXA_TRACE(("lixa_state_log_flusher: locking flusher_mutex...\n"));
            if (0 != (pte = pthread_mutex_lock(&log->flusher.mutex)))
                THROW(PTHREAD_MUTEX_LOCK_ERROR);
            /* check the operation asked by the main thread */
            if (STATE_LOG_FLUSHER_WAIT == log->flusher.operation) {
                LIXA_TRACE(("lixa_state_log_flusher: WAITING on condition"
                            "...\n"));
                if (0 != (pte = pthread_cond_wait(
                              &log->flusher.cond, &log->flusher.mutex)))
                    THROW(PTHREAD_COND_WAIT_ERROR);
                LIXA_TRACE(("lixa_state_log_flusher: condition has been "
                            "signaled\n"));
            }
            if (STATE_LOG_FLUSHER_FLUSH == log->flusher.operation) {
                LIXA_TRACE(("lixa_state_log_flusher: FLUSHING file #"
                            SIZE_T_FORMAT "\n", log->flusher.file_pos));
                /* checking array index to avoid memory crash */
                if (LIXA_STATE_TABLES <= log->flusher.file_pos)
                    THROW(OUT_OF_RANGE);
                /* flush the data to file */
                if (log->flusher.to_be_flushed) {
                    if (LIXA_RC_OK != (ret_cod = lixa_state_log_file_write(
                                           &log->files[
                                               log->flusher.file_pos],
                                           log->flusher.buffer,
                                           log->flusher.number_of_pages)))
                        THROW(LOG_FILE_WRITE_ERROR);
                } else {
                    /* this is an internal error, it should never happen */
                    LIXA_TRACE(("lixa_state_log_flusher: internal error, "
                                "flusher.operation=%d and "
                                "flusher.to_be_flushed=%d\n",
                                log->flusher.operation,
                                log->flusher.to_be_flushed));
                    THROW(INTERNAL_ERROR);
                }
                /* reset flushing condition */
                log->flusher.to_be_flushed = FALSE;
                log->flusher.operation = STATE_LOG_FLUSHER_WAIT;
                /* signaling to master thread */
                if (0 != (pte = pthread_cond_signal(&log->flusher.cond)))
                    THROW(PTHREAD_COND_SIGNAL_ERROR);
            }
            if (STATE_LOG_FLUSHER_EXIT == log->flusher.operation) {
                LIXA_TRACE(("lixa_state_log_flusher: EXITING\n"));
                exit = TRUE;
            }
            /* this should never happen */
            if (STATE_LOG_FLUSHER_WAIT != log->flusher.operation &&
                STATE_LOG_FLUSHER_FLUSH != log->flusher.operation &&
                STATE_LOG_FLUSHER_EXIT != log->flusher.operation) {
                LIXA_TRACE(("lixa_state_log_flusher: internal error "
                            "flusher_operation=%d\n",
                            log->flusher.operation));
                exit = TRUE;
            }
            /* unlock the mutex */
            LIXA_TRACE(("lixa_state_log_flusher: unlocking flusher_mutex\n"));
            if (0 != (pte = pthread_mutex_unlock(&log->flusher.mutex)))
                THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } /* while (!exit) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case PTHREAD_COND_WAIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_WAIT_ERROR;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case LOG_FILE_WRITE_ERROR:
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case PTHREAD_COND_SIGNAL_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_SIGNAL_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (excp > PTHREAD_MUTEX_LOCK_ERROR &&
            excp < PTHREAD_MUTEX_UNLOCK_ERROR) {
            int pte2 = pthread_mutex_unlock(&log->flusher.mutex);
            LIXA_TRACE(("lixa_state_log_flusher: unlocked mutex after "
                        "error, pthread_mutex_unlock returned %d\n", pte2));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_flusher/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return NULL;
}

