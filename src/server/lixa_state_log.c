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
#include "lixa_state_table.h"
#include "lixa_syslog.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



size_t LIXA_STATE_LOG_RECORDS_PER_PAGE;
const char *LIXA_STATE_LOG_FILE_SUFFIX = ".log";



const char *lixa_state_log_status_string(
    enum lixa_state_log_status_e status)
{
    switch (status) {
        case STATE_LOG_UNDEFINED:   return "UNDEFINED";        break;
        case STATE_LOG_FORMATTED:   return "FORMATTED";        break;
        case STATE_LOG_OPENED:      return "OPENED";           break;
        case STATE_LOG_USED:        return "USED";             break;
        case STATE_LOG_FULL:        return "FULL";             break;
        case STATE_LOG_EXTENDED:    return "EXTENDED";         break;
        case STATE_LOG_CLOSED:      return "CLOSED";           break;
        case STATE_LOG_DISPOSED:    return "DISPOSED";         break;
        default:                    return "???";
    }
    return "!!!";
}



int lixa_state_log_init(lixa_state_log_t *this,
                        const char *pathname,
                        size_t max_buffer_size,
                        int read_only,
                        int o_direct_bool,
                        int o_dsync_bool,
                        int o_rsync_bool,
                        int o_sync_bool)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        INVALID_STATUS,
        STRDUP_ERROR,
        PTHREAD_MUTEX_INIT_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    
    LIXA_TRACE(("lixa_state_log_init: pathname='%s', o_direct_bool=%d, "
                "o_dsync_bool=%d, orsync_bool=%d, osync_bool=%d\n",
                pathname, o_direct_bool, o_dsync_bool, o_rsync_bool,
                o_sync_bool));
    TRY {
        int pers_flags;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == pathname)
            THROW(NULL_OBJECT2);
        if (0 == strlen(pathname))
            THROW(INVALID_OPTION);
        /* check the state log has not been already used */
        if (STATE_LOG_UNDEFINED != this->status)
            THROW(INVALID_STATUS);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_log_t));
        this->fd = LIXA_NULL_FD;
        /* try to open the already existent files */
        if (read_only)
            pers_flags = O_RDONLY;
        else
            pers_flags = O_RDWR;
        if (o_direct_bool) pers_flags |= O_DIRECT;
        if (o_dsync_bool)  pers_flags |= O_DSYNC;
        if (o_rsync_bool)  pers_flags |= O_RSYNC;
        if (o_sync_bool)   pers_flags |= O_SYNC;
        this->flags = pers_flags;
        /* keep a local copy of the pathname */
        if (NULL == (this->pathname = strdup(pathname)))
            THROW(STRDUP_ERROR);
        /* initialize the mutex used to protect the synchronized area */
        if (0 != (pte = pthread_mutex_init(
                      &this->file_synchronizer.mutex, NULL)))
            THROW(PTHREAD_MUTEX_INIT_ERROR);
        
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
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
            case PTHREAD_MUTEX_INIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_INIT_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_init/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_close(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        NOTHING_TO_DO,
        INVALID_STATUS,
        CLOSE_ERROR,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_close\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);

        /* check if the operation must be skipped because useless */
        if (STATE_LOG_UNDEFINED == this->status ||
            STATE_LOG_CLOSED == this->status) {
            LIXA_TRACE(("lixa_state_log_close: nothing to do, status is "
                        "%d:%s\n", this->status,
                        lixa_state_log_status_string(this->status)));
            THROW(NOTHING_TO_DO);
        }

        /* check if the current status is compatible with closing */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_CLOSED, TRUE)))
            THROW(INVALID_STATUS);
        /* close the file descriptor */
        if (-1 == close(this->fd)) {
            THROW(CLOSE_ERROR);
        }
        /* set new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_CLOSED, FALSE)))
            THROW(SET_STATUS);
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NOTHING_TO_DO:
                ret_cod = LIXA_RC_OK;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case INVALID_STATUS:
            case SET_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_clean(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_DESTROY_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    
    LIXA_TRACE(("lixa_state_log_clean\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        if (NULL != this->pathname) {
            free(this->pathname);
            this->pathname = NULL;
        }
        if (LIXA_NULL_FD != this->fd) {
            close(this->fd);
            this->fd = LIXA_NULL_FD;
        }
        if (STATE_LOG_UNDEFINED == this->status) {
            LIXA_TRACE(("lixa_state_log_clean: WARNING, status is "
                        "UNDEFINED!\n"));
        }
        if (0 != (pte = pthread_mutex_destroy(&this->file_synchronizer.mutex)))
            THROW(PTHREAD_MUTEX_DESTROY_ERROR);
        /* reset everything, bye bye... */
        memset(this, 0, sizeof(lixa_state_log_t));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PTHREAD_MUTEX_DESTROY_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_DESTROY_ERROR;
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



int lixa_state_log_extend(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        PTHREAD_MUTEX_LOCK_ERROR,
        TRUNCATE_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        SET_STATUS1,
        SET_STATUS2,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_log_extend\n"));
    TRY {
        off_t new_total_size;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_EXTENDED, TRUE)))
            THROW(INVALID_STATUS);
        /* obtain the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->file_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* compute new size and extend the file */
        new_total_size = this->file_synchronizer.total_size +
            LIXA_STATE_LOG_FILE_SIZE_INCREMENT;
        if (0 != ftruncate(this->fd, new_total_size))
            THROW(TRUNCATE_ERROR);
        /* update new size */
        this->file_synchronizer.total_size = new_total_size;
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(
                      &this->file_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        /* set the new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_EXTENDED, FALSE)))
            THROW(SET_STATUS1);
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_USED, FALSE)))
            THROW(SET_STATUS2);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case SET_STATUS1:
            case SET_STATUS2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->file_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_extend/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_file_exist(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_file_exist\n"));
    TRY {
        int fd;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* try to open the file with the same flags of a real usage */
        if (-1 == (fd = open(this->pathname, this->flags)))
            THROW(OPEN_ERROR);
        close(fd);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_file_exist/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_create_new_file(lixa_state_log_t *this, void *single_page)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        OPEN_ERROR,
        PTHREAD_MUTEX_LOCK_ERROR,
        TRUNCATE_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        SET_STATUS1,
        SET_STATUS2,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_log_create_new_file\n"));
    TRY {
        mode_t mode;
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_FORMATTED, TRUE)))
            THROW(INVALID_STATUS);
        /* add O_EXCL and O_CREAT flags: file must not exist */
        this->flags |= O_EXCL | O_CREAT;
        LIXA_TRACE(("lixa_state_log_create_new_file: creating file '%s'\n",
                    this->pathname));
        /* mode flags (security) */
        mode = S_IRUSR | S_IWUSR | S_IRGRP;
        if (-1 == (this->fd = open(this->pathname, this->flags, mode)))
            THROW(OPEN_ERROR);
        /* obtain the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->file_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        if (0 == ftruncate(this->fd, LIXA_STATE_LOG_FILE_SIZE_DEFAULT))
            this->file_synchronizer.total_size =
                LIXA_STATE_LOG_FILE_SIZE_DEFAULT;
        else
            THROW(TRUNCATE_ERROR);
        /* move the file pointer at the beginning */
        this->file_synchronizer.offset = 0;
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(
                      &this->file_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        /* set the new status to FORMATTED */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_FORMATTED, FALSE)))
            THROW(SET_STATUS1);
        /* set the new status to USED */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_USED, FALSE)))
            THROW(SET_STATUS2);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case SET_STATUS1:
            case SET_STATUS2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->file_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_create_new_file/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_open_file(lixa_state_log_t *this, void *single_page)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        OPEN_ERROR,
        FSTAT_ERROR,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_open_file\n"));
    TRY {
        struct stat fd_stat;
        off_t number_of_pages, i, j;
        int error = FALSE;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* check if the current status is compatible with closing */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_OPENED, TRUE)))
            THROW(INVALID_STATUS);
        
        LIXA_SYSLOG((LOG_DEBUG, LIXA_SYSLOG_LXD065D, this->pathname));
        /* open the file descriptor */
        if (-1 == (this->fd = open(this->pathname, this->flags))) {
            LIXA_TRACE(("lixa_state_log_open_file: open('%s')=%d "
                        "(%s)\n", this->pathname, errno, strerror(errno)));
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD066W,
                         this->pathname, errno, strerror(errno)));
            THROW(OPEN_ERROR);
        }
        /* retrieve log file size */
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR);
        number_of_pages = fd_stat.st_size / LIXA_SYSTEM_PAGE_SIZE;
        if (fd_stat.st_size % LIXA_SYSTEM_PAGE_SIZE) {
            /* someone or something removed or added bytes at the end of the
               state log file, anyway we will try to read them */
            LIXA_TRACE(("lixa_state_log_open_file: log file '%s' has size "
                        OFF_T_FORMAT " bytes that's not multiple of system "
                        "page size (" SIZE_T_FORMAT "); something strange "
                        "at the end of the file happened...\n",
                        fd_stat.st_size, LIXA_SYSTEM_PAGE_SIZE));
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD070N,
                         this->pathname, fd_stat.st_size,
                         LIXA_SYSTEM_PAGE_SIZE));
            number_of_pages++;
        }
        /* reset the area that will be used to store the data */
        memset(&this->read_info, 0, sizeof(this->read_info));
        /* analyze the content; this loop will try to read as much as
           possible from the underlying file
           Note: using a larger buffer would be faster, but this function is
           called only at warm start-up time: could be a future improvement!
        */
        for (i=0; i<number_of_pages; ++i) {
            ssize_t read_bytes;
            
            LIXA_TRACE(("lixa_state_log_open_file: reading page "
                        OFF_T_FORMAT "/" OFF_T_FORMAT "\n", i,
                        number_of_pages));
            read_bytes = pread(this->fd, single_page, LIXA_SYSTEM_PAGE_SIZE,
                               i*LIXA_SYSTEM_PAGE_SIZE);
            if (0 >= read_bytes) {
                LIXA_TRACE(("lixa_state_log_open_file: pread() returned error "
                            "%d ('%s'); read_bytes=" SSIZE_T_FORMAT "\n",
                            errno, strerror(errno)));
                LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD069W,
                             i, this->pathname, errno, strerror(errno),
                             read_bytes));
                error = TRUE;
                break; /* leave the loop prematurely */
            }
            LIXA_TRACE(("lixa_state_log_open_file: pread() returned "
                        SSIZE_T_FORMAT " bytes\n", read_bytes));
            /* reset the tail if a partial page has been read */
            if (read_bytes < LIXA_SYSTEM_PAGE_SIZE)
                memset(single_page + read_bytes, 0,
                       LIXA_SYSTEM_PAGE_SIZE - read_bytes);
            /* loop on the records that can be available in the page */
            for (j=0; j<LIXA_STATE_LOG_RECORDS_PER_PAGE; ++j) {
                uint32_t crc32;
                struct lixa_state_log_record_s *log_record =
                    (struct lixa_state_log_record_s *)
                    (single_page + j * sizeof(struct lixa_state_log_record_s));
                char iso_timestamp[ISO_TIMESTAMP_BUFFER_SIZE];
                /* compute crc32 of the record and check with the stored one */
                crc32 = lixa_crc32(
                    (const uint8_t *)log_record + sizeof(uint32_t),
                    sizeof(struct lixa_state_log_record_s) - sizeof(uint32_t));
                if (crc32 == log_record->crc32) {
                    if (0 == this->read_info.number_of_records) {
                        this->read_info.first_record_id = log_record->id;
                        this->read_info.first_record_timestamp =
                            log_record->timestamp;
                    }
                    this->read_info.last_record_id = log_record->id;
                    this->read_info.last_record_timestamp =
                        log_record->timestamp;
                    this->read_info.number_of_records++;
                    lixa_utils_iso_timestamp(&log_record->timestamp,
                                             iso_timestamp,
                                             sizeof(iso_timestamp));
                    LIXA_TRACE(("lixa_state_log_open_file: record "
                                OFF_T_FORMAT " in page " OFF_T_FORMAT " is "
                                "valid: id=" LIXA_WORD_T_FORMAT
                                ", timestamp=%s, crc32=" UINT32_T_XFORMAT
                                ", original_slot=" UINT32_T_FORMAT "\n",
                                j, i, log_record->id, iso_timestamp,
                                log_record->crc32, log_record->original_slot));
                } else {
                    LIXA_TRACE(("lixa_state_log_open_file: record "
                                OFF_T_FORMAT " in page " OFF_T_FORMAT " is "
                                "corrupted: on disk crc32=" UINT32_T_XFORMAT
                                ", current crc32=" UINT32_T_XFORMAT "\n",
                                j, i, log_record->crc32, crc32));
                    /* use only when really necessary: it creates a huge
                       amount of data
                    LIXA_TRACE_HEX_DATA(
                        "lixa_state_log_open_file: record content: ",
                        (const byte_t *)log_record,
                        sizeof(struct lixa_state_log_record_s));
                    */
                }
            } /* for (j=0; j<LIXA_STATE_LOG_RECORDS_PER_PAGE) */
        } /* for (i=0; i<number_of_pages; ++i) */
        LIXA_TRACE(("lixa_state_log_open_file: number_of_records="
                    OFF_T_FORMAT ", first record id=" LIXA_WORD_T_FORMAT
                    ", last record id=" LIXA_WORD_T_FORMAT "\n",
                    this->read_info.number_of_records,
                    this->read_info.first_record_id,
                    this->read_info.last_record_id));
        
        /* set new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_OPENED, FALSE)))
            THROW(SET_STATUS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
            case SET_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_open_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_read_file(lixa_state_log_t *this, off_t pos, off_t prev_pos,
                             struct lixa_state_log_record_s *buffer,
                             void *single_page)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        PREAD_ERROR,
        BUFFER_OVERFLOW,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_read_file(pos=" OFF_T_FORMAT ", prev_pos="
                OFF_T_FORMAT ")\n", pos, prev_pos));
    TRY {
        off_t page_number, pos_in_page, prev_page_number;
        ssize_t read_bytes;
        
        if (NULL == this)
            THROW(NULL_OBJECT);

        /* log file must be in opened status */
        if (STATE_LOG_OPENED != this->status)
            THROW(INVALID_STATUS);

        /* where is the record stored? */
        page_number = pos / LIXA_STATE_LOG_RECORDS_PER_PAGE;
        prev_page_number = prev_pos / LIXA_STATE_LOG_RECORDS_PER_PAGE;
        pos_in_page = pos % LIXA_STATE_LOG_RECORDS_PER_PAGE;
        if (page_number != prev_page_number) {
            /* read the whole page that contains the record */
            read_bytes = pread(this->fd, single_page, LIXA_SYSTEM_PAGE_SIZE,
                               page_number*LIXA_SYSTEM_PAGE_SIZE);
            /* pread OK? */
            if (0 >= read_bytes) {
                LIXA_TRACE(("lixa_state_log_read_file: pread() returned error "
                            "%d ('%s'); read_bytes=" SSIZE_T_FORMAT "\n",
                            errno, strerror(errno)));
                THROW(PREAD_ERROR);
            }
            /* the record has been read (the page could be broken if it's the
               last one) */
            if (read_bytes <
                (pos_in_page+1)*sizeof(struct lixa_state_log_record_s))
                THROW(BUFFER_OVERFLOW);
        } /* if (page_number != prev_page_number) */
        /* copy data to buffer */
        memcpy(buffer, single_page +
               pos_in_page * sizeof(struct lixa_state_log_record_s),
               sizeof(struct lixa_state_log_record_s));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case PREAD_ERROR:
                ret_cod = LIXA_RC_PREAD_ERROR;
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
    LIXA_TRACE(("lixa_state_log_read_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_set_reserved(lixa_state_log_t *this, off_t reserved)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        OUT_OF_RANGE,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_log_set_reserved\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* synchronize on mutex */
        if (0 != (pte = pthread_mutex_lock(&this->file_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* check consistency */
        if (this->file_synchronizer.offset + reserved <=
            this->file_synchronizer.total_size)
            this->file_synchronizer.reserved = reserved;
        else
            THROW(OUT_OF_RANGE);
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(
                      &this->file_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->file_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_set_reserved/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_write(lixa_state_log_t *this, const void *buffer,
                         size_t number_of_pages)
{
    enum Exception {
        NULL_OBJECT,
        PWRITE_ERROR,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_log_write\n"));
    TRY {
        ssize_t written_bytes;
        size_t count = lixa_state_common_pages2buffer(number_of_pages);
        off_t offset;
        lixa_timer_t timer;
        long duration;

        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* use the method to get a synchronized value */
        offset = lixa_state_log_get_offset(this);
        lixa_timer_start(&timer);
        written_bytes = pwrite(this->fd, buffer, count, offset);
        lixa_timer_stop(&timer);
        duration = lixa_timer_get_diff(&timer);
        if (count != written_bytes) {
            LIXA_TRACE(("lixa_state_log_write: pwrite has written "
                        SSIZE_T_FORMAT " bytes instead of " SIZE_T_FORMAT "\n",
                        written_bytes, count));
            THROW(PWRITE_ERROR);
        } else {
            LIXA_TRACE(("lixa_state_log_write: written " SIZE_T_FORMAT
                        " pages, " SSIZE_T_FORMAT " bytes in %ld us to log\n",
                        number_of_pages, written_bytes, duration));
            /* transform microseconds to milliseconds */
            duration /= 1000;
            if (duration > 1000) {
                LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD054W, duration));
            } else if (duration > 100) {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD055N, duration));
            } else if (duration > 10) {
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD056I, duration));
            }
            /* maybe unnecessary, but on some CPU the following operation
             * might be no atomic */
            /* obtain the lock of the synchronized structure */
            if (0 != (pte = pthread_mutex_lock(
                          &this->file_synchronizer.mutex))) {
                THROW(PTHREAD_MUTEX_LOCK_ERROR);
            } else
                mutex_locked = TRUE;
            this->file_synchronizer.offset += written_bytes;
            this->file_synchronizer.reserved -= written_bytes;
            /* unlock the mutex */
            if (0 != (pte = pthread_mutex_unlock(
                          &this->file_synchronizer.mutex))) {
                THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
            } else
                mutex_locked = FALSE;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PWRITE_ERROR:
                ret_cod = LIXA_RC_PWRITE_ERROR;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
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
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->file_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_write/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_set_status(lixa_state_log_t *this,
                              enum lixa_state_log_status_e new_status,
                              int dry_run)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_set_status(new_status=%d:%s, dry_run=%d)\n",
                new_status, lixa_state_log_status_string(new_status),
                dry_run));
    TRY {
        int valid = TRUE;
        
        if (NULL == this)
            THROW(NULL_OBJECT);

        LIXA_TRACE(("lixa_state_log_set_status: current status is %d:%s\n",
                    this->status, lixa_state_log_status_string(this->status)));
        switch (new_status) {
            case STATE_LOG_UNDEFINED:
                LIXA_TRACE(("lixa_state_log_set_status: transition to "
                            "UNDEFINED status is never acceptable\n"));
                valid = FALSE;
                break;
            case STATE_LOG_FORMATTED:
                if (STATE_LOG_UNDEFINED != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "FORMATTED is acceptable only from "
                                "UNDEFINED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_LOG_OPENED:
                if (STATE_LOG_UNDEFINED != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "OPENED is acceptable only from "
                                "UNDEFINED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_LOG_USED:
                if (STATE_LOG_FORMATTED != this->status &&
                    STATE_LOG_EXTENDED != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "USED is acceptable only from "
                                "FORMATTED and EXTENDED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_LOG_FULL:
                if (STATE_LOG_USED != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "FULL is acceptable only from USED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_LOG_EXTENDED:
                if (STATE_LOG_FULL != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "EXTENDED is acceptable only from FULL\n"));
                    valid = FALSE;
                }
                break;
            case STATE_LOG_CLOSED:
                if (STATE_LOG_USED != this->status &&
                    STATE_LOG_FULL != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "CLOSED is acceptable only from "
                                "USED and FULL\n"));
                    valid = FALSE;
                }
                break;
            case STATE_LOG_DISPOSED:
                if (STATE_LOG_CLOSED != this->status) {
                    LIXA_TRACE(("lixa_state_log_set_status: transition to "
                                "DISPOSED is acceptable only from "
                                "CLOSED\n"));
                    valid = FALSE;
                }
                break;
            default:
                LIXA_TRACE(("lixa_state_log_set_status: %d:%s is not a valid "
                            "value for a the new_status\n", new_status,
                            lixa_state_log_status_string(new_status)));
                valid = FALSE;
                break;
        } /* switch (new_status) */
        if (!valid) {
            THROW(INVALID_STATUS);
        } else if (dry_run) {
            LIXA_TRACE(("lixa_state_log_set_status: dry run, status can be "
                        "switched from %d:%s to %d:%s\n",
                        this->status,
                        lixa_state_log_status_string(this->status),
                        new_status,
                        lixa_state_log_status_string(new_status)));
        } else {
            LIXA_TRACE(("lixa_state_log_set_status: %d:%s -> %d:%s\n",
                        this->status,
                        lixa_state_log_status_string(this->status),
                        new_status,
                        lixa_state_log_status_string(new_status)));
            this->status = new_status;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
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
    LIXA_TRACE(("lixa_state_log_set_status/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



