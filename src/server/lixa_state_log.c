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
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_close\n"));
    TRY {
        
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
        SET_STATUS,
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
        /* set the new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                               this, STATE_LOG_FORMATTED, FALSE)))
            THROW(SET_STATUS);
        
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
            case SET_STATUS:
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



int lixa_state_log_open_file(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_open_file\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD065I, this->pathname));
        /* open the file descriptor */
        if (-1 == (this->fd = open(this->pathname, this->flags))) {
            LIXA_TRACE(("lixa_state_log_open_file: open('%s')=%d "
                        "(%s)\n", this->pathname, errno, strerror(errno)));
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD066W,
                         this->pathname, errno, strerror(errno)));
            THROW(OPEN_ERROR);
        }
        /* analyze the content */
        /* @@@ */
        
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
    LIXA_TRACE(("lixa_state_log_open_file/excp=%d/"
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
                        " pages, " SSIZE_T_FORMAT " bytes in %ld ms to log\n",
                        number_of_pages, written_bytes, duration));
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
    
    LIXA_TRACE(("lixa_state_log_set_status(new_status=%d, dry_run=%d)\n",
                new_status, dry_run));
    TRY {
        int valid = TRUE;
        
        if (NULL == this)
            THROW(NULL_OBJECT);

        LIXA_TRACE(("lixa_state_log_set_status: current status is %d\n",
                    this->status));
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
                LIXA_TRACE(("lixa_state_log_set_status: %d is not a valid "
                            "value for a the new_status\n", new_status));
                valid = FALSE;
                break;
        } /* switch (new_status) */
        if (!valid) {
            THROW(INVALID_STATUS);
        } else if (dry_run) {
            LIXA_TRACE(("lixa_state_log_set_status: dry run, status can be "
                        "switched from %d to %d\n",
                        this->status, new_status));
        } else {
            LIXA_TRACE(("lixa_state_log_set_status: %d -> %d\n",
                        this->status, new_status));
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



