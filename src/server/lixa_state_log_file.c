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



#ifdef HAVE_UNISTD_H
# include <unistd.h>
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
#include "lixa_state_common.h"
#include "lixa_state_log_file.h"
#include "lixa_trace.h"
#include "lixa_utils.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



const char *LIXA_STATE_LOG_FILE_SUFFIX = ".log";



int lixa_state_log_file_init(lixa_state_log_file_t *this,
                             const char *pathname, int pers_flags)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_STATUS,
        STRDUP_ERROR,
        PTHREAD_MUTEX_INIT_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    
    LIXA_TRACE(("lixa_state_log_file_init\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == pathname)
            THROW(NULL_OBJECT2);
        /* check the state log has not been already used */
        if (STATE_LOG_FILE_UNDEFINED != this->status)
            THROW(INVALID_STATUS);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_log_file_t));
        this->fd = LIXA_NULL_FD;
        /* keep a local copy of the pathname */
        if (NULL == (this->pathname = strdup(pathname)))
            THROW(STRDUP_ERROR);
        this->pers_flags = pers_flags;
        /* initialize the mutex used to protect the synchronized area */
        if (0 != (pte = pthread_mutex_init(&this->synch.mutex, NULL)))
            THROW(PTHREAD_MUTEX_INIT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
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
    LIXA_TRACE(("lixa_state_log_file_init/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_file_clean(lixa_state_log_file_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_DESTROY_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    
    LIXA_TRACE(("lixa_state_log_file_clean\n"));
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
        if (STATE_LOG_FILE_UNDEFINED == this->status) {
            LIXA_TRACE(("lixa_state_log_file_clean: WARNING, status is "
                        "UNDEFINED!\n"));
        }
        if (0 != (pte = pthread_mutex_destroy(&this->synch.mutex)))
            THROW(PTHREAD_MUTEX_DESTROY_ERROR);
            
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
    LIXA_TRACE(("lixa_state_log_file_clean/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_file_exists(lixa_state_log_file_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_file_exists\n"));
    TRY {
        int fd;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* try to open the file with the same flags of a real usage */
        if (-1 == (fd = open(this->pathname, this->pers_flags)))
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
    LIXA_TRACE(("lixa_state_log_file_exists/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_file_create_new(lixa_state_log_file_t *this,
                                   void *single_page)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        PTHREAD_MUTEX_LOCK_ERROR,
        TRUNCATE_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_log_file_create_new\n"));
    TRY {
        mode_t mode;
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* add O_EXCL and O_CREAT flags: file must not exist */
        this->pers_flags |= O_EXCL | O_CREAT;
        /* mode flags (security) */
        mode = S_IRUSR | S_IWUSR | S_IRGRP;
        if (-1 == (this->fd = open(this->pathname, this->pers_flags, mode)))
            THROW(OPEN_ERROR);
        /* obtain the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->synch.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        if (0 == ftruncate(this->fd, LIXA_STATE_LOG_FILE_SIZE_DEFAULT))
            this->synch.total_size = LIXA_STATE_LOG_FILE_SIZE_DEFAULT;
        else
            THROW(TRUNCATE_ERROR);
        /* move the file pointer at the beginning */
        this->synch.offset = 0;
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(&this->synch.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->synch.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_file_create_new/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_file_extend(lixa_state_log_file_t *this)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        PTHREAD_MUTEX_LOCK_ERROR,
        TRUNCATE_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_log_file_extend\n"));
    TRY {
        off_t new_total_size;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check the file descriptor is valid */
        if (LIXA_NULL_FD == this->fd)
            THROW(INVALID_STATUS);
        /* obtain the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->synch.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* compute new size and extend the file */
        new_total_size = this->synch.total_size +
            LIXA_STATE_LOG_FILE_SIZE_INCREMENT;
        if (0 != ftruncate(this->fd, new_total_size))
            THROW(TRUNCATE_ERROR);
        /* update new size */
        this->synch.total_size = new_total_size;
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(&this->synch.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->synch.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_file_extend/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_file_write(lixa_state_log_file_t *this,
                              const void *buffer,
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
    
    LIXA_TRACE(("lixa_state_log_file_write\n"));
    TRY {
        ssize_t written_bytes;
        size_t count = lixa_state_common_pages2buffer(number_of_pages);
        off_t offset;

        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* use the method to get a synchronized value */
        offset = lixa_state_log_file_get_offset(this);
        written_bytes = pwrite(this->fd, buffer, count, offset);
        if (count != written_bytes) {
            LIXA_TRACE(("lixa_state_log_flush: pwrite has written "
                        SSIZE_T_FORMAT " bytes instead of " SIZE_T_FORMAT "\n",
                        written_bytes, count));
            THROW(PWRITE_ERROR);
        } else {
            LIXA_TRACE(("lixa_state_log_flush: written " SIZE_T_FORMAT
                        " pages, " SSIZE_T_FORMAT " bytes to log\n",
                        number_of_pages, written_bytes));
            /* maybe unnecessary, but on some CPU the following operation
             * might be no atomic */
            /* obtain the lock of the synchronized structure */
            if (0 != (pte = pthread_mutex_lock(&this->synch.mutex))) {
                THROW(PTHREAD_MUTEX_LOCK_ERROR);
            } else
                mutex_locked = TRUE;
            this->synch.offset += written_bytes;
            this->synch.reserved -= written_bytes;
            /* unlock the mutex */
            if (0 != (pte = pthread_mutex_unlock(&this->synch.mutex))) {
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
            pthread_mutex_unlock(&this->synch.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_file_write/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_log_file_set_reserved(lixa_state_log_file_t *this,
                                     off_t reserved)
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
    
    LIXA_TRACE(("lixa_state_log_file_set_reserved\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* synchronize on mutex */
        if (0 != (pte = pthread_mutex_lock(&this->synch.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* check consistency */
        if (this->synch.offset + reserved <= this->synch.total_size)
            this->synch.reserved = reserved;
        else
            THROW(OUT_OF_RANGE);
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(&this->synch.mutex))) {
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
            pthread_mutex_unlock(&this->synch.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_file_set_reserved/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}

