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



#include "lixa_trace.h"
#include "lixa_state_log.h"
#include "lixa_errors.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



int lixa_state_log_init(lixa_state_log_t *this,
                        const char *pathname,
                        int o_direct_bool,
                        int o_dsync_bool,
                        int o_rsync_bool,
                        int o_sync_bool)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_STATUS,
        INTERNAL_ERROR,
        POSIX_MEMALIGN_ERROR,
        OPEN_ERROR,
        CREATE_NEW_FILE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_init: pathname='%s', o_direct_bool=%d, "
                "o_dsync_bool=%d, orsync_bool=%d, osync_bool=%d\n",
                pathname, o_direct_bool, o_dsync_bool, o_rsync_bool,
                o_sync_bool));
    TRY {
        int flags;
        int error;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == pathname)
            THROW(NULL_OBJECT2);
        /* check the state log has not been already used */
        if (UNDEFINED != this->status)
            THROW(INVALID_STATUS);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_log_t));
        /* retrieve system page size */
        if (-1 == (this->page_size = (size_t)sysconf(_SC_PAGESIZE)))
            THROW(INTERNAL_ERROR);
        /* allocate the buffer for I/O */
        this->buffer_size = this->page_size *
            LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT;
        if (0 != (error = posix_memalign(&this->buffer, this->page_size,
                                         this->buffer_size))) {
            LIXA_TRACE(("lixa_state_log_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR);
        }
        memset(this->buffer, 0, this->buffer_size);
        
        /* try to open the already existent file */
        flags = O_RDWR;
        if (o_direct_bool) flags |= O_DIRECT;
        if (o_dsync_bool)  flags |= O_DSYNC;
        if (o_rsync_bool)  flags |= O_RSYNC;
        if (o_sync_bool)   flags |= O_SYNC;
        if (-1 == (this->fd = open(pathname, flags)) && ENOENT != errno)
            THROW(OPEN_ERROR);
        /* create the file if necessary */
        if (-1 == this->fd && ENOENT == errno) {
            LIXA_TRACE(("lixa_state_log_init: pathname '%s' does not exists, "
                        "creating it...\n", pathname));
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_create_new_file(
                                   this, pathname, flags)))
                THROW(CREATE_NEW_FILE_ERROR);
        }
        /* @@@ go on from here */
        
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
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case POSIX_MEMALIGN_ERROR:
                ret_cod = LIXA_RC_POSIX_MEMALIGN_ERROR;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case CREATE_NEW_FILE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_create_new_file(lixa_state_log_t *this,
                                   const char *pathname,
                                   int flags)
{
    enum Exception {
        OPEN_ERROR,
        PWRITE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_create_new_file: pathname='%s', flags=%d\n",
                pathname, flags));
    TRY {
        mode_t mode;
        size_t i;
        /* add O_EXCL and O_CREAT flags and try to open again the file */
        flags |= O_EXCL | O_CREAT;
        /* mode flags (security) */
        mode = S_IRUSR | S_IWUSR | S_IRGRP;
        if (-1 == (this->fd = open(pathname, flags, mode)))
            THROW(OPEN_ERROR);
        /* format the file, review me later @@@ */
        for (i=0; i<LIXA_STATE_LOG_FILE_SIZE_DEFAULT/
                 LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT; ++i) {
            off_t offset = i * this->buffer_size;
            if (this->buffer_size != pwrite(this->fd, this->buffer,
                                            this->buffer_size, offset))
                THROW(PWRITE_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case PWRITE_ERROR:
                ret_cod = LIXA_RC_PWRITE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_create_new_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_log_clean(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_clean\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        if (NULL != this->buffer)  free(this->buffer);
        if (UNDEFINED == this->status) {
            LIXA_TRACE(("lixa_state_log_clean: WARNING, status is "
                        "UNDEFINED!\n"));
        }
        /* reset everything, bye bye... */
        memset(this, 0, sizeof(lixa_state_log_t));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_log_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

