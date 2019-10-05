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



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



int lixa_state_log_init(lixa_state_log_t *this,
                        const char *pathname,
                        size_t system_page_size,
                        int o_direct_bool,
                        int o_dsync_bool,
                        int o_rsync_bool,
                        int o_sync_bool)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_STATUS,
        BUFFER_OVERFLOW,
        POSIX_MEMALIGN_ERROR1,
        POSIX_MEMALIGN_ERROR2,
        STRDUP_ERROR,
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
        int error;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == pathname)
            THROW(NULL_OBJECT2);
        /* check the state log has not been already used */
        if (STATE_LOG_UNDEFINED != this->status)
            THROW(INVALID_STATUS);
        /* check the system page is not too small... */
        if (0 == (system_page_size / sizeof(struct lixa_state_log_record_s)))
            THROW(BUFFER_OVERFLOW);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_log_t));
        /* save a copy of the (constant) value in the object */
        this->system_page_size = system_page_size;
        /* allocate the single memory page */
        if (0 != (error = posix_memalign(&this->single_page, system_page_size,
                                         system_page_size))) {
            LIXA_TRACE(("lixa_state_log_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR1);
        }
        memset(this->single_page, 0, system_page_size);
        /* allocate the buffer for I/O */
        this->buffer_size = system_page_size *
            LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT;
        if (0 != (error = posix_memalign(&this->buffer, system_page_size,
                                         this->buffer_size))) {
            LIXA_TRACE(("lixa_state_log_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR2);
        }
        memset(this->buffer, 0, this->buffer_size);
        /* keep a local copy of the pathname */
        if (NULL == (this->pathname = strdup(pathname)))
            THROW(STRDUP_ERROR);
        
        /* try to open the already existent file */
        this->pers_flags = O_RDWR;
        if (o_direct_bool) this->pers_flags |= O_DIRECT;
        if (o_dsync_bool)  this->pers_flags |= O_DSYNC;
        if (o_rsync_bool)  this->pers_flags |= O_RSYNC;
        if (o_sync_bool)   this->pers_flags |= O_SYNC;
        
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
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case POSIX_MEMALIGN_ERROR1:
            case POSIX_MEMALIGN_ERROR2:
                ret_cod = LIXA_RC_POSIX_MEMALIGN_ERROR;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
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



int lixa_state_log_create_new_file(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        PWRITE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_create_new_file\n"));
    TRY {
        mode_t mode;
        size_t i;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* add O_EXCL and O_CREAT flags and try to open again the file */
        this->pers_flags |= O_EXCL | O_CREAT;
        /* mode flags (security) */
        mode = S_IRUSR | S_IWUSR | S_IRGRP;
        if (-1 == (this->fd = open(this->pathname, this->pers_flags, mode)))
            THROW(OPEN_ERROR);
        /* format the file, review me later @@@ */
        memset(this->single_page, 0, this->system_page_size);
        for (i=0; i<LIXA_STATE_LOG_FILE_SIZE_DEFAULT; ++i) {
            off_t offset = i * this->system_page_size;
            if (this->system_page_size != pwrite(
                    this->fd, this->single_page,
                    this->system_page_size, offset))
                THROW(PWRITE_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
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



int lixa_state_log_exist_file(lixa_state_log_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_exist_file\n"));
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
    LIXA_TRACE(("lixa_state_log_exist_file/excp=%d/"
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
        if (NULL != this->pathname)
            free(this->pathname);
        if (NULL != this->buffer)
            free(this->buffer);
        if (NULL != this->single_page)
            free(this->single_page);
        if (STATE_LOG_UNDEFINED == this->status) {
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



int lixa_state_log_flush(lixa_state_log_t *this,
                         status_record_t *status_records,
                         GTree *updated_records,
                         int number_of_updated_records)
{
    enum Exception {
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_log_flush\n"));
    TRY {
        size_t number_of_records_per_page;
        size_t number_of_pages;
        size_t number_of_buffers;
        size_t number_of_pages_per_buffer, b;
        
        /* compute the number of records per page */
        number_of_records_per_page = this->system_page_size /
            sizeof(struct lixa_state_log_record_s);
        /* compute the number of buffer pages */
        number_of_pages =
            (number_of_updated_records / number_of_records_per_page) +
            (number_of_updated_records %
             number_of_records_per_page != 0 ? 1 : 0);
        /* compute the number of buffers */
        number_of_pages_per_buffer =
            this->buffer_size / this->system_page_size;
        number_of_buffers =
            (number_of_pages / number_of_pages_per_buffer) +
            (number_of_pages % number_of_pages_per_buffer != 0 ? 1 : 0);
        LIXA_TRACE(("lixa_state_log_flush: "
                    "number_of_updated_records=%d, "
                    "number_of_records_per_page=" SIZE_T_FORMAT ", "
                    "number_of_pages=" SIZE_T_FORMAT ", "
                    "number_of_pages_per_buffer=" SIZE_T_FORMAT ", "
                    "number_of_buffers=" SIZE_T_FORMAT "\n",
                    number_of_updated_records, number_of_records_per_page,
                    number_of_pages, number_of_pages_per_buffer,
                    number_of_buffers));
        /* loop on the number of buffers */
        for (b=0; b<number_of_buffers; ++b) {
            int full_buffer = FALSE;
            /* full or partial buffer will be used in this cycle? */
            if (number_of_pages >= number_of_pages_per_buffer)
                full_buffer = TRUE;
            /* reset the buffer @@@ */
            
            
        } /* for (b=0; b<number_of_buffers; ++b) */
        
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
    LIXA_TRACE(("lixa_state_log_flush/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

