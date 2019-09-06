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



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_state.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



const char *LIXA_STATE_FILE_SUFFIX = ".state";
const char *LIXA_STATE_LOG_SUFFIX = ".log";



int lixa_state_init(lixa_state_t *this, const char *path_prefix)
{
    enum Exception {
        NULL_OBJECT,
        INTERNAL_ERROR,
        CREATE_NEW_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_init\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_t));
        /* retrieve system page size */
        if (-1 == (this->system_page_size = (size_t)sysconf(_SC_PAGESIZE)))
            THROW(INTERNAL_ERROR);
        
        /* @@@ restart from available files or create new ones... */
        if (LIXA_RC_OK != (ret_cod = lixa_state_create_new(
                               this, path_prefix)))
            THROW(CREATE_NEW_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case CREATE_NEW_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_create_new(lixa_state_t *this, const char *path_prefix)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        MALLOC_ERROR,
        STATE_FILE_INIT_ERROR,
        STATE_LOG_INIT_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    char *tmp_name = NULL;
    
    LIXA_TRACE(("lixa_state_create_new\n"));
    TRY {
        size_t tmp_name_len;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the prefix of the path is not null */
        if (NULL == path_prefix)
            THROW(NULL_OBJECT2);
        if (0 == (tmp_name_len = strlen(path_prefix)))
            THROW(INVALID_OPTION);
        /* allocate a buffer for the file names */
        tmp_name_len += strlen(LIXA_STATE_FILE_SUFFIX) +
            strlen(LIXA_STATE_LOG_SUFFIX) + 100;
        if (NULL == (tmp_name = (char *)malloc(tmp_name_len)))
            THROW(MALLOC_ERROR);
        snprintf(tmp_name, tmp_name_len, "%s_%d%s", path_prefix, 0,
                 LIXA_STATE_FILE_SUFFIX);
        /* initialize the first state file */
        if (LIXA_RC_OK != (ret_cod = lixa_state_file_init(
                               &(this->files[0]), tmp_name)))
            THROW(STATE_FILE_INIT_ERROR);
        /* initialize the first state log */
        snprintf(tmp_name, tmp_name_len, "%s_%d%s", path_prefix, 0,
                 LIXA_STATE_LOG_SUFFIX);
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_init(
                               &(this->logs[0]), tmp_name,
                               this->system_page_size,
                               TRUE, FALSE, FALSE, FALSE)))
            THROW(STATE_LOG_INIT_ERROR);
        
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
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case STATE_FILE_INIT_ERROR:
            case STATE_LOG_INIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* memory recovery */
        if (NULL != tmp_name)
            free(tmp_name);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_create_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_clean(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        STATE_LOG_CLEAN_ERROR,
        STATE_FILE_CLEAN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_clean\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* @@@
           here we need a cycle on all state logs */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_clean(
                               &(this->logs[0]))))
            THROW(STATE_LOG_CLEAN_ERROR);
        if (LIXA_RC_OK != (ret_cod = lixa_state_file_clean(
                               &(this->files[0]))))
            THROW(STATE_FILE_CLEAN_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_LOG_CLEAN_ERROR:
            case STATE_FILE_CLEAN_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

