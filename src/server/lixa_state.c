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
#include "lixa_syslog.h"



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
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        INTERNAL_ERROR,
        MALLOC_ERROR,
        STATE_LOG_INIT_ERROR,
        STATE_FILE_INIT_ERROR,
        ANALYZE_AND_START,
        CREATE_NEW_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    char *pathname = NULL;
    
    LIXA_TRACE(("lixa_state_init\n"));
    TRY {
        int i;
        size_t pathname_len;
        int state_exists[LIXA_STATE_FILES];
        int log_exists[LIXA_STATE_FILES];
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the prefix of the path is not null */
        if (NULL == path_prefix)
            THROW(NULL_OBJECT2);
        if (0 == (pathname_len = strlen(path_prefix)))
            THROW(INVALID_OPTION);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_t));
        /* retrieve system page size */
        if (-1 == (this->system_page_size = (size_t)sysconf(_SC_PAGESIZE)))
            THROW(INTERNAL_ERROR);
        /* allocate a buffer for the file names */
        pathname_len += strlen(LIXA_STATE_FILE_SUFFIX) +
            strlen(LIXA_STATE_LOG_SUFFIX) + 100;
        if (NULL == (pathname = (char *)malloc(pathname_len)))
            THROW(MALLOC_ERROR);        
        /* initialize state log objects */
        for (i=0; i<LIXA_STATE_FILES; ++i) {
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_LOG_SUFFIX);
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_init(
                                   &(this->logs[i]), pathname,
                                   this->system_page_size,
                                   TRUE, FALSE, FALSE, FALSE)))
                THROW(STATE_LOG_INIT_ERROR);
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_FILE_SUFFIX);
            /* initialize the first state file */
            if (LIXA_RC_OK != (ret_cod = lixa_state_file_init(
                                   &(this->files[i]), pathname)))
                THROW(STATE_FILE_INIT_ERROR);
        }
        /* check for file existence */
        for (i=0; i<LIXA_STATE_FILES; ++i) {
            /* check state files */
            if (LIXA_RC_OK == (ret_cod = lixa_state_file_exist_file(
                                   &(this->files[i])))) {
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD036I,
                             lixa_state_file_get_pathname(&(this->files[i]))));
                state_exists[i] = TRUE;
            } else {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD037N,
                             lixa_state_file_get_pathname(&(this->files[i]))));
                state_exists[i] = FALSE;
            }
            /* check log files */
            if (LIXA_RC_OK == (ret_cod = lixa_state_log_exist_file(
                                   &(this->logs[i])))) {
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD034I,
                             lixa_state_log_get_pathname(&(this->logs[i]))));
                log_exists[i] = TRUE;
            } else {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD035N,
                             lixa_state_log_get_pathname(&(this->logs[i]))));
                log_exists[i] = FALSE;
            }
        }
        /* analyze how many file exists and how to proceed... */
        if (LIXA_RC_OK != (ret_cod = lixa_state_analyze_and_start(
                               this, state_exists, log_exists)))
            THROW(ANALYZE_AND_START);
        
        /* @@@ restart from available files or create new ones... */
        if (LIXA_RC_OK != (ret_cod = lixa_state_create_new(
                               this)))
            THROW(CREATE_NEW_ERROR);
        
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
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case STATE_LOG_INIT_ERROR:
            case STATE_FILE_INIT_ERROR:
            case ANALYZE_AND_START:
                break;
            case CREATE_NEW_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* memory recovery */
        if (NULL != pathname)
            free(pathname);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_analyze_and_start(lixa_state_t *this,
                                 const int *state_exists,
                                 const int *log_exists)
{
    enum Exception {
        CORRUPTED_STATUS_FILE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_analyze_and_start\n"));
    TRY {
        int i, number_of_states, number_of_logs, number_of_consec_states,
            number_of_consec_logs, previous_state, previous_log;
        /* count the number of state files and log files */
        number_of_states = number_of_logs = number_of_consec_states =
            number_of_consec_logs = 0;
        previous_state = previous_log = TRUE;
        for (i=0; i<LIXA_STATE_FILES; ++i) {
            if (state_exists[i])
                number_of_states++;
            if (log_exists[i])
                number_of_logs++;
            if (0 == i) {
                if (previous_state && state_exists[i])
                    number_of_consec_states++;
                else
                    previous_state = FALSE;
            } else {
                if (previous_state && state_exists[i])
                    number_of_consec_states++;
                else
                    previous_state = FALSE;
                if (previous_log && log_exists[i])
                    number_of_consec_logs++;
                else
                    previous_log = FALSE;
            }
        }
        /* fix the number of ordered logs if necessary */
        if (number_of_consec_states == LIXA_STATE_FILES &&
            number_of_consec_logs == LIXA_STATE_FILES-1 &&
            log_exists[0])
            number_of_consec_logs++;
            
        LIXA_TRACE(("lixa_state_analyze_and_start: number of state files "
                    "is %d/%d, number of log files is %d/%d\n",
                    number_of_consec_states, number_of_states,
                    number_of_consec_logs, number_of_logs));
        if (0 == number_of_states && 0 == number_of_logs) {
            /* no file exists */
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD038N));
            /* @@@ attempt cold start*/
        } else if (1 == number_of_consec_states &&
                   0 == number_of_consec_logs) {
            /* only first state file is available */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD039W,
                         lixa_state_file_get_pathname(&(this->files[0]))));
            /* @@@ attempt cold start */
        } else if (2 == number_of_consec_states &&
                   0 == number_of_consec_logs) {
            /* only first and second state files are available */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD040W,
                         lixa_state_file_get_pathname(&(this->files[0])),
                         lixa_state_file_get_pathname(&(this->files[1]))));
            /* @@@ attempt cold start */
        } else if (
            (number_of_states == number_of_consec_states &&
             number_of_logs == number_of_consec_logs) && (
                 /* state files are 1 more than log files */
                 (number_of_consec_states == number_of_consec_logs+1) ||
                 /* state files are 2 more than log files */
                 (number_of_consec_states == number_of_consec_logs+2) ||
                 /* state files and log files are the same number */
                 (number_of_consec_states == number_of_consec_logs)
                                                           )) {
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD041N,
                         number_of_states, number_of_logs));
            /* @@@ attempt warm start */
        } else {
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD042E));
            THROW(CORRUPTED_STATUS_FILE);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CORRUPTED_STATUS_FILE:
                ret_cod = LIXA_RC_CORRUPTED_STATUS_FILE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_analyze_and_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_create_new(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_create_new\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        
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
        int i;
        if (NULL == this)
            THROW(NULL_OBJECT);

        for (i=0; i<LIXA_STATE_FILES; ++i) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_clean(
                                   &(this->logs[0]))))
                THROW(STATE_LOG_CLEAN_ERROR);
            if (LIXA_RC_OK != (ret_cod = lixa_state_file_clean(
                                   &(this->files[0]))))
                THROW(STATE_FILE_CLEAN_ERROR);
        } /* for (i=0 */
        
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

