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
#include "lixa_state.h"
#include "lixa_syslog.h"
#include "lixa_trace.h"
#include "lixa_utils.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



const char *LIXA_STATE_TABLE_SUFFIX = ".state";
const char *LIXA_STATE_LOG_SUFFIX = ".log";



int lixa_state_init(lixa_state_t *this, const char *path_prefix,
                    size_t max_buffer_log_size)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        INTERNAL_ERROR,
        MALLOC_ERROR,
        STATE_LOG_INIT_ERROR,
        STATE_TABLE_INIT_ERROR,
        ANALYZE_AND_START,
        COLD_START_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    char *pathname = NULL;
    
    LIXA_TRACE(("lixa_state_init\n"));
    TRY {
        int i;
        size_t pathname_len;
        int state_exists[LIXA_STATE_TABLES];
        int log_exists[LIXA_STATE_TABLES];
        int cold_start = FALSE;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the prefix of the path is not null */
        if (NULL == path_prefix)
            THROW(NULL_OBJECT2);
        if (0 == (pathname_len = strlen(path_prefix)))
            THROW(INVALID_OPTION);
        LIXA_TRACE(("lixa_state_init: path_prefix='%s', "
                    "max_buffer_log_size=" SIZE_T_FORMAT "\n",
                    path_prefix, max_buffer_log_size));
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_t));
        /* retrieve system page size */
        if (-1 == (LIXA_SYSTEM_PAGE_SIZE = (size_t)sysconf(_SC_PAGESIZE)))
            THROW(INTERNAL_ERROR);
        this->max_buffer_log_size = max_buffer_log_size;
        /* allocate a buffer for the file names */
        pathname_len += strlen(LIXA_STATE_TABLE_SUFFIX) +
            strlen(LIXA_STATE_LOG_SUFFIX) + 100;
        if (NULL == (pathname = (char *)malloc(pathname_len)))
            THROW(MALLOC_ERROR);        
        /* initialize state log objects */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_LOG_SUFFIX);
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_init(
                                   &(this->logs[i]), pathname,
                                   max_buffer_log_size,
                                   TRUE, FALSE, FALSE, FALSE)))
                THROW(STATE_LOG_INIT_ERROR);
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_TABLE_SUFFIX);
            /* initialize the first state file */
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_init(
                                   &(this->tables[i]), pathname)))
                THROW(STATE_TABLE_INIT_ERROR);
        }
        /* check for file existence */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            /* check state tables */
            if (LIXA_RC_OK == (ret_cod = lixa_state_table_exist_file(
                                   &(this->tables[i])))) {
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD036I,
                             lixa_state_table_get_pathname(&(this->tables[i]))));
                state_exists[i] = TRUE;
            } else {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD037N,
                             lixa_state_table_get_pathname(&(this->tables[i]))));
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
        if (LIXA_RC_OK != (ret_cod = lixa_state_analyze(
                               this, state_exists, log_exists, &cold_start)))
            THROW(ANALYZE_AND_START);
        
        /* cold start or warm restart */
        if (cold_start) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_cold_start(this)))
                THROW(COLD_START_ERROR);
        } else {
            /* @@@ warm start */
            ;
        }
                    
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
            case STATE_TABLE_INIT_ERROR:
            case ANALYZE_AND_START:
                break;
            case COLD_START_ERROR:
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



int lixa_state_analyze(lixa_state_t *this,
                       const int *state_exists,
                       const int *log_exists,
                       int *cold_start)
{
    enum Exception {
        CORRUPTED_STATUS_FILE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_analyze\n"));
    TRY {
        int i, number_of_states, number_of_logs, number_of_consec_states,
            number_of_consec_logs, previous_state, previous_log;
        /* by default, the state server restart from previous state */
        *cold_start = FALSE;
        /* count the number of state tables and log files */
        number_of_states = number_of_logs = number_of_consec_states =
            number_of_consec_logs = 0;
        previous_state = previous_log = TRUE;
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
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
        if (number_of_consec_states == LIXA_STATE_TABLES &&
            number_of_consec_logs == LIXA_STATE_TABLES-1 &&
            log_exists[0])
            number_of_consec_logs++;
            
        LIXA_TRACE(("lixa_state_analyze: number of table files "
                    "is %d/%d, number of log files is %d/%d\n",
                    number_of_consec_states, number_of_states,
                    number_of_consec_logs, number_of_logs));
        if (0 == number_of_states && 0 == number_of_logs) {
            /* no file exists */
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD038N));
            *cold_start = TRUE;
        } else if (1 == number_of_consec_states &&
                   0 == number_of_consec_logs) {
            /* only first state table is available */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD039W,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            *cold_start = TRUE;
        } else if (2 == number_of_consec_states &&
                   0 == number_of_consec_logs) {
            /* only first and second state tables are available */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD040W,
                         lixa_state_table_get_pathname(&(this->tables[0])),
                         lixa_state_table_get_pathname(&(this->tables[1]))));
            *cold_start = TRUE;
        } else if (
            (number_of_states == number_of_consec_states &&
             number_of_logs == number_of_consec_logs) && (
                 /* state tables are 1 more than log files */
                 (number_of_consec_states == number_of_consec_logs+1) ||
                 /* state tables are 2 more than log files */
                 (number_of_consec_states == number_of_consec_logs+2) ||
                 /* state tables and log files are the same number */
                 (number_of_consec_states == number_of_consec_logs)
                                                           )) {
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD041N,
                         number_of_states, number_of_logs));
        } else {
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD042E));
            THROW(CORRUPTED_STATUS_FILE);
        }
        LIXA_TRACE(("lixa_state_analyze: *cold_start=%d\n", *cold_start));
        
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
    LIXA_TRACE(("lixa_state_analyze/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_cold_start(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        UNLINK_ERROR1,
        TABLE_CREATE_NEW_FILE1,
        TABLE_SYNCHRONIZE,
        TABLE_CLOSE,
        UNLINK_ERROR2,
        TABLE_CREATE_NEW_FILE2,
        UNLINK_ERROR3,
        LOG_CREATE_NEW_FILE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_cold_start\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* create first state table */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD044I,
                     lixa_state_table_get_pathname(&(this->tables[0]))));
        /* remove the old file if it exists */
        if (-1 == unlink(lixa_state_table_get_pathname(&(this->tables[0]))) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR1);
        if (LIXA_RC_OK != (ret_cod = (lixa_state_table_create_new_file(
                                          &(this->tables[0]))))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD043E,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            THROW(TABLE_CREATE_NEW_FILE1);
        }
        if (LIXA_RC_OK != (ret_cod = (lixa_state_table_synchronize(
                                          &(this->tables[0]))))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD045E,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            THROW(TABLE_SYNCHRONIZE);
        }
        if (LIXA_RC_OK != (ret_cod = (lixa_state_table_close(
                                          &(this->tables[0]))))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD046E,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            THROW(TABLE_CLOSE);
        }
        /* create second state table, but don't sync and close it */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD044I,
                     lixa_state_table_get_pathname(&(this->tables[1]))));
        /* remove the old table if it exists */
        if (-1 == unlink(lixa_state_table_get_pathname(&(this->tables[1]))) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR2);
        if (LIXA_RC_OK != (ret_cod = (lixa_state_table_create_new_file(
                                          &(this->tables[1]))))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD043E,
                         lixa_state_table_get_pathname(&(this->tables[1]))));
            THROW(TABLE_CREATE_NEW_FILE2);
        }
        /* create second state log */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD047I,
                     lixa_state_log_get_pathname(&(this->logs[1]))));
        /* remove the old table if it exists */
        if (-1 == unlink(lixa_state_log_get_pathname(&(this->logs[1]))) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR3);
        if (LIXA_RC_OK != (ret_cod = (lixa_state_log_create_new_file(
                                          &(this->logs[1]))))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD048E,
                         lixa_state_table_get_pathname(&(this->tables[1]))));
            THROW(LOG_CREATE_NEW_FILE);
        }
        /* emit a message related to the cold start completion */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD049I,
                     lixa_state_table_get_pathname(&(this->tables[0])),
                     lixa_state_table_get_pathname(&(this->tables[1])),
                     lixa_state_log_get_pathname(&(this->logs[1]))));
        /* set current state table */
        this->used_state_table = 1;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case UNLINK_ERROR1:
            case UNLINK_ERROR2:
            case UNLINK_ERROR3:
                ret_cod = LIXA_RC_UNLINK_ERROR;
                break;
            case TABLE_CREATE_NEW_FILE1:
            case TABLE_CREATE_NEW_FILE2:
            case TABLE_SYNCHRONIZE:
            case TABLE_CLOSE:
            case LOG_CREATE_NEW_FILE:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_cold_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_clean(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        STATE_LOG_CLEAN_ERROR,
        STATE_TABLE_CLEAN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_clean\n"));
    TRY {
        int i;
        if (NULL == this)
            THROW(NULL_OBJECT);

        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_clean(
                                   &(this->logs[i]))))
                THROW(STATE_LOG_CLEAN_ERROR);
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_clean(
                                   &(this->tables[i]))))
                THROW(STATE_TABLE_CLEAN_ERROR);
        } /* for (i=0 */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_LOG_CLEAN_ERROR:
            case STATE_TABLE_CLEAN_ERROR:
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



int lixa_state_check_log_actions(lixa_state_t *this, int *must_flush,
                                 int *must_switch)
{
    enum Exception {
        NULL_OBJECT,
        LOG_CHECK_BUFFER_FULL,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_check_log_actions\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check if flush is requested by the global parameter */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_check_actions(
                               &this->logs[this->used_state_table],
                               must_flush, must_switch)))
            THROW(LOG_CHECK_BUFFER_FULL);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case LOG_CHECK_BUFFER_FULL:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_check_log_actions/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_flush_log_records(lixa_state_t *this,
                                 status_record_t *status_records)
{
    enum Exception {
        NULL_OBJECT,
        STATE_LOG_FLUSH_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_flush_log_records\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixa_state_flush_log_records: used_state_table=%d\n",
                    this->used_state_table));
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_flush(
                               &this->logs[this->used_state_table],
                               status_records)))
            THROW(STATE_LOG_FLUSH_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_LOG_FLUSH_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_flush_log_records/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_mark_block(lixa_state_t *this, uint32_t block_id)
{
    enum Exception {
        NULL_OBJECT,
        STATE_LOG_MARK_BLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_mark_block\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixa_state_mark_block: used_state_table=%d, block_id="
                    UINT32_T_FORMAT "\n", this->used_state_table, block_id));
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_mark_block(
                               &this->logs[this->used_state_table],
                               block_id)))
            THROW(STATE_LOG_MARK_BLOCK_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_LOG_MARK_BLOCK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_mark_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

