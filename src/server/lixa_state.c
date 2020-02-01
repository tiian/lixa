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



#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
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



const char *LIXA_STATE_TABLE_SUFFIX = ".table";



int lixa_state_init(lixa_state_t *this, const char *path_prefix,
                    size_t max_buffer_log_size)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        INTERNAL_ERROR,
        MALLOC_ERROR,
        PTHREAD_MUTEX_INIT_ERROR,
        PTHREAD_COND_INIT_ERROR,
        PTHREAD_CREATE_ERROR,
        STATE_LOG_INIT_ERROR,
        STATE_TABLE_INIT_ERROR,
        ANALYZE_AND_START,
        COLD_START_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
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
        pathname_len += strlen(LIXA_STATE_TABLE_SUFFIX) + 100;
        if (NULL == (pathname = (char *)malloc(pathname_len)))
            THROW(MALLOC_ERROR);
        /* initialize background thread for state table synchronization */
        /* initialize the synchronized internal structure shared with the
           flusher thread */
        if (0 != (pte = pthread_mutex_init(
                      &this->table_synchronizer.mutex, NULL)))
            THROW(PTHREAD_MUTEX_INIT_ERROR);
        if (0 != (pte = pthread_cond_init(
                      &this->table_synchronizer.cond, NULL)))
            THROW(PTHREAD_COND_INIT_ERROR);
        this->table_synchronizer.operation = STATE_FLUSHER_WAIT;
        this->table_synchronizer.file_pos = 0;
        this->table_synchronizer.to_be_flushed = FALSE;
        this->table_synchronizer.table = NULL;
        /* activate flusher thread */
        if (0 != (pte = pthread_create(&this->table_synchronizer.thread, NULL,
                                       lixa_state_async_table_flusher,
                                       (void *)&this->table_synchronizer)))
            THROW(PTHREAD_CREATE_ERROR);
        /* initialize state log object */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_init(
                               &this->log, path_prefix,
                               max_buffer_log_size,
                               TRUE, FALSE, FALSE, FALSE)))
            THROW(STATE_LOG_INIT_ERROR);
        /* initialize state log objects */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            /* initialize the first state file */
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_TABLE_SUFFIX);
            
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
                             lixa_state_table_get_pathname(
                                 &(this->tables[i]))));
                state_exists[i] = TRUE;
            } else {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD037N,
                             lixa_state_table_get_pathname(
                                 &(this->tables[i]))));
                state_exists[i] = FALSE;
            }
            /* check log files */
            if (LIXA_RC_OK == (ret_cod = lixa_state_log_exist_file(
                                   &this->log, i))) {
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD034I,
                             lixa_state_log_get_pathname(&this->log,i)));
                log_exists[i] = TRUE;
            } else {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD035N,
                             lixa_state_log_get_pathname(&this->log,i)));
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
            case PTHREAD_MUTEX_INIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_INIT_ERROR;
                break;
            case PTHREAD_COND_INIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_INIT_ERROR;
                break;
            case PTHREAD_CREATE_ERROR:
                ret_cod = LIXA_RC_PTHREAD_CREATE_ERROR;
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
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
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
        TABLE_MAP,
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
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_create_new_file(
                               &(this->tables[0]),
                               LIXA_STATE_TABLE_INIT_SIZE)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD043E,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            THROW(TABLE_CREATE_NEW_FILE1);
        }
        /* @@@ Is it really useful?!
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_sign(&(this->tables[0]))))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD045E,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            THROW(TABLE_SYNCHRONIZE);
        }
        */
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
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_create_new_file(
                               &(this->tables[1]),
                               LIXA_STATE_TABLE_INIT_SIZE)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD043E,
                         lixa_state_table_get_pathname(&(this->tables[1]))));
            THROW(TABLE_CREATE_NEW_FILE2);
        }
        if (LIXA_RC_OK != (ret_cod = (lixa_state_table_map(
                                          &(this->tables[1]), FALSE)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD057E,
                         lixa_state_table_get_pathname(&(this->tables[1]))));
            THROW(TABLE_MAP);
        }
        /* create second state log */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD047I,
                     lixa_state_log_get_pathname(&this->log,1)));
        /* remove the old table if it exists */
        if (-1 == unlink(lixa_state_log_get_pathname(&this->log,1)) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR3);
        if (LIXA_RC_OK != (ret_cod = (lixa_state_log_create_new_file(
                                          &this->log,1)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD048E,
                         lixa_state_log_get_pathname(&this->log,1)));
            THROW(LOG_CREATE_NEW_FILE);
        }
        /* emit a message related to the cold start completion */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD049I,
                     lixa_state_table_get_pathname(&(this->tables[0])),
                     lixa_state_table_get_pathname(&(this->tables[1])),
                     lixa_state_log_get_pathname(&this->log,1)));
        /* set current files */
        this->used_state_table = 1;
        lixa_state_log_set_used_file(&this->log, 1);
        
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
            case TABLE_MAP:
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
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        PTHREAD_JOIN_ERROR,
        PTHREAD_COND_DESTROY_ERROR,
        PTHREAD_MUTEX_DESTROY_ERROR,
        STATE_LOG_CLEAN_ERROR,
        STATE_TABLE_CLEAN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_clean\n"));
    TRY {
        int i;
        if (NULL == this)
            THROW(NULL_OBJECT);

        /* obtain the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->table_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* ask flusher termination */
        LIXA_TRACE(("lixa_state_clean: sending to flusher thread "
                    "exit request...\n"));
        this->table_synchronizer.operation = STATE_FLUSHER_EXIT;
        if (0 != (pte = pthread_cond_signal(&this->table_synchronizer.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR);
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(
                      &this->table_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        /* wait flusher termination */
        LIXA_TRACE(("lixa_state_clean: waiting flusher thread "
                    "termination...\n"));
        if (0 != (pte = pthread_join(this->table_synchronizer.thread, NULL)))
            THROW(PTHREAD_JOIN_ERROR);
        /* mutex and condition are no more necessary */
        if (0 != (pte = pthread_cond_destroy(&this->table_synchronizer.cond)))
            THROW(PTHREAD_COND_DESTROY_ERROR);
        if (0 != (pte = pthread_mutex_destroy(
                      &this->table_synchronizer.mutex)))
            THROW(PTHREAD_MUTEX_DESTROY_ERROR);
        
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_clean(
                                   &this->log)))
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
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
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
                               &this->log, must_flush, must_switch)))
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



int lixa_state_flush_log_records(lixa_state_t *this)
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
                               &this->log,
                               &this->tables[this->used_state_table])))
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



int lixa_state_extend_log(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        STATE_LOG_FLUSH_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_extend_log\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixa_state_extend_log: used_state_table=%d\n",
                    this->used_state_table));
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_extend(&this->log)))
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
    LIXA_TRACE(("lixa_state_extend_log/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_switch(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        TABLE_CREATE_NEW_FILE,
        SET_STATUS1,
        SET_STATUS2,
        TABLE_COPY_FROM,
        SET_STATUS3,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_switch\n"));
    TRY {
        /* check object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* has next state table to be created and formatted? */
        if (STATE_TABLE_UNDEFINED == lixa_state_table_get_status(
                &this->tables[lixa_state_get_next_state(this)])) {
            if (LIXA_RC_OK != (
                    ret_cod = lixa_state_table_create_new_file(
                        &this->tables[lixa_state_get_next_state(this)],
                        LIXA_STATE_TABLE_INIT_SIZE)))
                THROW(TABLE_CREATE_NEW_FILE);
        }
        /* current state table is switched to COPY_SOURCE status */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_set_status(
                    &this->tables[this->used_state_table],
                    STATE_TABLE_COPY_SOURCE, FALSE)))
            THROW(SET_STATUS1);
        /* next state table is switched to COPY_TARGET status */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_set_status(
                    &this->tables[lixa_state_get_next_state(this)],
                    STATE_TABLE_COPY_TARGET, FALSE)))
            THROW(SET_STATUS2);
        /* copy to target (next) from source (current) */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_copy_from(
                               &this->tables[lixa_state_get_next_state(this)],
                               &this->tables[this->used_state_table])))
            THROW(TABLE_COPY_FROM);
        /* @@@ start disk synchronization of current state file using a
           background thread ... */

        /* next state table is switched to USED status */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_set_status(
                    &this->tables[lixa_state_get_next_state(this)],
                    STATE_TABLE_USED, FALSE)))
            THROW(SET_STATUS3);
        
        /* @@@ switch log file */

        /* @@@ change used_state_table */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case SET_STATUS1:
            case SET_STATUS2:
            case TABLE_COPY_FROM:
            case SET_STATUS3:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_switch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_mark_block(lixa_state_t *this, uint32_t block_id)
{
    enum Exception {
        NULL_OBJECT,
        STATE_LOG_MARK_BLOCK_ERROR,
        STATE_TABLE_SYNC_BLOCK_ERROR,
        CHECK_LOG_ACTIONS_ERROR,
        FLUSH_LOG_RECORDS_ERROR,
        FLUSH_LOG_EXTEND_ERROR,
        SWITCH_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_mark_block(block_id=" UINT32_T_FORMAT ")\n",
                block_id));
    TRY {
        int must_flush = FALSE;
        int must_switch = FALSE;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixa_state_mark_block: used_state_table=%d, block_id="
                    UINT32_T_FORMAT "\n", this->used_state_table, block_id));
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_mark_block(
                               &this->log, block_id)))
            THROW(STATE_LOG_MARK_BLOCK_ERROR);
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_sync_block(
                    &this->tables[this->used_state_table], block_id)))
            THROW(STATE_TABLE_SYNC_BLOCK_ERROR);
        /* check if state log must be flushed */
        if (LIXA_RC_OK != (ret_cod = lixa_state_check_log_actions(
                               this, &must_flush, &must_switch)))
            THROW(CHECK_LOG_ACTIONS_ERROR);
        if (must_flush) {
            LIXA_TRACE(("lixa_state_mark_block: flush records\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_state_flush_log_records(this)))
                THROW(FLUSH_LOG_RECORDS_ERROR);
        }

        if (must_switch) {
            /* is the previous state table in the middle of a sync phase? */
            if (lixa_state_table_is_syncing(
                    &this->tables[lixa_state_get_prev_state(this)])) {
                LIXA_TRACE(("lixa_state_mark_block: extend log file\n"));
                if (LIXA_RC_OK != (ret_cod = lixa_state_extend_log(this)))
                    THROW(FLUSH_LOG_EXTEND_ERROR);
            } else {
                LIXA_TRACE(("lixa_state_mark_block: switch state table and "
                            "log\n"));
                if (LIXA_RC_OK != (ret_cod = lixa_state_switch(this)))
                    THROW(SWITCH_ERROR);
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_LOG_MARK_BLOCK_ERROR:
            case STATE_TABLE_SYNC_BLOCK_ERROR:
            case CHECK_LOG_ACTIONS_ERROR:
            case FLUSH_LOG_RECORDS_ERROR:
            case FLUSH_LOG_EXTEND_ERROR:
            case SWITCH_ERROR:
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



int lixa_state_insert_block(lixa_state_t *this, uint32_t *block_id)
{
    enum Exception {
        NULL_OBJECT,
        G_ARRAY_NEW_ERROR1,
        STATE_TABLE_EXTEND_ERROR,
        MARK_BLOCK_ERROR1,
        G_ARRAY_NEW_ERROR2,
        STATE_TABLE_INSERT_BLOCK_ERROR,
        MARK_BLOCK_ERROR2,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    GArray *changed_block_ids = NULL;
    
    LIXA_TRACE(("lixa_state_insert_block\n"));
    TRY {
        guint i;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* allocate an empty array */
        if (NULL == (changed_block_ids = g_array_new(
                         FALSE, FALSE, sizeof(uint32_t))))
            THROW(G_ARRAY_NEW_ERROR1);
        /* check if the current state table is full */
        if (lixa_state_table_is_full(&this->tables[this->used_state_table])) {
            LIXA_TRACE(("lixa_state_insert_block: current state table (%d) "
                        "is full and must be extended...\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_extend(
                                   &this->tables[this->used_state_table],
                                   changed_block_ids)))
                THROW(STATE_TABLE_EXTEND_ERROR);
            /* mark the changed blocks */
            for (i=0; i<changed_block_ids->len; ++i) {
                if (LIXA_RC_OK != (ret_cod = lixa_state_mark_block(
                                       this, g_array_index(
                                           changed_block_ids, uint32_t, i))))
                    THROW(MARK_BLOCK_ERROR1);
            }
            /* free the array */
            g_array_free(changed_block_ids, TRUE);
            changed_block_ids = NULL;
        }
        /* allocate an empty array */
        if (NULL == (changed_block_ids = g_array_new(
                         FALSE, FALSE, sizeof(uint32_t))))
            THROW(G_ARRAY_NEW_ERROR2);
        /* insert the block in the table currently used */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_insert_block(
                               &this->tables[this->used_state_table],
                               block_id, changed_block_ids)))
            THROW(STATE_TABLE_INSERT_BLOCK_ERROR);
        /* mark the changed blocks */
        for (i=0; i<changed_block_ids->len; ++i) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_mark_block(
                                   this, g_array_index(
                                       changed_block_ids, uint32_t, i))))
                THROW(MARK_BLOCK_ERROR2);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_ARRAY_NEW_ERROR1:
            case G_ARRAY_NEW_ERROR2:
                ret_cod = LIXA_RC_G_ARRAY_NEW_ERROR;
                break;
            case STATE_TABLE_EXTEND_ERROR:
            case MARK_BLOCK_ERROR1:
            case STATE_TABLE_INSERT_BLOCK_ERROR:
            case MARK_BLOCK_ERROR2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover memory */
        if (NULL != changed_block_ids) {
            g_array_free(changed_block_ids, TRUE);
            changed_block_ids = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_insert_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_delete_block(lixa_state_t *this, uint32_t block_id)
{
    enum Exception {
        NULL_OBJECT,
        G_ARRAY_NEW_ERROR,
        STATE_TABLE_DELETE_BLOCK_ERROR,
        MARK_BLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    GArray *changed_block_ids = NULL;
    
    LIXA_TRACE(("lixa_state_delete_block\n"));
    TRY {
        guint i;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* allocate an empty array */
        if (NULL == (changed_block_ids = g_array_new(
                         FALSE, FALSE, sizeof(uint32_t))))
            THROW(G_ARRAY_NEW_ERROR);
        /* insert the block in the table currently used */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_delete_block(
                               &this->tables[this->used_state_table],
                               block_id, changed_block_ids)))
            THROW(STATE_TABLE_DELETE_BLOCK_ERROR);
        /* mark the changed blocks */
        for (i=0; i<changed_block_ids->len; ++i) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_mark_block(
                                   this, g_array_index(
                                       changed_block_ids, uint32_t, i))))
                THROW(MARK_BLOCK_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_ARRAY_NEW_ERROR:
                ret_cod = LIXA_RC_G_ARRAY_NEW_ERROR;
                break;
            case STATE_TABLE_DELETE_BLOCK_ERROR:
            case MARK_BLOCK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover memory */
        if (NULL != changed_block_ids) {
            g_array_free(changed_block_ids, TRUE);
            changed_block_ids = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_delete_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void *lixa_state_async_table_flusher(void *data)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_WAIT_ERROR,
        TABLE_SYNC_MAP_ERROR,
        INTERNAL_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0; /* pthread_error */
    int mutex_locked = FALSE;
    struct lixa_table_synchronizer_s
        *table_synchronizer = (struct lixa_table_synchronizer_s *)data;
    
    LIXA_TRACE(("lixa_state_async_table_flusher\n"));
    TRY {
        int exit = FALSE;

        if (NULL == table_synchronizer)
            THROW(NULL_OBJECT);
        
        while (!exit) {
            LIXA_TRACE(("lixa_state_async_table_flusher: locking "
                        "flusher_mutex...\n"));
            if (0 != (pte = pthread_mutex_lock(
                          &table_synchronizer->mutex))) {
                THROW(PTHREAD_MUTEX_LOCK_ERROR);
            } else
                mutex_locked = TRUE;
            /* check the operation asked by the main thread */
            if (STATE_FLUSHER_WAIT == table_synchronizer->operation) {
                LIXA_TRACE(("lixa_state_async_table_flusher: WAITING on "
                            "condition...\n"));
                if (0 != (pte = pthread_cond_wait(
                              &table_synchronizer->cond,
                              &table_synchronizer->mutex)))
                    THROW(PTHREAD_COND_WAIT_ERROR);
                LIXA_TRACE(("lixa_state_async_table_flusher: condition has "
                            "been signaled\n"));
            }
            if (STATE_FLUSHER_FLUSH == table_synchronizer->operation) {
                /* flush the data to file */
                if (table_synchronizer->to_be_flushed) {
                    if (LIXA_RC_OK != (ret_cod = lixa_state_table_sync_map(
                                           table_synchronizer->table)))
                        THROW(TABLE_SYNC_MAP_ERROR);
                } else {
                    /* this is an internal error, it should never happen */
                    LIXA_TRACE(("lixa_state_async_table_flusher: internal "
                                "error, flusher.operation=%d and "
                                "flusher.to_be_flushed=%d\n",
                                table_synchronizer->operation,
                                table_synchronizer->to_be_flushed));
                    THROW(INTERNAL_ERROR);
                }
                /* reset flushing condition */
                table_synchronizer->to_be_flushed = FALSE;
                table_synchronizer->operation = STATE_FLUSHER_WAIT;
                /* signaling to master thread */
                if (0 != (pte = pthread_cond_signal(
                              &table_synchronizer->cond)))
                    THROW(PTHREAD_COND_SIGNAL_ERROR);
            }
            if (STATE_FLUSHER_EXIT == table_synchronizer->operation) {
                LIXA_TRACE(("lixa_state_async_table_flusher: EXITING\n"));
                exit = TRUE;
            }
            /* this should never happen */
            if (STATE_FLUSHER_WAIT != table_synchronizer->operation &&
                STATE_FLUSHER_FLUSH != table_synchronizer->operation &&
                STATE_FLUSHER_EXIT != table_synchronizer->operation) {
                LIXA_TRACE(("lixa_state_async_table_flusher: internal error "
                            "flusher_operation=%d\n",
                            table_synchronizer->operation));
                exit = TRUE;
            }
            /* unlock the mutex */
            LIXA_TRACE(("lixa_state_async_table_flusher: unlocking "
                        "flusher_mutex\n"));
            if (0 != (pte = pthread_mutex_unlock(
                          &table_synchronizer->mutex))) {
                THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
            } else
                mutex_locked = FALSE;
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
            case TABLE_SYNC_MAP_ERROR:
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
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&table_synchronizer->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_async_table_flusher/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return NULL;
}



