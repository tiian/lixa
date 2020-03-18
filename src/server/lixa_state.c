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



int lixa_state_init(lixa_state_t *this, const char *path_prefix,
                    size_t max_buffer_log_size, int read_only)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_OPTION,
        INTERNAL_ERROR,
        BUFFER_OVERFLOW,
        POSIX_MEMALIGN_ERROR1,
        MALLOC_ERROR1,
        PTHREAD_MUTEX_INIT_ERROR1,
        PTHREAD_COND_INIT_ERROR1,
        PTHREAD_CREATE_ERROR1,
        PTHREAD_MUTEX_INIT_ERROR2,
        PTHREAD_COND_INIT_ERROR2,
        POSIX_MEMALIGN_ERROR2,
        PTHREAD_CREATE_ERROR2,
        MALLOC_ERROR2,
        STATE_LOG_INIT_ERROR,
        STATE_TABLE_INIT_ERROR,
        ANALYZE_AND_START,
        COLD_START_ERROR,
        WARM_START_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    char *pathname = NULL;
    
    LIXA_TRACE(("lixa_state_init\n"));
    TRY {
        int error, i;
        size_t pathname_len;
        int table_exists[LIXA_STATE_TABLES];
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
            LIXA_TRACE(("lixa_state_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR1);
        }
        memset(this->single_page, 0, LIXA_SYSTEM_PAGE_SIZE);
        /* allocate a buffer for the file names */
        pathname_len += strlen(LIXA_STATE_TABLE_SUFFIX) + 100;
        if (NULL == (pathname = (char *)malloc(pathname_len)))
            THROW(MALLOC_ERROR1);
        /* initial size of the buffer */
        this->size_of_block_ids = lixa_state_log_buffer2blocks(
            LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT);
        /* initialize background thread for state table synchronization */
        /* initialize the synchronized internal structure shared with the
           flusher thread */
        if (0 != (pte = pthread_mutex_init(
                      &this->table_synchronizer.mutex, NULL)))
            THROW(PTHREAD_MUTEX_INIT_ERROR1);
        if (0 != (pte = pthread_cond_init(
                      &this->table_synchronizer.cond, NULL)))
            THROW(PTHREAD_COND_INIT_ERROR1);
        this->table_synchronizer.operation = STATE_FLUSHER_WAIT;
        this->table_synchronizer.to_be_flushed = FALSE;
        this->table_synchronizer.table = NULL;
        /* activate flusher thread */
        if (0 != (pte = pthread_create(&this->table_synchronizer.thread, NULL,
                                       lixa_state_async_table_flusher,
                                       (void *)&this->table_synchronizer)))
            THROW(PTHREAD_CREATE_ERROR1);
        /* initialize background thread for state log synchronization */
        /* initialize the synchronized internal structure shared with the
           flusher thread */
        if (0 != (pte = pthread_mutex_init(
                      &this->log_synchronizer.mutex, NULL)))
            THROW(PTHREAD_MUTEX_INIT_ERROR2);
        if (0 != (pte = pthread_cond_init(&this->log_synchronizer.cond, NULL)))
            THROW(PTHREAD_COND_INIT_ERROR2);
        this->log_synchronizer.operation = STATE_FLUSHER_WAIT;
        this->log_synchronizer.to_be_flushed = FALSE;
        this->log_synchronizer.log = NULL;
        this->log_synchronizer.number_of_pages = 0;
        /* allocate the buffer for I/O */
        this->log_synchronizer.buffer_size = lixa_state_log_blocks2buffer(
            this->size_of_block_ids);
        if (0 != (error = posix_memalign(
                      &this->log_synchronizer.buffer,
                      LIXA_SYSTEM_PAGE_SIZE,
                      this->log_synchronizer.buffer_size))) {
            LIXA_TRACE(("lixa_state_init/posix_memalign: error=%d\n",
                        error));
            THROW(POSIX_MEMALIGN_ERROR2);
        } else {
            LIXA_TRACE(("lixa_state_init: size_of_block_ids="
                        SIZE_T_FORMAT ", "
                        "number_of_records_per_page= " SIZE_T_FORMAT ", "
                        "buffer_size= " SIZE_T_FORMAT "\n",
                        this->size_of_block_ids,
                        LIXA_STATE_LOG_RECORDS_PER_PAGE,
                        this->log_synchronizer.buffer_size));
        }
        memset(this->log_synchronizer.buffer, 0,
               this->log_synchronizer.buffer_size);
        /* activate flusher thread */
        if (0 != (pte = pthread_create(&this->log_synchronizer.thread, NULL,
                                       lixa_state_async_log_flusher,
                                       (void *)&this->log_synchronizer)))
            THROW(PTHREAD_CREATE_ERROR2);
        
        /* check max buffer size, resize it if necessary */
        if (max_buffer_log_size < LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT) {
            LIXA_TRACE(("lixa_state_init: max_buffer_size ("
                        SIZE_T_FORMAT ") is less then "
                        "default (" SIZE_T_FORMAT "), using default\n",
                        max_buffer_log_size,
                        LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT));
            max_buffer_log_size = LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT;
        }
        /* max number of records in buffer, config limit */
        this->max_number_of_block_ids = lixa_state_log_buffer2blocks(
            max_buffer_log_size);
        /* buffer is empty */
        this->number_of_block_ids = 0;
        /* allocate the array for block_ids */
        if (NULL == (this->block_ids = (uint32_t *)malloc(
                         sizeof(uint32_t) * this->size_of_block_ids)))
            THROW(MALLOC_ERROR2);
        memset(this->block_ids, 0, sizeof(uint32_t) * this->size_of_block_ids);
        /* initialize state log objects */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            /* initialize the state logs */
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_LOG_FILE_SUFFIX);
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_init(
                                   &(this->logs[i]), pathname,
                                   max_buffer_log_size, read_only,
                                   TRUE, FALSE, FALSE, FALSE)))
                THROW(STATE_LOG_INIT_ERROR);
            /* initialize the state tables */
            snprintf(pathname, pathname_len, "%s_%d%s", path_prefix, i,
                     LIXA_STATE_TABLE_SUFFIX);
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_init(
                                   &(this->tables[i]), pathname, read_only)))
                THROW(STATE_TABLE_INIT_ERROR);
        }
        /* check for file existence */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            /* check state tables */
            if (LIXA_RC_OK == (ret_cod = lixa_state_table_file_exist(
                                   &(this->tables[i])))) {
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD036I,
                             lixa_state_table_get_pathname(
                                 &(this->tables[i]))));
                table_exists[i] = TRUE;
            } else {
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD037N,
                             lixa_state_table_get_pathname(
                                 &(this->tables[i]))));
                table_exists[i] = FALSE;
            }
            /* check log files */
            if (LIXA_RC_OK == (ret_cod = lixa_state_log_file_exist(
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
                               this, table_exists, log_exists, &cold_start)))
            THROW(ANALYZE_AND_START);
        
        /* cold start or warm restart */
        if (cold_start) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_cold_start(this)))
                THROW(COLD_START_ERROR);
        } else {
            if (LIXA_RC_OK != (ret_cod = lixa_state_warm_start(
                                   this, table_exists, log_exists, FALSE)))
                THROW(WARM_START_ERROR);
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
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case POSIX_MEMALIGN_ERROR1:
                ret_cod = LIXA_RC_POSIX_MEMALIGN_ERROR;
                break;
            case MALLOC_ERROR1:
            case MALLOC_ERROR2:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case PTHREAD_MUTEX_INIT_ERROR1:
            case PTHREAD_MUTEX_INIT_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_INIT_ERROR;
                break;
            case PTHREAD_COND_INIT_ERROR1:
            case PTHREAD_COND_INIT_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_COND_INIT_ERROR;
                break;
            case PTHREAD_CREATE_ERROR1:
            case PTHREAD_CREATE_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_CREATE_ERROR;
                break;
            case STATE_LOG_INIT_ERROR:
            case STATE_TABLE_INIT_ERROR:
            case ANALYZE_AND_START:
            case COLD_START_ERROR:
            case WARM_START_ERROR:
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
        if (NONE != excp) { 
            if (excp > POSIX_MEMALIGN_ERROR1 && NULL != this->single_page) {
                free(this->single_page);
                this->single_page = NULL;
            }
            if (excp > MALLOC_ERROR2 && NULL != this->block_ids) {
                free(this->block_ids);
                this->block_ids = NULL;
            }
            if (excp > POSIX_MEMALIGN_ERROR2 &&
                NULL != this->log_synchronizer.buffer) {
                free(this->log_synchronizer.buffer);
                this->log_synchronizer.buffer = NULL;
            }
        } /* if (NONE != excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_init/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_analyze(lixa_state_t *this,
                       const int *table_exists,
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
        int i, number_of_tables, number_of_logs, number_of_consec_tables,
            number_of_consec_logs, previous_table, previous_log;
        /* by default, the state server restart from previous state */
        *cold_start = FALSE;
        /* count the number of state tables and log files */
        number_of_tables = number_of_logs = number_of_consec_tables =
            number_of_consec_logs = 0;
        previous_table = previous_log = TRUE;
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            if (table_exists[i])
                number_of_tables++;
            if (log_exists[i])
                number_of_logs++;
            if (0 == i) {
                if (previous_table && table_exists[i])
                    number_of_consec_tables++;
                else
                    previous_table = FALSE;
            } else {
                if (previous_table && table_exists[i])
                    number_of_consec_tables++;
                else
                    previous_table = FALSE;
                if (previous_log && log_exists[i])
                    number_of_consec_logs++;
                else
                    previous_log = FALSE;
            }
        }
        /* fix the number of ordered logs if necessary */
        if (number_of_consec_tables == LIXA_STATE_TABLES &&
            number_of_consec_logs == LIXA_STATE_TABLES-1 &&
            log_exists[0])
            number_of_consec_logs++;
            
        LIXA_TRACE(("lixa_state_analyze: number of table files "
                    "is %d/%d, number of log files is %d/%d\n",
                    number_of_consec_tables, number_of_tables,
                    number_of_consec_logs, number_of_logs));
        if (0 == number_of_tables && 0 == number_of_logs) {
            /* no file exists */
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD038N));
            *cold_start = TRUE;
        } else if (1 == number_of_consec_tables &&
                   0 == number_of_consec_logs) {
            /* only first state table is available */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD039W,
                         lixa_state_table_get_pathname(&(this->tables[0]))));
            *cold_start = TRUE;
        } else if (2 == number_of_consec_tables &&
                   0 == number_of_consec_logs) {
            /* only first and second state tables are available */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD040W,
                         lixa_state_table_get_pathname(&(this->tables[0])),
                         lixa_state_table_get_pathname(&(this->tables[1]))));
            *cold_start = TRUE;
        } else if (
            (number_of_tables == number_of_consec_tables &&
             number_of_logs == number_of_consec_logs) && (
                 /* state tables are 1 more than log files */
                 (number_of_consec_tables == number_of_consec_logs+1) ||
                 /* state tables are 2 more than log files */
                 (number_of_consec_tables == number_of_consec_logs+2) ||
                 /* state tables and log files are the same number */
                 (number_of_consec_tables == number_of_consec_logs)
                                                           )) {
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD041N,
                         number_of_tables, number_of_logs));
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
                     lixa_state_table_get_pathname(&this->tables[0])));
        /* remove the old file if it exists */
        if (-1 == unlink(lixa_state_table_get_pathname(&this->tables[0])) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR1);
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_create_new_file(
                               &this->tables[0],
                               LIXA_STATE_TABLE_INIT_SIZE)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD043E,
                         lixa_state_table_get_pathname(&this->tables[0])));
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
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_close(&this->tables[0])))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD046E,
                         lixa_state_table_get_pathname(&this->tables[0])));
            THROW(TABLE_CLOSE);
        }
        /* create second state table, but don't sync and close it */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD044I,
                     lixa_state_table_get_pathname(&this->tables[1])));
        /* remove the old table if it exists */
        if (-1 == unlink(lixa_state_table_get_pathname(&this->tables[1])) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR2);
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_create_new_file(
                               &this->tables[1],
                               LIXA_STATE_TABLE_INIT_SIZE)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD043E,
                         lixa_state_table_get_pathname(&this->tables[1])));
            THROW(TABLE_CREATE_NEW_FILE2);
        }
        if (LIXA_RC_OK != (
                ret_cod = (lixa_state_table_map(&this->tables[1], FALSE)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD057E,
                         lixa_state_table_get_pathname(&this->tables[1])));
            THROW(TABLE_MAP);
        }
        /* create second state log */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD047I,
                     lixa_state_log_get_pathname(&this->logs[1])));
        /* remove the old log if it exists */
        if (-1 == unlink(lixa_state_log_get_pathname(&this->logs[1])) &&
            errno != ENOENT)
            THROW(UNLINK_ERROR3);
        if (LIXA_RC_OK != (ret_cod = (
                               lixa_state_log_create_new_file(
                                   &this->logs[1], this->single_page)))) {
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD048E,
                         lixa_state_log_get_pathname(&this->logs[1])));
            THROW(LOG_CREATE_NEW_FILE);
        }
        /* emit a message related to the cold start completion */
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD049I,
                     lixa_state_table_get_pathname(&this->tables[0]),
                     lixa_state_table_get_pathname(&this->tables[1]),
                     lixa_state_log_get_pathname(&this->logs[1])));
        /* set current files */
        this->active_state = 1;
        
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



int lixa_state_warm_start(lixa_state_t *this,
                          const int *table_exists,
                          const int *log_exists,
                          int read_only)
{
    enum Exception {
        NULL_OBJECT,
        TABLE_OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_warm_start\n"));
    TRY {
        int i;

        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* open all the available files */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            if (table_exists[i])
                if (LIXA_RC_OK != (ret_cod = lixa_state_table_open_file(
                                       &this->tables[i])))
                    THROW(TABLE_OPEN_ERROR);
            if (log_exists[i])
                if (LIXA_RC_OK != (ret_cod = lixa_state_log_open_file(
                                       &this->logs[i])))
                    THROW(TABLE_OPEN_ERROR);
        } /* for (i=0; i<LIXA_STATE_TABLES; ++i) */

        /* @@@ analyze the tables and the logs */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TABLE_OPEN_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_warm_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_close(lixa_state_t *this)
{
    enum Exception {
        FLUSH_TABLE,
        PTHREAD_MUTEX_LOCK_ERROR1,
        PTHREAD_COND_WAIT_ERROR1,
        PTHREAD_COND_SIGNAL_ERROR1,
        PTHREAD_MUTEX_UNLOCK_ERROR1,
        PTHREAD_JOIN_ERROR1,
        PTHREAD_COND_DESTROY_ERROR1,
        PTHREAD_MUTEX_DESTROY_ERROR1,
        FLUSH_LOG_RECORDS,
        PTHREAD_MUTEX_LOCK_ERROR2,
        PTHREAD_COND_WAIT_ERROR2,
        PTHREAD_COND_SIGNAL_ERROR2,
        PTHREAD_MUTEX_UNLOCK_ERROR2,
        PTHREAD_JOIN_ERROR2,
        PTHREAD_COND_DESTROY_ERROR2,
        PTHREAD_MUTEX_DESTROY_ERROR2,
        TABLE_CLOSE,
        LOG_CLOSE,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_close\n"));
    TRY {
        /* close all the objects starting from the current one in use */
        int saved_active_state = this->active_state;

        do {
            LIXA_TRACE(("lixa_state_close: closing state table and log "
                        "number %d\n", this->active_state));
            if (this->active_state == saved_active_state) {
                /*
                 * STATE TABLE SECTION
                 */
                /* flush current state table */
                if (LIXA_RC_OK != (ret_cod = lixa_state_flush_table(this)))
                    THROW(FLUSH_TABLE);
                /* obtain the lock of the synchronized structure */
                if (0 != (pte = pthread_mutex_lock(
                              &this->table_synchronizer.mutex))) {
                    THROW(PTHREAD_MUTEX_LOCK_ERROR1);
                } else
                    mutex_locked = TRUE;
                /* this synchronization is necessary to avoid overlapping a
                   previous flusher execution, under normal condition it must
                   be very fast */
                if (this->table_synchronizer.to_be_flushed) {
                    lixa_timer_t timer;
                    long duration;
            
                    LIXA_TRACE(("lixa_state_close: WAITING on "
                                "condition for table flusher thread...\n"));
                    lixa_timer_start(&timer);
                    if (0 != (pte = pthread_cond_wait(
                                  &this->table_synchronizer.cond,
                                  &this->table_synchronizer.mutex)))
                        THROW(PTHREAD_COND_WAIT_ERROR1);
                    lixa_timer_stop(&timer);
                    duration = lixa_timer_get_diff(&timer);
                    LIXA_TRACE(("lixa_state_close: condition has been "
                                "signaled, total wait time is %ld us\n",
                                duration));
                    /* transform microseconds to milliseconds */
                    duration /= 1000;
                    if (duration > 0)
                        LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD058N,
                                     duration));
                }
                /* ask flusher termination */
                LIXA_TRACE(("lixa_state_close: sending to table flusher "
                            "thread exit request...\n"));
                this->table_synchronizer.operation = STATE_FLUSHER_EXIT;
                if (0 != (pte = pthread_cond_signal(
                              &this->table_synchronizer.cond)))
                    THROW(PTHREAD_COND_SIGNAL_ERROR1);
                /* unlock the mutex */
                if (0 != (pte = pthread_mutex_unlock(
                              &this->table_synchronizer.mutex))) {
                    THROW(PTHREAD_MUTEX_UNLOCK_ERROR1);
                } else
                    mutex_locked = FALSE;
                /* wait flusher termination */
                if (0 != (pte = pthread_join(this->table_synchronizer.thread,
                                             NULL)))
                    THROW(PTHREAD_JOIN_ERROR1);
                LIXA_TRACE(("lixa_state_close: table flusher thread "
                            "terminated\n"));
                /* mutex and condition are no more necessary */
                if (0 != (pte = pthread_cond_destroy(
                              &this->table_synchronizer.cond)))
                    THROW(PTHREAD_COND_DESTROY_ERROR1);
                if (0 != (pte = pthread_mutex_destroy(
                              &this->table_synchronizer.mutex)))
                    THROW(PTHREAD_MUTEX_DESTROY_ERROR1);
                /*
                 * STATE LOG SECTION
                 */
                /* flush current state log */
                if (LIXA_RC_OK != (ret_cod =
                                   lixa_state_flush_log_records(this)))
                    THROW(FLUSH_LOG_RECORDS);
                /* obtain the lock of the synchronized structure */
                if (0 != (pte = pthread_mutex_lock(
                              &this->log_synchronizer.mutex))) {
                    THROW(PTHREAD_MUTEX_LOCK_ERROR2);
                } else
                    mutex_locked = TRUE;
                /* this synchronization is necessary to avoid overlapping a
                   previous flusher execution, under normal condition it must
                   be very fast */
                if (this->log_synchronizer.to_be_flushed) {
                    lixa_timer_t timer;
                    long duration;
            
                    LIXA_TRACE(("lixa_state_close: WAITING on "
                                "condition for log flusher thread...\n"));
                    lixa_timer_start(&timer);
                    if (0 != (pte = pthread_cond_wait(
                                  &this->log_synchronizer.cond,
                                  &this->log_synchronizer.mutex)))
                        THROW(PTHREAD_COND_WAIT_ERROR1);
                    lixa_timer_stop(&timer);
                    duration = lixa_timer_get_diff(&timer);
                    LIXA_TRACE(("lixa_state_close: condition has been "
                                "signaled, total wait time is %ld us\n",
                                duration));
                    /* transform microseconds to milliseconds */
                    duration /= 1000;
                    if (duration > 0)
                        LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD058N,
                                     duration));
                }
                /* ask flusher termination */
                LIXA_TRACE(("lixa_state_close: sending to log flusher "
                            "thread exit request...\n"));
                this->log_synchronizer.operation = STATE_FLUSHER_EXIT;
                if (0 != (pte = pthread_cond_signal(
                              &this->log_synchronizer.cond)))
                    THROW(PTHREAD_COND_SIGNAL_ERROR2);
                /* unlock the mutex */
                if (0 != (pte = pthread_mutex_unlock(
                              &this->log_synchronizer.mutex))) {
                    THROW(PTHREAD_MUTEX_UNLOCK_ERROR2);
                } else
                    mutex_locked = FALSE;
                /* wait flusher termination */
                if (0 != (pte = pthread_join(this->log_synchronizer.thread,
                                             NULL)))
                    THROW(PTHREAD_JOIN_ERROR2);
                LIXA_TRACE(("lixa_state_close: log flusher thread "
                            "terminated\n"));
                /* mutex and condition are no more necessary */
                if (0 != (pte = pthread_cond_destroy(
                              &this->log_synchronizer.cond)))
                    THROW(PTHREAD_COND_DESTROY_ERROR2);
                if (0 != (pte = pthread_mutex_destroy(
                              &this->log_synchronizer.mutex)))
                    THROW(PTHREAD_MUTEX_DESTROY_ERROR2);
            } /* this->active_state == saved_active_state */
            /* closing the state table */
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_close(
                                   &this->tables[this->active_state])))
                THROW(TABLE_CLOSE);
            /* closing the state log */
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_close(
                                   &this->logs[this->active_state])))
                THROW(LOG_CLOSE);
            /* go to next table and log */
            this->active_state = lixa_state_get_next_state(this);
        } while (this->active_state != saved_active_state);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case FLUSH_TABLE:
            case FLUSH_LOG_RECORDS:
                break;
            case PTHREAD_MUTEX_LOCK_ERROR1:
            case PTHREAD_MUTEX_LOCK_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case PTHREAD_COND_WAIT_ERROR1:
            case PTHREAD_COND_WAIT_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_COND_WAIT_ERROR;
                break;
            case PTHREAD_COND_SIGNAL_ERROR1:
            case PTHREAD_COND_SIGNAL_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_COND_SIGNAL_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR1:
            case PTHREAD_MUTEX_UNLOCK_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case PTHREAD_JOIN_ERROR1:
            case PTHREAD_JOIN_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_JOIN_ERROR;
                break;
            case PTHREAD_COND_DESTROY_ERROR1:
            case PTHREAD_COND_DESTROY_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_COND_DESTROY_ERROR;
                break;
            case PTHREAD_MUTEX_DESTROY_ERROR1:
            case PTHREAD_MUTEX_DESTROY_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_DESTROY_ERROR;
                break;
            case TABLE_CLOSE:
            case LOG_CLOSE:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore mutex in the event of error */
        if (mutex_locked &&
            excp > PTHREAD_MUTEX_LOCK_ERROR1 &&
            excp < PTHREAD_MUTEX_UNLOCK_ERROR1)
            pthread_mutex_unlock(&this->table_synchronizer.mutex);
        if (mutex_locked &&
            excp > PTHREAD_MUTEX_LOCK_ERROR2 &&
            excp < PTHREAD_MUTEX_UNLOCK_ERROR2)
            pthread_mutex_unlock(&this->log_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_close/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_clean(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR2,
        PTHREAD_COND_SIGNAL_ERROR2,
        PTHREAD_MUTEX_UNLOCK_ERROR2,
        PTHREAD_JOIN_ERROR2,
        PTHREAD_COND_DESTROY_ERROR2,
        PTHREAD_MUTEX_DESTROY_ERROR2,
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
        if (0 != (pte = pthread_mutex_lock(&this->log_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR2);
        } else
            mutex_locked = TRUE;
        /* ask flusher termination */
        LIXA_TRACE(("lixa_state_clean: sending to log flusher thread "
                    "exit request...\n"));
        this->log_synchronizer.operation = STATE_FLUSHER_EXIT;
        if (0 != (pte = pthread_cond_signal(&this->log_synchronizer.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR2);
        /* unlock the mutex */
        if (0 != (pte = pthread_mutex_unlock(&this->log_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR2);
        } else
            mutex_locked = FALSE;
        /* wait flusher termination */
        if (0 != (pte = pthread_join(this->log_synchronizer.thread, NULL)))
            THROW(PTHREAD_JOIN_ERROR2);
        LIXA_TRACE(("lixa_state_clean: log flusher thread terminated\n"));
        /* mutex and condition are no more necessary */
        if (0 != (pte = pthread_cond_destroy(&this->log_synchronizer.cond)))
            THROW(PTHREAD_COND_DESTROY_ERROR2);
        if (0 != (pte = pthread_mutex_destroy(&this->log_synchronizer.mutex)))
            THROW(PTHREAD_MUTEX_DESTROY_ERROR2);
        /* call clean up for table and log objects */
        for (i=0; i<LIXA_STATE_TABLES; ++i) {
            if (LIXA_RC_OK != (ret_cod = lixa_state_log_clean(
                                   &this->logs[i])))
                THROW(STATE_LOG_CLEAN_ERROR);
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_clean(
                                   &this->tables[i])))
                THROW(STATE_TABLE_CLEAN_ERROR);
        } /* for (i=0 */
        /* free memory of other objects */
        if (NULL != this->log_synchronizer.buffer)
            free(this->log_synchronizer.buffer);
        if (NULL != this->block_ids)
            free(this->block_ids);
        if (NULL != this->single_page)
            free(this->single_page);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PTHREAD_MUTEX_LOCK_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case PTHREAD_COND_SIGNAL_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_COND_SIGNAL_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case PTHREAD_JOIN_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_JOIN_ERROR;
                break;
            case PTHREAD_COND_DESTROY_ERROR2:
                ret_cod = LIXA_RC_PTHREAD_COND_DESTROY_ERROR;
                break;
            case PTHREAD_MUTEX_DESTROY_ERROR2:
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
        /* restore mutex in the event of error */
        if (mutex_locked &&
            excp > PTHREAD_MUTEX_LOCK_ERROR2 &&
            excp < PTHREAD_MUTEX_UNLOCK_ERROR2)
            pthread_mutex_unlock(&this->log_synchronizer.mutex);
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
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        BUFFER_OVERFLOW,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    *must_flush = FALSE;
    *must_switch = FALSE;
    
    LIXA_TRACE(("lixa_state_check_log_actions\n"));
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
                             this->log_synchronizer.buffer_size));
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
                        LIXA_TRACE(("lixa_state_check_log_actions/"
                                    "malloc: returned null!\n"));
                        free(new_buffer);
                        new_buffer = NULL;
                    }
                    if (error != 0) {
                        LIXA_TRACE(("lixa_state_check_log_actions/"
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
                    if (0 != (pte = pthread_mutex_lock(
                                  &this->log_synchronizer.mutex))) {
                        THROW(PTHREAD_MUTEX_LOCK_ERROR);
                    } else
                        mutex_locked = TRUE;
                    LIXA_TRACE(("lixa_state_check_log_actions: "
                                "buffer expanded from " SIZE_T_FORMAT
                                " to " SIZE_T_FORMAT " bytes, "
                                "size_of_block_ids expanded from "
                                SIZE_T_FORMAT " to " SIZE_T_FORMAT
                                " records (max_number_of_block_ids = "
                                SIZE_T_FORMAT ")\n",
                                this->log_synchronizer.buffer_size,
                                new_buffer_size,
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
                    free(this->log_synchronizer.buffer);
                    this->log_synchronizer.buffer = new_buffer;
                    this->log_synchronizer.buffer_size = new_buffer_size;
                    if (0 != (pte = pthread_mutex_unlock(
                                  &this->log_synchronizer.mutex))) {
                        THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
                    } else
                        mutex_locked = FALSE;
                } /* if (error != 0 || new_block_ids == NULL) */
            } /* if (this->size_of_block_ids == this->max_number_of_ ... */
        } /* if (this->number_of_block_ids == this->size_of_block_ids) */
        /* check again size because it could have been expanded in the
           previous step */
        if (this->number_of_block_ids != this->size_of_block_ids) {
            /* there are free slots in the current buffer */
            uint32_t current_needed_pages = lixa_state_log_needed_pages(
                &this->logs[this->active_state],
                this->number_of_block_ids);
            uint32_t future_needed_pages = lixa_state_log_needed_pages(
                &this->logs[this->active_state],
                this->number_of_block_ids+1);
            size_t available_pages =
                lixa_state_common_buffer2pages(
                    lixa_state_log_get_free_space(
                        &this->logs[this->active_state]));
            LIXA_TRACE(("lixa_state_check_log_actions: "
                        "number_of_block_ids=" UINT32_T_FORMAT ", "
                        "current_needed_pages= " UINT32_T_FORMAT ", "
                        "future_needed_pages=" UINT32_T_FORMAT ", "
                        "available_pages=" SIZE_T_FORMAT ", "
                        "active_state=%d, "
                        "total_size=" OFF_T_FORMAT ", "
                        "reserved=" OFF_T_FORMAT ", "
                        "offset=" OFF_T_FORMAT "\n",
                        this->number_of_block_ids,
                        current_needed_pages, future_needed_pages,
                        available_pages, this->active_state,
                        lixa_state_log_get_total_size(
                            &this->logs[this->active_state]),
                        lixa_state_log_get_reserved(
                            &this->logs[this->active_state]),
                        lixa_state_log_get_offset(
                            &this->logs[this->active_state]) ));
            if (current_needed_pages > available_pages)
                /* this is a severe internal error, a bug */
                THROW(BUFFER_OVERFLOW);
            if (current_needed_pages == available_pages &&
                future_needed_pages > available_pages) {
                *must_flush = TRUE;
                *must_switch = TRUE;
                LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD051I,
                             lixa_state_log_get_pathname(
                                 &this->logs[this->active_state])));
            }
        } /* if (this->number_of_block_ids != this->size_of_block_ids) */
        LIXA_TRACE(("lixa_state_check_log_actions: must_flush=%d, "
                    "must_switch=%d\n", *must_flush, *must_switch));
        
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
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&this->log_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_check_log_actions/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_flush_log_records(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_WAIT_ERROR,
        BUFFER_OVERFLOW1,
        BUFFER_OVERFLOW2,
        GETTIMEOFDAY_ERROR,
        INTERNAL_ERROR,
        FILE_SET_RESERVED,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_flush_log_records\n"));
    TRY {
        size_t number_of_pages;
        size_t number_of_pages_in_buffer;
        size_t pos_in_page, filled_pages, r;
        size_t log_free_pages;
        off_t reserved;
        struct timeval timestamp;

        if (NULL == this)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixa_state_flush_log_records: active_state=%d\n",
                    this->active_state));
        /* acquire the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->log_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* this synchronization is necessary to avoid overlapping a previous
           flusher execution, under normal condition it must be very fast */
        if (this->log_synchronizer.to_be_flushed) {
            lixa_timer_t timer;
            long duration;
            
            LIXA_TRACE(("lixa_state_flush_log_records: WAITING on "
                        "condition...\n"));
            lixa_timer_start(&timer);
            if (0 != (pte = pthread_cond_wait(
                          &this->log_synchronizer.cond,
                          &this->log_synchronizer.mutex)))
                THROW(PTHREAD_COND_WAIT_ERROR);
            lixa_timer_stop(&timer);
            duration = lixa_timer_get_diff(&timer)/1000;
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD053N, duration));
            LIXA_TRACE(("lixa_state_flush_log_records: condition has been "
                        "signaled, total wait time is %ld ms\n", duration));
        }
        /* compute the number of buffer pages */
        number_of_pages = lixa_state_log_blocks2pages(
            this->number_of_block_ids);
        /* compute the number of pages in the buffer */
        number_of_pages_in_buffer =
            this->log_synchronizer.buffer_size / LIXA_SYSTEM_PAGE_SIZE;
        /* check the amount of free space in the log */
        log_free_pages = lixa_state_common_buffer2pages(
            lixa_state_log_get_total_size(
                &this->logs[this->active_state]) -
            lixa_state_log_get_offset(&this->logs[this->active_state]));
        LIXA_TRACE(("lixa_state_flush_log_records: "
                    "number_of_block_ids=" UINT32_T_FORMAT ", "
                    "number_of_records_per_page=" SIZE_T_FORMAT ", "
                    "number_of_pages=" SIZE_T_FORMAT ", "
                    "number_of_pages_in_buffer=" SIZE_T_FORMAT ", "
                    "log_free_pages=" SIZE_T_FORMAT "\n",
                    this->number_of_block_ids,
                    LIXA_STATE_LOG_RECORDS_PER_PAGE,
                    number_of_pages, number_of_pages_in_buffer,
                    log_free_pages));
        /* this should never be true */
        if (number_of_pages > number_of_pages_in_buffer) {
            LIXA_TRACE(("lixa_state_flush_log_records: "
                        "this->number_of_block_ids=" SIZE_T_FORMAT
                        ", this->log_synchronizer.buffer_size=" SIZE_T_FORMAT
                        ", number_of_pages=" SIZE_T_FORMAT
                        ", number_of_pages_in_buffer=" SIZE_T_FORMAT "\n",
                        this->number_of_block_ids,
                        this->log_synchronizer.buffer_size,
                        number_of_pages,
                        number_of_pages_in_buffer));
            THROW(BUFFER_OVERFLOW1);
        }
        /* this should never be true */
        if (number_of_pages > log_free_pages)
            THROW(BUFFER_OVERFLOW2);
        /* retrieve the timestamp associated to this flush operation */
        if (0 != gettimeofday(&timestamp, NULL))
            THROW(GETTIMEOFDAY_ERROR);
        /* reset the buffer */
        memset(this->log_synchronizer.buffer, 0,
               number_of_pages * LIXA_SYSTEM_PAGE_SIZE);
        /* loop on the number of records */
        pos_in_page = 0;
        filled_pages = 0;
        for (r=0; r<this->number_of_block_ids; ++r) {
            struct lixa_state_log_record_s *log_record =
                (struct lixa_state_log_record_s *)
                (this->log_synchronizer.buffer +
                 filled_pages * LIXA_SYSTEM_PAGE_SIZE +
                 pos_in_page * sizeof(struct lixa_state_log_record_s));
            const lixa_state_slot_t *s1 =
                lixa_state_table_get_slot(
                    &this->tables[this->active_state], this->block_ids[r]);
            const lixa_state_record_t *record =
                lixa_state_slot_get_record(s1);
            
            log_record->id = lixa_state_log_get_next_record_id(
                &this->logs[this->active_state]);
            log_record->timestamp = timestamp;
            memcpy(&(log_record->record), record,
                   sizeof(union status_record_u));
            /* compute the CRC32 code */
            log_record->crc32 = lixa_crc32(
                (const uint8_t *)log_record,
                sizeof(struct lixa_state_log_record_s) - sizeof(uint32_t));
            LIXA_TRACE(("lixa_state_flush_log_records: filled_page="
                        SIZE_T_FORMAT ", pos_in_page=" SIZE_T_FORMAT
                        ", log_record->id=" UINT32_T_FORMAT
                        ", log_record->crc32=0x%08x, slot in state table = "
                        UINT32_T_FORMAT "\n",
                        filled_pages, pos_in_page, log_record->id,
                        log_record->crc32, this->block_ids[r]));
            pos_in_page++;
            if (pos_in_page % LIXA_STATE_LOG_RECORDS_PER_PAGE == 0) {
                pos_in_page = 0;
                filled_pages++;
            }
        } /* for (r=0; r<number_of_records_to_be_flushed; ++r) */
        /* ask to flusher thread to perform the flushing */
        LIXA_TRACE(("lixa_state_flush_log_records: asking flusher thread to "
                    "flush the buffer to file\n"));
        this->log_synchronizer.operation = STATE_FLUSHER_FLUSH;
        this->log_synchronizer.to_be_flushed = TRUE;
        this->log_synchronizer.log = &this->logs[this->active_state];
        this->log_synchronizer.number_of_pages = number_of_pages;
        /* check there are no reservation in place */
        if (0 != (reserved = lixa_state_log_get_reserved(
                      &this->logs[this->active_state]))) {
            LIXA_TRACE(("lixa_state_flush_log_records: internal error, "
                        "reserved=" OFF_T_FORMAT "\n", reserved));
            THROW(INTERNAL_ERROR);
        }
        /* set a reservation before waiking up the flusher thread */
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_reserved(
                               &this->logs[this->active_state],
                               lixa_state_common_pages2buffer(
                                   this->log_synchronizer.number_of_pages))))
            THROW(FILE_SET_RESERVED);
        if (0 != (pte = pthread_cond_signal(&this->log_synchronizer.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR);
        if (0 != (pte = pthread_mutex_unlock(&this->log_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);
        } else
            mutex_locked = FALSE;
        /* reset block_ids */
        this->number_of_block_ids = 0;
        memset(this->block_ids, 0,
               sizeof(uint32_t) * this->size_of_block_ids);
        
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
            case BUFFER_OVERFLOW1:
            case BUFFER_OVERFLOW2:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case FILE_SET_RESERVED:
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
            pthread_mutex_unlock(&this->log_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_flush_log_records/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
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
        LIXA_TRACE(("lixa_state_extend_log: active_state=%d\n",
                    this->active_state));
        if (LIXA_RC_OK != (ret_cod = lixa_state_log_extend(
                               &this->logs[this->active_state])))
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



int lixa_state_flush_table(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_WAIT_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_flush_table\n"));
    TRY {
        /* check object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* acquire the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->table_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* this synchronization is necessary to avoid overlapping a previous
           flusher execution, under normal condition it must be very fast */
        if (this->table_synchronizer.to_be_flushed) {
            lixa_timer_t timer;
            long duration;
            
            LIXA_TRACE(("lixa_state_flush_table: WAITING on condition...\n"));
            lixa_timer_start(&timer);
            if (0 != (pte = pthread_cond_wait(
                          &this->table_synchronizer.cond,
                          &this->table_synchronizer.mutex)))
                THROW(PTHREAD_COND_WAIT_ERROR);
            lixa_timer_stop(&timer);
            duration = lixa_timer_get_diff(&timer);
            LIXA_TRACE(("lixa_state_flush_table: condition has been "
                        "signaled, total wait time is %ld us\n", duration));
            duration /= 1000; /* transform to milliseconds */
            if (duration > 0)
                LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD058N, duration));
        }
        /* ask to table flusher thread to perform the flushing */
        LIXA_TRACE(("lixa_state_flush_table: asking table flusher thread to "
                    "synchronize the memory map with the underlying file\n"));
        this->table_synchronizer.operation = STATE_FLUSHER_FLUSH;
        this->table_synchronizer.table = &this->tables[this->active_state];
        this->table_synchronizer.to_be_flushed = TRUE;
        if (0 != (pte = pthread_cond_signal(&this->table_synchronizer.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR);
        if (0 != (pte = pthread_mutex_unlock(
                      &this->table_synchronizer.mutex))) {
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
            case PTHREAD_COND_WAIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_WAIT_ERROR;
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
            pthread_mutex_unlock(&this->table_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_flush_table/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_sync_log(lixa_state_t *this)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_WAIT_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0;
    int mutex_locked = FALSE;
    
    LIXA_TRACE(("lixa_state_sync_log\n"));
    TRY {
        /* check object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* acquire the lock of the synchronized structure */
        if (0 != (pte = pthread_mutex_lock(&this->log_synchronizer.mutex))) {
            THROW(PTHREAD_MUTEX_LOCK_ERROR);
        } else
            mutex_locked = TRUE;
        /* this synchronization is necessary to avoid overlapping a previous
           flusher execution, under normal condition it must be very fast */
        if (this->log_synchronizer.to_be_flushed) {
            lixa_timer_t timer;
            long duration;
            
            LIXA_TRACE(("lixa_state_sync_log: WAITING on condition...\n"));
            lixa_timer_start(&timer);
            if (0 != (pte = pthread_cond_wait(
                          &this->log_synchronizer.cond,
                          &this->log_synchronizer.mutex)))
                THROW(PTHREAD_COND_WAIT_ERROR);
            lixa_timer_stop(&timer);
            duration = lixa_timer_get_diff(&timer)/1000;
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD062N, duration));
            LIXA_TRACE(("lixa_state_sync_log: condition has been "
                        "signaled, total wait time is %ld ms\n", duration));
        }
        /* ask to log flusher thread to perform the flushing */
        LIXA_TRACE(("lixa_state_sync_log: asking log flusher thread to "
                    "synchronize the buffer content to the underlying "
                    "file\n"));
        this->log_synchronizer.operation = STATE_FLUSHER_FLUSH;
        this->log_synchronizer.log = &this->logs[this->active_state];
        this->log_synchronizer.to_be_flushed = TRUE;
        if (0 != (pte = pthread_cond_signal(&this->log_synchronizer.cond)))
            THROW(PTHREAD_COND_SIGNAL_ERROR);
        if (0 != (pte = pthread_mutex_unlock(
                      &this->log_synchronizer.mutex))) {
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
            case PTHREAD_COND_WAIT_ERROR:
                ret_cod = LIXA_RC_PTHREAD_COND_WAIT_ERROR;
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
            pthread_mutex_unlock(&this->log_synchronizer.mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_sync_log/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    return ret_cod;
}



int lixa_state_switch(lixa_state_t *this, lixa_word_t last_record_id)
{
    enum Exception {
        NULL_OBJECT,
        TABLE_CREATE_NEW_FILE,
        SET_LAST_RECORD_ID,
        SET_STATUS1,
        SET_STATUS2,
        TABLE_COPY_FROM,
        FLUSH_TABLE,
        SET_STATUS3,
        LOG_CREATE_NEW_FILE,
        SYNC_LOG,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_switch(last_record_id=" LIXA_WORD_T_FORMAT ")\n",
                last_record_id));
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
        /* put in state table header a reference to the last record that has
           been for sure flushed by the state log */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_set_last_record_id(
                    &this->tables[this->active_state], last_record_id)))
            THROW(SET_LAST_RECORD_ID);
        /* current state table is switched to COPY_SOURCE status */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_set_status(
                    &this->tables[this->active_state],
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
                               &this->tables[this->active_state])))
            THROW(TABLE_COPY_FROM);
        /* start disk synchronization of current state file using the
           background thread ... */
        if (LIXA_RC_OK != (ret_cod = lixa_state_flush_table(this)))
            THROW(FLUSH_TABLE);
        /* next state table is switched to USED status */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_set_status(
                    &this->tables[lixa_state_get_next_state(this)],
                    STATE_TABLE_USED, FALSE)))
            THROW(SET_STATUS3);
        
        /*
          switch log file: ask flusher thread to start synchronization
         */
        /* has next state log to be created and formatted? */
        if (STATE_LOG_UNDEFINED == lixa_state_log_get_status(
                &this->logs[lixa_state_get_next_state(this)])) {
            if (LIXA_RC_OK != (
                    ret_cod = lixa_state_log_create_new_file(
                        &this->logs[lixa_state_get_next_state(this)],
                        this->single_page)))
                THROW(LOG_CREATE_NEW_FILE);
        }
        /* @@@ remove me: useless or dagerous...
        if (LIXA_RC_OK != (ret_cod = lixa_state_sync_log(this)))
            THROW(SYNC_LOG);
        */

        /* change active_state */
        this->active_state = lixa_state_get_next_state(this);
        LIXA_TRACE(("lixa_state_switch: new state table and log is now %d\n",
                    this->active_state));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case SET_LAST_RECORD_ID:
            case SET_STATUS1:
            case SET_STATUS2:
            case TABLE_COPY_FROM:
            case FLUSH_TABLE:
            case SET_STATUS3:
                break;
            case LOG_CREATE_NEW_FILE:
            case SYNC_LOG:
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
        BUFFER_OVERFLOW,
        STATE_TABLE_SYNC_BLOCK_ERROR,
        CHECK_LOG_ACTIONS_ERROR,
        FLUSH_LOG_RECORDS_ERROR,
        LOG_SET_STATUS_ERROR,
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
        int already_in_array = FALSE;
        lixa_word_t last_record_id;
        uint32_t i;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check there is space in the buffer */
        if (this->size_of_block_ids == this->number_of_block_ids) {
            /* this is an internal error because it should never happen! */
            LIXA_TRACE(("lixa_state_mark_block: size_of_block_ids = "
                        "number_of_block_ids = " UINT32_T_FORMAT "\n",
                        this->size_of_block_ids));
            THROW(BUFFER_OVERFLOW);
        }
        /* check if block_id is already in array */
        for (i=0; i<this->number_of_block_ids; ++i)
            if (this->block_ids[i] == block_id) {
                already_in_array = TRUE;                
                break;
            }
        /* keep track of the passed block_id */
        if (!already_in_array)
            this->block_ids[this->number_of_block_ids++] = block_id;
        /* sign the block in the state table */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_sync_block(
                    &this->tables[this->active_state], block_id)))
            THROW(STATE_TABLE_SYNC_BLOCK_ERROR);
        /* check if state log must be flushed */
        if (LIXA_RC_OK != (ret_cod = lixa_state_check_log_actions(
                               this, &must_flush, &must_switch)))
            THROW(CHECK_LOG_ACTIONS_ERROR);
        /* retrieve the id of the last record in the current log state file:
           it will be written in the state table in the event of switching */
        last_record_id = lixa_state_log_get_last_record_id(
            &this->logs[this->active_state]);
        /* flush the state log if necessary */
        if (must_flush) {
            LIXA_TRACE(("lixa_state_mark_block: flush records\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_state_flush_log_records(this)))
                THROW(FLUSH_LOG_RECORDS_ERROR);
        }
        /* at this point we are sure last_record_id has been flushed because
           above flushing started after it */
        if (must_switch) {
            /* is the previous state table in the middle of a sync phase? */
            if (lixa_state_table_is_syncing(
                    &this->tables[lixa_state_get_prev_state(this)])) {
                LIXA_TRACE(("lixa_state_mark_block: state table is syncing, "
                            "extending log file\n"));
                if (LIXA_RC_OK != (ret_cod = lixa_state_log_set_status(
                                       &this->logs[this->active_state],
                                       STATE_LOG_FULL, FALSE)))
                    THROW(LOG_SET_STATUS_ERROR);
                if (LIXA_RC_OK != (ret_cod = lixa_state_extend_log(this)))
                    THROW(FLUSH_LOG_EXTEND_ERROR);
            } else {
                LIXA_TRACE(("lixa_state_mark_block: switch state table and "
                            "log\n"));
                if (LIXA_RC_OK != (ret_cod = lixa_state_switch(
                                       this, last_record_id)))
                    THROW(SWITCH_ERROR);
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case STATE_TABLE_SYNC_BLOCK_ERROR:
            case CHECK_LOG_ACTIONS_ERROR:
            case FLUSH_LOG_RECORDS_ERROR:
            case LOG_SET_STATUS_ERROR:
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
        if (lixa_state_table_is_full(&this->tables[this->active_state])) {
            LIXA_TRACE(("lixa_state_insert_block: current state table (%d) "
                        "is full and must be extended...\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_extend(
                                   &this->tables[this->active_state],
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
                               &this->tables[this->active_state],
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
                               &this->tables[this->active_state],
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
            if (0 != (pte = pthread_mutex_lock(&table_synchronizer->mutex))) {
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
                    /* perform flushing */
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
    /* Emergency exit? */
    if (NONE != excp) {
        int rc;
        LIXA_SYSLOG((LOG_CRIT, LIXA_SYSLOG_LXD067C, excp, ret_cod, pte,
                     errno));
        /* sending kill to this process */
        if (0 != (rc = kill(getpid(), SIGKILL))) {
            LIXA_TRACE(("lixa_state_async_table_flusher/kill: errno=%d, "
                        "'%s'\n", errno, strerror(errno)));
        }
    } /* if (NONE != excp) */
    return NULL;
}



void *lixa_state_async_log_flusher(void *data)
{
    enum Exception {
        NULL_OBJECT,
        PTHREAD_MUTEX_LOCK_ERROR,
        PTHREAD_COND_WAIT_ERROR,
        LOG_FILE_WRITE_ERROR,
        INTERNAL_ERROR,
        PTHREAD_COND_SIGNAL_ERROR,
        PTHREAD_MUTEX_UNLOCK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int pte = 0; /* pthread_error */
    int mutex_locked = FALSE;
    struct lixa_log_synchronizer_s
        *log_synchronizer = (struct lixa_log_synchronizer_s *)data;
    
    LIXA_TRACE(("lixa_state_async_log_flusher\n"));
    TRY {
        int exit = FALSE;

        if (NULL == log_synchronizer)
            THROW(NULL_OBJECT);
        
        while (!exit) {
            LIXA_TRACE(("lixa_state_async_log_flusher: locking "
                        "flusher_mutex...\n"));
            if (0 != (pte = pthread_mutex_lock(&log_synchronizer->mutex))) {
                THROW(PTHREAD_MUTEX_LOCK_ERROR);
            } else
                mutex_locked = TRUE;
            /* check the operation asked by the main thread */
            if (STATE_FLUSHER_WAIT == log_synchronizer->operation) {
                LIXA_TRACE(("lixa_state_async_log_flusher: WAITING on "
                            "condition...\n"));
                if (0 != (pte = pthread_cond_wait(
                              &log_synchronizer->cond,
                              &log_synchronizer->mutex)))
                    THROW(PTHREAD_COND_WAIT_ERROR);
                LIXA_TRACE(("lixa_state_async_log_flusher: condition has been "
                            "signaled\n"));
            }
            if (STATE_FLUSHER_FLUSH == log_synchronizer->operation) {
                /* flush the data to file */
                if (log_synchronizer->to_be_flushed) {
                    if (LIXA_RC_OK != (ret_cod = lixa_state_log_write(
                                           log_synchronizer->log,
                                           log_synchronizer->buffer,
                                           log_synchronizer->number_of_pages)))
                        THROW(LOG_FILE_WRITE_ERROR);
                } else {
                    /* this is an internal error, it should never happen */
                    LIXA_TRACE(("lixa_state_async_log_flusher: internal "
                                "error, flusher.operation=%d and "
                                "flusher.to_be_flushed=%d\n",
                                log_synchronizer->operation,
                                log_synchronizer->to_be_flushed));
                    THROW(INTERNAL_ERROR);
                }
                /* reset flushing condition */
                log_synchronizer->to_be_flushed = FALSE;
                log_synchronizer->operation = STATE_FLUSHER_WAIT;
                /* signaling to master thread */
                if (0 != (pte = pthread_cond_signal(&log_synchronizer->cond)))
                    THROW(PTHREAD_COND_SIGNAL_ERROR);
            }
            if (STATE_FLUSHER_EXIT == log_synchronizer->operation) {
                LIXA_TRACE(("lixa_state_async_log_flusher: EXITING\n"));
                exit = TRUE;
            }
            /* this should never happen */
            if (STATE_FLUSHER_WAIT != log_synchronizer->operation &&
                STATE_FLUSHER_FLUSH != log_synchronizer->operation &&
                STATE_FLUSHER_EXIT != log_synchronizer->operation) {
                LIXA_TRACE(("lixa_state_async_log_flusher: internal error "
                            "flusher_operation=%d\n",
                            log_synchronizer->operation));
                exit = TRUE;
            }
            /* unlock the mutex */
            LIXA_TRACE(("lixa_state_async_log_flusher: unlocking "
                        "flusher_mutex\n"));
            if (0 != (pte = pthread_mutex_unlock(&log_synchronizer->mutex))) {
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
        /* restore mutex in the event of error */
        if (mutex_locked)
            pthread_mutex_unlock(&log_synchronizer->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_async_log_flusher/excp=%d/"
                "ret_cod=%d/pthreaderror=%d/errno=%d\n", excp, ret_cod, pte,
                errno));
    /* Emergency exit? */
    if (NONE != excp) {
        int rc;
        LIXA_SYSLOG((LOG_CRIT, LIXA_SYSLOG_LXD068C, excp, ret_cod, pte,
                     errno));
        /* sending kill to this process */
        if (0 != (rc = kill(getpid(), SIGKILL))) {
            LIXA_TRACE(("lixa_state_async_log_flusher/kill: errno=%d, "
                        "'%s'\n", errno, strerror(errno)));
        }
    } /* if (NONE != excp) */
    return NULL;
}



