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
#ifndef LIXA_STATE_H
# define LIXA_STATE_H



#include "config.h"



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include "lixa_trace.h"
#include "lixa_state_common.h"
#include "lixa_state_table.h"
#include "lixa_state_log.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Suffix appended when a new state table is created
 */
extern const char *LIXA_STATE_TABLE_SUFFIX;



/**
 * LIXA State data type ("class")
 */
typedef struct lixa_state_s {
    /**
     * Maximum size of the log buffer, typically retrieved from configuration
     */
    size_t                max_buffer_log_size;
    /**
     * Points to the state table (and state log) currently in use
     */
    int                   used_state_table;
    /**
     * Array of state tables
     */
    lixa_state_table_t    tables[LIXA_STATE_TABLES];
    /**
     * Array of state logs
     */
    lixa_state_log_t      log;
} lixa_state_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    
    /**
     * Initialize a State object to manage state server persistence
     * @param[in,out] this object to be initialized
     * @param[in] path_prefix of the state and log files
     * @param[in] max_buffer_log_size maximum number of bytes that can be used
     *            for the log I/O buffer
     * @return a reason code
     */
    int lixa_state_init(lixa_state_t *this, const char *path_prefix,
                        size_t max_buffer_log_size);



    /**
     * Analyze the available state and log files: establish if a warm restart
     * or a cold start must be performed
     * @param[in,out] this state object
     * @param[in] state_exists specifies which state tables are available
     * @param[in] log_exists specifies which log tables are available
     * @param[out] cold_start returns TRUE if a cold start must be performed
     * @return a reason code
     */     
    int lixa_state_analyze(lixa_state_t *this,
                           const int *state_exists,
                           const int *log_exists,
                           int *cold_start);

    
    
    /**
     * Create new state and log files for this state object because a cold
     * start must be done
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_cold_start(lixa_state_t *this);

    
    
    /**
     * Cleanup a State object
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_clean(lixa_state_t *this);



    
    /**
     * Check if the current log requires some actions
     * @param[in] this state object
     * @param[out] must_flush if the buffer is full
     * @param[out] must_switch if the log is full
     * @return a reason code
     */
    int lixa_state_check_log_actions(lixa_state_t *this, int *must_flush,
                                     int *must_switch);
    


    /**
     * Flush the records to the state log
     * @param[in,out] this state object
     * @param[in] status_records @@@ move to lixa_state_table_t?
     * @return a reason code
     */
    int lixa_state_flush_log_records(lixa_state_t *this,
                                     status_record_t *status_records);

    

    /**
     * Extend the state log
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_extend_log(lixa_state_t *this);

    

    /**
     * Mark a block because it has been updated
     * @param[in,out] this state object
     * @param[in] block_id of the changed block
     * @return a reason code
     */
    int lixa_state_mark_block(lixa_state_t *this, uint32_t block_id);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_STATE_H */