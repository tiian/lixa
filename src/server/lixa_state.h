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
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
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
 * This internal struct is synchronized with a mutex and a condition to
 * avoid race conditions between the main thread and the flusher thread
 * dedicated to state table synchronization
 */
struct lixa_table_synchronizer_s {
    /**
     * Mutex used to protect the object when accessed concurrently by the
     * flusher thread
     */
    pthread_mutex_t                     mutex;
    /**
     * Condition used to signal events between main thread and flusher
     * thread
     */
    pthread_cond_t                      cond;
    /**
     * Operation asked by main thread to flusher thread
     */
    enum lixa_state_flusher_ops_e       operation;
    /**
     * A reference to the state table that must be flushed
     */
    lixa_state_table_t                 *table;
    /**
     * Boolean flag: master set it to TRUE before flushing, flusher set it
     * to FALSE after completion
     */
    int                                 to_be_flushed;
    /**
     * Thread used to flush the log files in background (asynchronously)
     */
    pthread_t                           thread;
};



/**
 * This internal struct is synchronized with a mutex and a condition to
 * avoid race conditions between the main thread and the flusher thread
 */
struct lixa_log_synchronizer_s {
    /**
     * Mutex used to protect the object when accessed concurrently by the
     * flusher thread
     */
    pthread_mutex_t                   mutex;
    /**
     * Condition used to signal events between main thread and flusher
     * thread
     */
    pthread_cond_t                    cond;
    /**
     * Operation asked by main thread to flusher thread
     */
    enum lixa_state_flusher_ops_e     operation;
    /**
     * A reference to the state log that must be flushed
     */
    lixa_state_log_t                 *log;
    /**
     * Boolean flag: master set it to TRUE before flushing, flusher set it
     * to FALSE after completion
     */
    int                               to_be_flushed;
    /**
     * Number of pages that must be flushed from buffer to file
     */
    size_t                            number_of_pages;
    /**
     * Size of the buffer used to manage I/O; unit is "number of bytes"
     */
    size_t                            buffer_size;
    /**
     * Buffer used to manage I/O
     */
    void                             *buffer;
    /**
     * Thread used to flush the log files in background (asynchronously)
     */
    pthread_t                         thread;
};



/**
 * LIXA State data type ("class")
 */
typedef struct lixa_state_s {
    /**
     * Points to the active state table and state log: the objects currently
     * in use
     */
    int                               active_state;
    /**
     * Array of state tables
     */
    lixa_state_table_t                tables[LIXA_STATE_TABLES];
    /**
     * Array of state logs
     */
    lixa_state_log_t                  logs[LIXA_STATE_TABLES];
    /*
     * Section dedicated to log management
     */
    /**
     * Maximum size of the log buffer, typically retrieved from configuration
     */
    size_t                            max_buffer_log_size;
    /**
     * The size of the array, the number of block_ids that can be currently
     * managed by the buffer
     */
    size_t                            size_of_block_ids;
    /**
     * An array used to store the positions of all the changed blocks in the
     * state file
     */
    uint32_t                         *block_ids;
    /**
     * The number of block_id values in block_ids array
     */
    size_t                            number_of_block_ids;
    /**
     * Maximum size of the array, upper limit to cap memory consumption
     */
    size_t                            max_number_of_block_ids;
    /**
     * A single memory page used for special purposes like formatting logs
     */
    void                             *single_page;
    /**
     * Struct used to communicate with the background flusher thread for
     * state logs
     */
    struct lixa_log_synchronizer_s    log_synchronizer;
    /*
     * Section dedicated to table management
     */
    /**
     * Struct used to communicate with the background flusher thread for
     * state tables
     */
    struct lixa_table_synchronizer_s  table_synchronizer;
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
     * @param[in] read_only must be TRUE if the state will be used only in
     *            read-only mode
     * @return a reason code
     */
    int lixa_state_init(lixa_state_t *this, const char *path_prefix,
                        size_t max_buffer_log_size, int read_only);



    /**
     * Analyze the available state and log files: establish if a warm restart
     * or a cold start must be performed
     * @param[in,out] this state object
     * @param[in] table_exists specifies which table files are available
     * @param[in] log_exists specifies which log files are available
     * @param[out] cold_start returns TRUE if a cold start must be performed
     * @return a reason code
     */     
    int lixa_state_analyze(lixa_state_t *this,
                           const int *table_exists,
                           const int *log_exists,
                           int *cold_start);

    
    
    /**
     * Start a fresh new state table and log set because a cold start must be
     * done
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_cold_start(lixa_state_t *this);

    
    
    /**
     * Start the existing state table and log set because a warm start must be
     * done
     * @param[in,out] this state object
     * @param[in] table_exists specifies which table files are available
     * @param[in] log_exists specifies which log files are available
     * @param[in] read_only mode for state inspection from another process
     * @return a reason code
     */
    int lixa_state_warm_start(lixa_state_t *this,
                              const int *table_exists,
                              const int *log_exists,
                              int read_only);



    /**
     * Close a state object, shutdown the state and put everything in a
     * consistent and durable status
     * @param[in,out] this state object
     * @return a reason code
     */     
    int lixa_state_close(lixa_state_t *this);


    
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
     * @return a reason code
     */
    int lixa_state_flush_log_records(lixa_state_t *this);



    /**
     * Extend the state log
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_extend_log(lixa_state_t *this);



    /**
     * Start the background flush of the current state table
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_flush_table(lixa_state_t *this);


    
    /**
     * Start the background sync of the current state log
     * @param[in,out] this state object
     * @return a reason code
     */
    int lixa_state_sync_log(lixa_state_t *this);
    


    /**
     * Switch the current state table and log to the following ones
     * @param[in,out] this state object
     * @param[in] last_record_id is the identifier of the last record that's
     *            for sure in current state log
     * @return a reason code
     */     
    int lixa_state_switch(lixa_state_t *this, lixa_word_t last_record_id);

    

    /**
     * Mark a block because it has been updated
     * @param[in,out] this state object
     * @param[in] block_id of the changed block
     * @return a reason code
     */
    int lixa_state_mark_block(lixa_state_t *this, uint32_t block_id);



    /**
     * Insert a new used block in the state
     * @param[in,out] this state object
     * @param[out] block_id of the inserted block
     * @return a reason code
     */
    int lixa_state_insert_block(lixa_state_t *this, uint32_t *block_id);


    
    /**
     * Delete a used block from the state
     * @param[in,out] this state object
     * @param[in] block_id of the deleted block
     * @return a reason code
     */
    int lixa_state_delete_block(lixa_state_t *this, uint32_t block_id);



    /**
     * Return the state table used before the current one
     * @param[in] this state object
     * @return previous used state table (and log)
     */
    static inline int lixa_state_get_prev_state(const lixa_state_t *this) {
        return 0 == this->active_state ?
            LIXA_STATE_TABLES-1 : this->active_state-1;
    }


    
    /**
     * Return the state table that will be used after the current one
     * @param[in] this state object
     * @return previous used state table (and log)
     */
    static inline int lixa_state_get_next_state(const lixa_state_t *this) {
        return LIXA_STATE_TABLES-1 == this->active_state ?
            0 : this->active_state+1;
    }


    
    /**
     * Entry point of the flusher thread for state tables
     * @param[in,out] data is of type @ref lixa_state_table_t
     */
    void *lixa_state_async_table_flusher(void *data);


    
    /**
     * Entry point of the flusher thread for state logs
     * @param[in,out] data is of type @ref lixa_state_log_t
     */
    void *lixa_state_async_log_flusher(void *data);


    
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
