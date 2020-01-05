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
#ifndef LIXA_STATE_LOG_H
# define LIXA_STATE_LOG_H



#include "config.h"



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_utils.h"
#include "lixa_state_common.h"
#include "lixa_state_log_file.h"
#include "status_record.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/** Initial buffer size (unit = byte) */
#define LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT   (2*LIXA_SYSTEM_PAGE_SIZE)



/**
 * Number of record that can be stored in a memory page
 */
extern size_t LIXA_STATE_LOG_RECORDS_PER_PAGE;



/*
 * Just a declaration
 */
typedef struct lixa_state_table_s lixa_state_table_t;



/**
 * Operations that can be sent from the main thread to the flusher thread
 */
enum lixa_state_log_flusher_ops_e {
    /** nothing to do, just wait on condition */
    STATE_LOG_FLUSHER_WAIT = 0,
    /** flush the buffer to the current used file */
    STATE_LOG_FLUSHER_FLUSH,
    /** thread termination */
    STATE_LOG_FLUSHER_EXIT
};



/**
 * LIXA State Log data type ("class")
 */
typedef struct lixa_state_log_s {
    /**
     * Files used to persist the log
     */
    lixa_state_log_file_t             files[LIXA_STATE_TABLES];
    /**
     * Points to the currently used file, @ref files array
     */
    size_t                            used_file;
    /**
     * A single memory page used for special purposes like formatting
     */
    void                             *single_page;
    /**
     * An array used to store the positions of all the changed blocks in the
     * state file
     */
    uint32_t                         *block_ids;
    /**
     * The size of the array, the number of block_ids that can be currently
     * managed by the buffer
     */
    size_t                            size_of_block_ids;
    /**
     * The number of block_id values in block_ids array
     */
    size_t                            number_of_block_ids;
    /**
     * Maximum size of the array, upper limit to cap memory consumption
     */
    size_t                            max_number_of_block_ids;
    /**
     * Last record id used in the state log; 0 has special meaning of resetted
     * record, unused record
     */
    lixa_word_t                       last_record_id;
    /**
     * This internal struct is synchronized with a mutex and a condition to
     * avoid race conditions between the main thread and the flusher thread
     */
    struct {
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
        enum lixa_state_log_flusher_ops_e operation;
        /**
         * File that must be flushed by the flusher thread: it's protected by
         * flusher_mutex and by flusher_cond
         */
        size_t                            file_pos;
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
    } synch;
} lixa_state_log_t;



/**
 * Struct used to access records in the state log file
 */
struct lixa_state_log_record_s {
    /**
     * Unique identifier of the record
     */
    lixa_word_t                      id;
    /**
     * Time of the record flush operation (many records can have the same
     * timestamp because flushing can happen inside a batch
     */
    struct timeval                   timestamp;
    /**
     * The record that must be transferred
     */
    union status_record_u            record;
    /**
     * CRC to check record integrity
     */
    uint32_t                         crc32;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a StateLog object to manage a state log
     * @param[in,out] this object to be initialized
     * @param[in] path_prefix that must be used to open or create the
     *            underlying files
     * @param[in] max_buffer_size maximum number of bytes that can be used for
     *            the I/O buffer
     * @param[in] o_direct_bool activates O_DIRECT POSIX flag
     * @param[in] o_dsync_bool activates O_DSYNC POSIX flag
     * @param[in] o_rsync_bool activates O_RSYNC POSIX flag
     * @param[in] o_sync_bool activates O_SYNC POSIX flag
     * @return a reason code
     */
    int lixa_state_log_init(lixa_state_log_t *this,
                            const char *path_prefix,
                            size_t max_buffer_size,
                            int o_direct_bool,
                            int o_dsync_bool,
                            int o_rsync_bool,
                            int o_sync_bool);



    /**
     * Create a new underlying file for the state log object
     * @param[in,out] this current state file object
     * @param[in] pos postion of the file to be created in the array
     * @return a reason code
     */
    static inline int lixa_state_log_create_new_file(
        lixa_state_log_t *this, size_t pos)
    {
        return LIXA_STATE_TABLES > pos ?
            lixa_state_log_file_create_new(
                &this->files[pos], this->single_page) : LIXA_RC_OUT_OF_RANGE;
    }



    /**
     * Check if an underlying file exists and can be opened
     * @param[in] this current state log object
     * @param[in] pos position of the file to be checked in the array
     * @return a reason code, LIXA_RC_OK if the file exists and can be opened
     */
    static inline int lixa_state_log_exist_file(lixa_state_log_t *this,
                                                size_t pos)
    {
        return LIXA_STATE_TABLES > pos ?
            lixa_state_log_file_exists(&this->files[pos]) :
            LIXA_RC_OUT_OF_RANGE;
    }



    /**
     * Cleanup a StateLog object
     */
    int lixa_state_log_clean(lixa_state_log_t *this);



    /**
     * Set the file to be used for logging
     */
    static inline void lixa_state_log_set_used_file(
        lixa_state_log_t *this, size_t used_file) {
        if (LIXA_STATE_TABLES > used_file)
            this->used_file = used_file;
        else {
            LIXA_TRACE(("lixa_state_log_set_used_file: used_file="
                        SIZE_T_FORMAT"!!!\n", used_file));
        }
    }

    

    /**
     * Return the pathname of one of the files associated to the state log
     * @param[in] this current state log object
     * @param[in] pos position of the file to be checked in the array
     * @return a pathname
     */
    static inline const char* lixa_state_log_get_pathname(
        const lixa_state_log_t *this, size_t pos) {
        return LIXA_STATE_TABLES > pos ?
            lixa_state_log_file_get_pathname(&this->files[pos]) : NULL;
    }

    
    
    /**
     * Flush the records to the underlying file
     * @param[in,out] this state log object
     * @param[in] state_table
     * @return a reason code
     */
    int lixa_state_log_flush(lixa_state_log_t *this,
                             const lixa_state_table_t *state_table);
    /*
                             status_record_t *status_records);
    */
    


    /**
     * Extend the underlying file
     * @param[in,out] this state log object
     * @return a reason code
     */
    int lixa_state_log_extend(lixa_state_log_t *this);



    /**
     * Mark a block because it has been updated; the function assumes that
     * a block has not been marked before, the function does not manage
     * deduplication by itself
     * @param[in,out] this state log object
     * @param[in] block_id of the changed block
     * @return a reason code
     */
    int lixa_state_log_mark_block(lixa_state_log_t *this,
                                  uint32_t block_id);



    /**
     * Returns the number of marked block_ids
     * @param[in,out] this state log object
     * @return a value
     */     
    static inline uint32_t lixa_state_log_get_number_of_block_ids(
        lixa_state_log_t *this)
    {
        return this->number_of_block_ids;
    }



    /**
     * Check if the state log require some actions
     * @param[in,out] this state log object
     * @param[out] must_flush if the buffer is full and it must be flushed
     * @param[out] must_switch if the log is full and it must be switched (or
     *             expanded)
     * @return a boolean value
     */     
    int lixa_state_log_check_actions(lixa_state_log_t *this,
                                     int *must_flush, int *must_switch);



    /**
     * Compute the number of pages necessary to store a number of records
     * @param[in] this state log object
     * @param[in] number_of_records that must be computed
     * @return the number of pages necessary to store the number of records
     */
    static inline uint32_t lixa_state_log_needed_pages(
        lixa_state_log_t *this, uint32_t number_of_records)
    {
        return (number_of_records / LIXA_STATE_LOG_RECORDS_PER_PAGE) +
            (number_of_records % LIXA_STATE_LOG_RECORDS_PER_PAGE ? 1 : 0);
    }



    /**
     * Conversion from buffer size to number of blocks
     * @param[in] buffer_size in bytes
     * @return number of blocks
     */
    static inline size_t lixa_state_log_buffer2blocks(size_t buffer_size)
    {
        return buffer_size / LIXA_SYSTEM_PAGE_SIZE *
            LIXA_STATE_LOG_RECORDS_PER_PAGE;
    }

    

    /**
     * Conversion from number of blocks to buffer size (page aligned)
     * @param[in] number_of_blocks
     * @return size of the page aligned buffer in bytes
     */
    static inline size_t lixa_state_log_blocks2buffer(size_t number_of_blocks)
    {
        return ((number_of_blocks / LIXA_STATE_LOG_RECORDS_PER_PAGE) +
                (number_of_blocks % LIXA_STATE_LOG_RECORDS_PER_PAGE ? 1 : 0)) *
                LIXA_SYSTEM_PAGE_SIZE;
    }



    /**
     * Conversion from number of blocks to number of pages
     * @param[in] number_of_blocks
     * @return number of pages
     */
    static inline size_t lixa_state_log_blocks2pages(size_t number_of_blocks)
    {
        return (number_of_blocks / LIXA_STATE_LOG_RECORDS_PER_PAGE) +
            (number_of_blocks % LIXA_STATE_LOG_RECORDS_PER_PAGE ? 1 : 0);
    }



    /**
     * Entry point of the flusher thread
     * @param[in,out] data is of type @ref lixa_state_log_t
     */
    void *lixa_state_log_flusher(void *data);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_STATE_LOG_H */
