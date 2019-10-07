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
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif



#include "lixa_trace.h"
#include "status_record.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/** Initial file size (unit = pages) */
#define LIXA_STATE_LOG_FILE_SIZE_DEFAULT    250



/**
 * Possible statuses of a state log
 */
enum lixa_state_log_status_e {
    STATE_LOG_UNDEFINED = 0,
    STATE_LOG_FORMATTED,
    STATE_LOG_USED,
    STATE_LOG_FULL,
    STATE_LOG_EXTENDED,
    STATE_LOG_CLOSED,
    STATE_LOG_DISPOSED
};



/**
 * LIXA State Log data type ("class")
 */
typedef struct lixa_state_log_s {
    /**
     * Current status of the state log
     */
    enum lixa_state_log_status_e     status;
    /**
     * File descriptor associated to the underlying file
     */
    int                              fd;
    /**
     * Persistency flags associated to the underlying file
     */
    int                              pers_flags;
    /**
     * Name of the associated underlying file
     */
    char                            *pathname;
    /**
     * Size of the buffer used to manage I/O; unit is "number of bytes"
     */
    size_t                           buffer_size;
    /**
     * Buffer used to manage I/O
     */
    void                            *buffer;
    /**
     * A single memory page used for special purposes like formatting
     */
    void                            *single_page;
    /**
     * An array used to store the positions of all the changed blocks in the
     * state file
     */
    uint32_t                        *block_ids;
    /**
     * The size of the array, the maximum number of block_ids that can be
     * copied in the buffer
     */
    uint32_t                         size_of_block_ids;
    /**
     * The number of block_id values in block_ids array
     */
    uint32_t                         number_of_block_ids;
    /**
     * System page file
     */
    size_t                           system_page_size;
    /**
     * Number of records that can be written in a page
     */
    size_t                           number_of_records_per_page;
    /**
     * Total size of the underlying file in bytes
     */
    off_t                            file_total_size;
    /**
     * Offset of the first writable page in the file
     */
    off_t                            file_offset;
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
     * Digest to check record integrity
     */
    md5_digest_t                     digest;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a StateLog object to manage a state log
     * @param[in,out] this object to be initialized
     * @param[in] pathname that must be used to open or create the underlying
     *            file
     * @param[in] system_page_size size of a memory page
     * @param[in] max_records_in_buffer number of records that can be kept in
     *            the I/O buffer
     * @param[in] o_direct_bool activates O_DIRECT POSIX flag
     * @param[in] o_dsync_bool activates O_DSYNC POSIX flag
     * @param[in] o_rsync_bool activates O_RSYNC POSIX flag
     * @param[in] o_sync_bool activates O_SYNC POSIX flag
     * @return a reason code
     */
    int lixa_state_log_init(lixa_state_log_t *this,
                            const char *pathname,
                            size_t system_page_size,
                            size_t buffer_size,
                            int o_direct_bool,
                            int o_dsync_bool,
                            int o_rsync_bool,
                            int o_sync_bool);



    /**
     * Create a new underlying file for the state log object
     * @param[in,out] this current state file object
     * @return a reason code
     */
    int lixa_state_log_create_new_file(lixa_state_log_t *this);



    /**
     * Check if the underlying file exists and can be opened
     * @param[in] this current state log object
     * @return a reason code, LIXA_RC_OK if the file exists and can be opened
     */
    int lixa_state_log_exist_file(lixa_state_log_t *this);

    
    
    /**
     * Cleanup a StateLog object
     */
    int lixa_state_log_clean(lixa_state_log_t *this);



    /**
     * Return the pathname of the file associated to the state log object
     */
    static inline const char* lixa_state_log_get_pathname(
        const lixa_state_log_t *this) {
        return this->pathname;
    }

    
    
    /**
     * Flush the records to the underlying file
     * @param[in,out] this state log object
     * @param[in] status_records @@@ move to lixa_state_file_t?
     * @return a reason code
     */
    int lixa_state_log_flush(lixa_state_log_t *this,
                             status_record_t *status_records);



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
     * Returns a boolean related to buffer full condition 
     * @param[in,out] this state log object
     * @return a boolean value
     */     
    int lixa_state_log_is_buffer_full(lixa_state_log_t *this);


    
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
