/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
/** Initial file size (unit = byte) */
#define LIXA_STATE_LOG_FILE_SIZE_DEFAULT     (50*LIXA_SYSTEM_PAGE_SIZE)
/** Incremental file size (unit = byte) */
#define LIXA_STATE_LOG_FILE_SIZE_INCREMENT   (25*LIXA_SYSTEM_PAGE_SIZE)



/**
 * Number of record that can be stored in a memory page
 */
extern size_t LIXA_STATE_LOG_RECORDS_PER_PAGE;
/**
 * Suffix appended when a new log file is created
 */
extern const char *LIXA_STATE_LOG_FILE_SUFFIX;



/**
 * Possible statuses of a state log
 */
enum lixa_state_log_status_e {
    STATE_LOG_UNDEFINED = 0,
    STATE_LOG_FORMATTED,
    STATE_LOG_OPENED,
    STATE_LOG_USED,
    STATE_LOG_FULL,
    STATE_LOG_EXTENDING,
    STATE_LOG_CLOSED,
    STATE_LOG_DISPOSED
};



/*
 * Just a declaration
 */
typedef struct lixa_state_table_s lixa_state_table_t;



/**
 * Struct used to access records in the state log file
 */
struct lixa_state_log_record_s {
    /**
     * CRC to check record integrity: this MUST BE the first field in the
     * struct
     */
    uint32_t                         crc32;
    /**
     * The position of the record in the state table
     */
    uint32_t                         original_slot;
    /**
     * Unique identifier of the record
     */
    lixa_word_t                      id;
    /**
     * Time of the record flush operation (many records can have the same
     * timestamp because flushing can happen inside a batch)
     */
    struct timeval                   timestamp;
    /**
     * The record that must be persisted/retrieved
     */
    lixa_state_record_t              record;
};



/**
 * LIXA State Log data type ("class")
 */
typedef struct lixa_state_log_s {
    /**
     * Current status of the state log
     */
    enum lixa_state_log_status_e      status;
    /**
     * Name of the associated underlying file
     */
    char                             *pathname;
    /**
     * File descriptor associated to the underlying file
     */
    int                               fd;
    /**
     * Persistency flags associated to the underlying file
     */
    int                               flags;
    /**
     * Last record id used in the state log; 0 has special meaning of resetted
     * record, unused record
     */
    lixa_word_t                       last_record_id;
    /**
     * This internal struct is used to save the most relevant info read from
     * log file; @ref lixa_state_log_open_file sets the values during start-up
     */
    struct {
        /**
         * Number of (valid) records available in the log file
         */
        off_t                         number_of_records;
        /**
         * Unique identifier of the first read record
         */
        lixa_word_t                   first_record_id;
        /**
         * Time of first read record flush operation
         */
        struct timeval                first_record_timestamp;
        /**
         * Unique identifier of the last read record
         */
        lixa_word_t                   last_record_id;
        /**
         * Time of last read record flush operation
         */
        struct timeval                last_record_timestamp;
    } read_info;
    /**
     * This internal struct is synchronized with a mutex to avoid inconsistent
     * states between the main thread and the flusher thread
     */
    struct {
        /**
         * Mutex used to protect the object when accessed concurrently by the
         * flusher thread
         */
        pthread_mutex_t               mutex;
        /**
         * Total size of the underlying file in bytes
         */
        off_t                         total_size;
        /**
         * Offset of the first writable page in the file
         */
        off_t                         offset;
        /**
         * Number of bytes reserved after offset due to an in progress
         * asynchronous flushing or generic file operation
         */
        off_t                         reserved;
    } file_synchronizer;
} lixa_state_log_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    static inline uint32_t lixa_state_log_record_get_crc32(
        const struct lixa_state_log_record_s *r) {
        return r->crc32;
    }
    

    
    static inline uint32_t lixa_state_log_record_get_original_slot(
        const struct lixa_state_log_record_s *r) {
        return r->original_slot;
    }
    

    
    static inline lixa_word_t lixa_state_log_record_get_id(
        const struct lixa_state_log_record_s *r) {
        return r->id;
    }
    

    
    static inline const struct timeval *lixa_state_log_record_get_timestamp(
        const struct lixa_state_log_record_s *r) {
        return &r->timestamp;
    }



    static inline const lixa_state_record_t *
    lixa_state_log_record_get_record(const struct lixa_state_log_record_s *r) {
        return &r->record;
    }



    /**
     * Check if a log record is valid (TRUE) or corrupted (FALSE)
     * @param[in] r log record
     * @return a boolean value
     */
    static inline int lixa_state_log_record_is_valid(
        const struct lixa_state_log_record_s *r) {
        return  lixa_crc32(
            (const uint8_t *)r + sizeof(uint32_t),
            sizeof(struct lixa_state_log_record_s) - sizeof(uint32_t)) ==
            r->crc32;
    }


    
    /**
     * Return the human readable string associated to a status
     */
    const char *lixa_state_log_status_string(
        enum lixa_state_log_status_e status);

    
    
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
     * @param[in] read_only must be TRUE if the state log will be used on
     *            read-only mode
     * @return a reason code
     */
    int lixa_state_log_init(lixa_state_log_t *this,
                            const char *path_prefix,
                            size_t max_buffer_size,
                            int o_direct_bool, int o_dsync_bool,
                            int o_rsync_bool, int o_sync_bool, int read_only);



    /**
     * Create a new underlying file for the state log object
     * @param[in,out] this current state file object
     * @param[in,out] single_page of memory that's page aligned and that can
     *                be used for buffering
     * @param[in] log_size in bytes of the log state file
     * @return a reason code
     */
    int lixa_state_log_create_new_file(lixa_state_log_t *this,
                                       void *single_page, off_t log_size);



    /**
     * Open an existing state log file for initial reading
     * @param[in,out] this state log object
     * @param[in] buffer_size for log reading
     * @return a reason code
     */
    int lixa_state_log_open_file(lixa_state_log_t *this, size_t buffer_size);

    

    /**
     * Reopen an existing state log file for writing, applying write specific
     * flags
     * @param[in,out] this state log object
     * @return a reason code
     */     
    int lixa_state_log_reopen_file(lixa_state_log_t *this);

    
    
    /**
     * Retrieve a record from the underlying file
     * @param[in,out] this state log object
     * @param[in] pos of the record in the file
     * @param[in] prev_pos was the position of the previous read record
     * @param[out] record pointer to the position of the record in the buffer
     * @param[in,out] buffer of memory that's page aligned and that can
     *                be used for log buffering
     * @param[in] pages_per_buffer number of pages in a single IO buffer
     * @return a reason code
     */
    int lixa_state_log_read_file(lixa_state_log_t *this, off_t pos,
                                 off_t prev_pos,
                                 const struct lixa_state_log_record_s **record,
                                 void *buffer, off_t pages_per_buffer);
                               

    
    /**
     * Check if an underlying file exists and can be opened
     * @param[in] this current state log object
     * @return a reason code, LIXA_RC_OK if the file exists and can be opened
     */
    int lixa_state_log_file_exist(lixa_state_log_t *this);
        


    /**
     * Reuse a previously disposed state log file
     * @param[in,out] this current state log object
     * @return a reason code
     */
    int lixa_state_log_reuse(lixa_state_log_t *this);

    

    /**
     * Put in "closed" state a Log object
     * @param[in,out] this current state log object
     * @return a reason code
     */
    int lixa_state_log_close(lixa_state_log_t *this);
    

    
    /**
     * Shutdown a State Log object and make durable the current status
     * @param[in,out] this current state log object
     * @return a reason code
     */
    int lixa_state_log_shutdown(lixa_state_log_t *this);
    

    
    /**
     * Cleanup a StateLog object
     */
    int lixa_state_log_clean(lixa_state_log_t *this);



    /**
     * Return the pathname of the file associated to the state log file
     * object
     */
    static inline const char* lixa_state_log_get_pathname(
        const lixa_state_log_t *this) {
        return this->pathname;
    }



    /**
     * Return the id of the last record in the log
     */
    static inline lixa_word_t lixa_state_log_get_last_record_id(
        const lixa_state_log_t *this) {
        return this->last_record_id;
    }



    /**
     * Set the id of the last record in the (previous) log: used during
     * log switching
     */
    static inline void lixa_state_log_set_last_record_id(
        lixa_state_log_t *this, lixa_word_t last_record_id) {
        this->last_record_id = last_record_id;
    }

    

    /**
     * Return the id of the next record that will be appended in the log;
     * updated the id of the last record as well
     * @param[in,out] this state log object
     * @return the id of the next record
     */
    static inline lixa_word_t lixa_state_log_get_next_record_id(
        lixa_state_log_t *this) {
        if (0 == ++(this->last_record_id))
            /* 0 is a reserved value and must be skipped */
            ++(this->last_record_id);
        return this->last_record_id;
    }


    
    /**
     * Extend the underlying file
     * @param[in,out] this state log object
     * @return a reason code
     */
    int lixa_state_log_extend(lixa_state_log_t *this);



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
     * Return the total size of the underlying file in bytes
     */
    static inline off_t lixa_state_log_get_total_size(lixa_state_log_t *this)
    {
        off_t total_size;
        pthread_mutex_lock(&this->file_synchronizer.mutex);
        total_size = this->file_synchronizer.total_size;
        pthread_mutex_unlock(&this->file_synchronizer.mutex);
        return total_size;
    }


    
    /**
     * Return the reserved size of the underlying file in bytes
     */
    static inline off_t lixa_state_log_get_reserved(lixa_state_log_t *this)
    {
        off_t reserved;
        pthread_mutex_lock(&this->file_synchronizer.mutex);
        reserved = this->file_synchronizer.reserved;
        pthread_mutex_unlock(&this->file_synchronizer.mutex);
        return reserved;
    }


    
    /**
     * Set a reserved space in the log file, at the right offset
     * @param[in,out] this state log object
     * @param[in] reserved space in byte that must be reserved
     * @return a reason code
     */
    int lixa_state_log_set_reserved(lixa_state_log_t *this,
                                    off_t reserved);



    /**
     * Return the offset of the first writable page in the underlying file
     */
    static inline off_t lixa_state_log_get_offset(lixa_state_log_t *this)
    {
        off_t offset;
        pthread_mutex_lock(&this->file_synchronizer.mutex);
        offset = this->file_synchronizer.offset;
        pthread_mutex_unlock(&this->file_synchronizer.mutex);
        return offset;
    }



    /**
     * Return the currently used size of the underlying file in bytes
     */
    static inline off_t lixa_state_log_get_used(lixa_state_log_t *this)
    {
        off_t used;
        pthread_mutex_lock(&this->file_synchronizer.mutex);
        used = this->file_synchronizer.offset +
            this->file_synchronizer.reserved;
        pthread_mutex_unlock(&this->file_synchronizer.mutex);
        return used;
    }




    /**
     * Get statistics about the state log file; the number of used bytes is the
     * sum of "offset" and "reserved"
     * @param[in] this state log object
     * @param[out] total_size size of the file in bytes
     * @param[out] offset of the first writable page in the underlying file
     * @param[out] reserved space in byte that's reserved for an in flight
     *             flushing
     */
    static inline void lixa_state_log_get_file_stats(
        lixa_state_log_t *this, off_t *total_size, off_t *offset,
        off_t *reserved) {
        pthread_mutex_lock(&this->file_synchronizer.mutex);
        *total_size = this->file_synchronizer.total_size;
        *offset  = this->file_synchronizer.offset;
        *reserved = this->file_synchronizer.reserved;
        pthread_mutex_unlock(&this->file_synchronizer.mutex);
    }


    
    /**
     * Return the free space really available in the file, taking into
     * consideration even reserved space in the event of a
     * scheduled/in progress write operation
     */
    static inline off_t lixa_state_log_get_free_space(lixa_state_log_t *this)
    {
        off_t free_space;
        pthread_mutex_lock(&this->file_synchronizer.mutex);
        free_space = this->file_synchronizer.total_size -
            this->file_synchronizer.offset -
            this->file_synchronizer.reserved;
        LIXA_TRACE(("lixa_state_log_get_free_space: total_size=" OFF_T_FORMAT
                    ", offset=" OFF_T_FORMAT ", reserved=" OFF_T_FORMAT
                    ", free_space=" OFF_T_FORMAT "\n",
                    this->file_synchronizer.total_size,
                    this->file_synchronizer.offset,
                    this->file_synchronizer.reserved, free_space));
        pthread_mutex_unlock(&this->file_synchronizer.mutex);
        return free_space;
    }


    
    /**
     * Write a buffer to the state log
     * @param[in,out] this state log object
     * @param[in] buffer to be written
     * @param[in] number_of_pages to write
     * @param[in] switch_after_write boolean flag to ask for the fsync of the
     *            file and call to @ref lixa_state_log_close to put the 
     *            state log object in "closed" status
     * @return a reason code
     */
    int lixa_state_log_write(lixa_state_log_t *this, const void *buffer,
                             size_t number_of_pages, int switch_after_write);

    

    /**
     * Set a new status for the state log: before setting, it checks if the
     * transition is valid or not
     * @param[in,out] this state log object
     * @param[in] new_status to be set
     * @param[in] dry_run boolean value, TRUE if function must only test the
     *            status change, FALSE if real switch must be performed
     * @return a reason code
     */
    int lixa_state_log_set_status(lixa_state_log_t *this,
                                  enum lixa_state_log_status_e new_status,
                                  int dry_run);


    
    /**
     * Return the current status of the state table
     */
    static inline enum lixa_state_log_status_e
    lixa_state_log_get_status(const lixa_state_log_t *this) {
        return this->status;
    }



    /**
     * Information read from the underlying file at start-up time
     * @param[in] this state log object
     * @return the number of valid records in the file
     */
    static inline off_t lixa_state_log_get_ri_number_of_records(
        const lixa_state_log_t *this) {
        return NULL == this ? 0 : this->read_info.number_of_records;
    }


    
    /**
     * Information read from the underlying file at start-up time
     * @param[in] this state log object
     * @return the id of the first record in the file
     */
    static inline lixa_word_t lixa_state_log_get_ri_first_record_id(
        const lixa_state_log_t *this) {
        return NULL == this ? 0 : this->read_info.first_record_id;
    }


    
    /**
     * Information read from the underlying file at start-up time
     * @param[in] this state log object
     * @return the timestamp associated to the first record in the file
     */
    static inline const struct timeval
    *lixa_state_log_get_ri_first_record_timestamp(
    const lixa_state_log_t *this) {
        return NULL == this ? NULL : &this->read_info.first_record_timestamp;
    }


    
    /**
     * Information read from the underlying file at start-up time
     * @param[in] this state log object
     * @return the id of the last record in the file
     */
    static inline lixa_word_t lixa_state_log_get_ri_last_record_id(
        const lixa_state_log_t *this) {
        return NULL == this ? 0 : this->read_info.last_record_id;
    }


    
    /**
     * Information read from the underlying file at start-up time
     * @param[in] this state log object
     * @return the timestamp associated to the last record in the file
     */
    static inline const struct timeval
    *lixa_state_log_get_ri_last_record_timestamp(
    const lixa_state_log_t *this) {
        return NULL == this ? NULL : &this->read_info.last_record_timestamp;
    }


    
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
