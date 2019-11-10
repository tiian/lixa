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
#ifndef LIXA_STATE_LOG_FILE_H
# define LIXA_STATE_LOG_FILE_H



#include "config.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/** Initial file size (unit = byte) */
#define LIXA_STATE_LOG_FILE_SIZE_DEFAULT     (50*LIXA_SYSTEM_PAGE_SIZE)
/** Incremental file size (unit = byte) */
#define LIXA_STATE_LOG_FILE_SIZE_INCREMENT   (25*LIXA_SYSTEM_PAGE_SIZE)

/**
 * Suffix appended when a new log file is created
 */
extern const char *LIXA_STATE_LOG_FILE_SUFFIX;



/**
 * Possible statuses of a state log file
 */
enum lixa_state_log_file_status_e {
    STATE_LOG_FILE_UNDEFINED = 0,
    STATE_LOG_FILE_FORMATTED,
    STATE_LOG_FILE_USED,
    STATE_LOG_FILE_FULL,
    STATE_LOG_FILE_EXTENDED,
    STATE_LOG_FILE_CLOSED,
    STATE_LOG_FILE_DISPOSED
};



/**
 * LIXA State Log File data type ("class"): a physical representation of a log
 * inside a file
 */
typedef struct lixa_state_log_file_s {
    /**
     * Current status of the state log
     */
    enum lixa_state_log_file_status_e    status;
    /**
     * File descriptor associated to the underlying file
     */
    int                                  fd;
    /**
     * Persistency flags associated to the underlying file
     */
    int                                  pers_flags;
    /**
     * Name of the associated underlying file
     */
    char                                *pathname;
    /**
     * This internal struct is synchronized with a mutex to avoid inconsistent
     * states between the main thread and the flusher thread
     */
    struct {
        /**
         * Mutex used to protect the object when accessed concurrently by the
         * flusher thread
         */
        pthread_mutex_t                   mutex;
        /**
         * Total size of the underlying file in bytes
         */
        off_t                             total_size;
        /**
         * Offset of the first writable page in the file
         */
        off_t                             offset;
        /**
         * Number of bytes reserved after offset due to an in progress
         * asynchronous flushing or generic file operation
         */
        off_t                             reserved;
    } synch;
} lixa_state_log_file_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a StateLogFile object to manage a state log file
     * @param[in,out] this object to be initialized
     * @param[in] pathname that must be used to open or create the file
     * @param[in] pers_flags persistency flags associated to the file
     * @return a reason code
     */
    int lixa_state_log_file_init(lixa_state_log_file_t *this,
                                 const char *pathname, int pers_flags);



    /**
     * Cleanup a StateLogFile object
     */
    int lixa_state_log_file_clean(lixa_state_log_file_t *this);



    /**
     * Check if the file exists and can be opened
     * @param[in] this current state log file object
     * @return a reason code, LIXA_RC_OK if the file exists and can be opened
     */
    int lixa_state_log_file_exists(lixa_state_log_file_t *this);

    
    /**
     * Create the underlying file for the state log file object
     * @param[in,out] this state log file object
     * @param[in,out] single_page of memory that's page aligned and that can
     *                be used for buffering
     * @return a reason code
     */
    int lixa_state_log_file_create_new(lixa_state_log_file_t *this,
                                       void *single_page);



    /**
     * Extend a state log file
     * @param[in,out] this state log file object
     * @return a reason code
     */
    int lixa_state_log_file_extend(lixa_state_log_file_t *this);

    

    /**
     * Write a buffer to the log file
     * @param[in,out] this state log file object
     * @param[in] buffer to be written
     * @param[in] number_of_pages to write
     * @return a reason code
     */
    int lixa_state_log_file_write(lixa_state_log_file_t *this,
                                  const void *buffer,
                                  size_t number_of_pages);

    

    /**
     * Return the pathname of the file associated to the state log file
     * object
     */
    static inline const char* lixa_state_log_file_get_pathname(
        const lixa_state_log_file_t *this) {
        return this->pathname;
    }

    

    /**
     * Return the file descriptor
     */
    static inline int lixa_state_log_file_get_fd(lixa_state_log_file_t *this)
    {
        return this->fd;
    }


    
    /**
     * Return the total size of the file in bytes
     */
    static inline off_t lixa_state_log_file_get_total_size(
        lixa_state_log_file_t *this)
    {
        off_t total_size;
        pthread_mutex_lock(&this->synch.mutex);
        total_size = this->synch.total_size;
        pthread_mutex_unlock(&this->synch.mutex);
        return total_size;
    }


    
    /**
     * Return the reserved size of the file in bytes
     */
    static inline off_t lixa_state_log_file_get_reserved(
        lixa_state_log_file_t *this)
    {
        off_t reserved;
        pthread_mutex_lock(&this->synch.mutex);
        reserved = this->synch.reserved;
        pthread_mutex_unlock(&this->synch.mutex);
        return reserved;
    }


    
    /**
     * Return the offset of the first writable page in the file
     */
    static inline off_t lixa_state_log_file_get_offset(
        lixa_state_log_file_t *this)
    {
        off_t offset;
        pthread_mutex_lock(&this->synch.mutex);
        offset = this->synch.offset;
        pthread_mutex_unlock(&this->synch.mutex);
        return offset;
    }


    /**
     * Return the free space really available in the file, taking into
     * consideration even reserved space in the event of a
     * scheduled/in progress write operation
     */
    static inline off_t lixa_state_log_file_get_free_space(
        lixa_state_log_file_t *this)
    {
        off_t free_space;
        pthread_mutex_lock(&this->synch.mutex);
        free_space = this->synch.total_size - this->synch.offset -
            this->synch.reserved;
        pthread_mutex_unlock(&this->synch.mutex);
        return free_space;
    }


    
    /**
     * Set a reserved space in the file, at the right of offset
     * @param[in,out] this state log file object
     * @param[in] reserved space in byte that must be reserved
     * @return a reason code
     */
    int lixa_state_log_file_set_reserved(lixa_state_log_file_t *this,
                                         off_t reserved);



    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_STATE_LOG_FILE_H */
