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
#define LIXA_STATE_LOG_FILE_SIZE_DEFAULT     (256*LIXA_SYSTEM_PAGE_SIZE)



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
     * Total size of the underlying file in bytes
     */
    off_t                                total_size;
    /**
     * Offset of the first writable page in the file
     */
    off_t                                offset;
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
        return this->total_size;
    }


    
    /**
     * Return the offset of the first writable page in the file
     */
    static inline off_t lixa_state_log_file_get_offset(
    lixa_state_log_file_t *this)
    {
        return this->offset;
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



#endif /* LIXA_STATE_LOG_FILE_H */
