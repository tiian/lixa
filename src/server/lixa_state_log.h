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



#include "lixa_trace.h"
#include "config.h"



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
/** Initial buffer size (unit = pages) */
#define LIXA_STATE_LOG_BUFFER_SIZE_DEFAULT   10



/**
 * Possible statuses of a state log
 */
enum lixa_state_log_status_e {
    UNDEFINED = 0,
    FORMATTED,
    USED,
    FULL,
    EXTENDED,
    CLOSED,
    DISPOSED
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
     * System page file
     */
    size_t                           page_size;
    /**
     * Buffer used to manage I/O
     */
    void                            *buffer;
    /**
     * Size of the buffer used to manage I/O
     */
    size_t                           buffer_size;
} lixa_state_log_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a StateLog object to manage a state log
     * @param[in,out] this object to be initialized
     * @param[in] pathname that must be used to open or create the underlying
     *            file
     * @param[in] o_direct_bool activates O_DIRECT POSIX flag
     * @param[in] o_dsync_bool activates O_DSYNC POSIX flag
     * @param[in] o_rsync_bool activates O_RSYNC POSIX flag
     * @param[in] o_sync_bool activates O_SYNC POSIX flag
     * @return a reason code
     */
    int lixa_state_log_init(lixa_state_log_t *this,
                            const char *pathname,
                            int o_direct_bool,
                            int o_dsync_bool,
                            int o_rsync_bool,
                            int o_sync_bool);



    /**
     * Create a new underlying file for the state log object
     * @param[in,out] this current state file object
     * @param[in] pathname that must be used to create the underlying file
     * @param[in] flags that must be used to create the file
     * @return a reason code
     */
    int lixa_state_log_create_new_file(lixa_state_log_t *this,
                                       const char *pathname,
                                       int flags);

    

    /**
     * Cleanup a StateLog object
     */
    int lixa_state_log_clean(lixa_state_log_t *this);


    
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
