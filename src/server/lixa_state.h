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



#include "lixa_trace.h"
#include "lixa_state_file.h"
#include "lixa_state_log.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



#define LIXA_STATE_FILES   3



/**
 * Suffix appended when a new state file is created
 */
extern const char *LIXA_STATE_FILE_SUFFIX;
/**
 * Suffix appended when a new log file is created
 */
extern const char *LIXA_STATE_LOG_SUFFIX;



/**
 * LIXA State data type ("class")
 */
typedef struct lixa_state_s {
    /**
     * System page file
     */
    size_t                system_page_size;
    /**
     * Array of state files
     */
    lixa_state_file_t     files[LIXA_STATE_FILES];
    /**
     * Array of state logs
     */
    lixa_state_log_t      logs[LIXA_STATE_FILES];
} lixa_state_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    
    /**
     * Initialize a State object to manage state server persistence
     * @param[in,out] this object to be initialized
     * @param[in] path_prefix of the state and log files
     * @return a reason code
     */
    int lixa_state_init(lixa_state_t *this, const char *path_prefix);



    /**
     * Create new state and log files for this state object
     * @param[in,out] this state object
     * @param[in] path_prefix of the state and log files
     * @return a reason code
     */
    int lixa_state_create_new(lixa_state_t *this, const char *path_prefix);

    
    
    /**
     * Cleanup a State object
     */
    int lixa_state_clean(lixa_state_t *this);


    
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
