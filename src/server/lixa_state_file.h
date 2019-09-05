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
#ifndef LIXA_STATE_FILE_H
# define LIXA_STATE_FILE_H



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



/**
 * Possible statuses of a state file
 */
enum lixa_state_file_status_e {
    STATE_FILE_UNDEFINED = 0,
    STATE_FILE_FORMATTED,
    STATE_FILE_USED,
    STATE_FILE_FULL,
    STATE_FILE_EXTENDED,
    STATE_FILE_COPY_SOURCE,
    STATE_FILE_COPY_TARGET,
    STATE_FILE_SYNCH,
    STATE_FILE_CLOSED,
    STATE_FILE_DISPOSED
};



/**
 * LIXA State File data type ("class")
 */
typedef struct lixa_state_file_s {
    /**
     * Current status of the state file
     */
    enum lixa_state_file_status_e     status;
    /**
     * File descriptor associated to the underlying file
     */
    int                              fd;
} lixa_state_file_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a StateFile object to manage a state file
     * @param[in,out] this object to be initialized
     * @param[in] pathname that must be used to open or create the underlying
     *            file
     * @return a reason code
     */
    int lixa_state_file_init(lixa_state_file_t *this,
                             const char *pathname);

    

    /**
     * Create a new underlying file for the state file object
     * @param[in,out] this current state file object
     * @param[in] pathname that must be used to create the underlying file
     * @return a reason code
     */
    int lixa_state_file_create_new_file(lixa_state_file_t *this,
                                        const char *pathname);

    

    /**
     * Cleanup a StateFile object
     */
    int lixa_state_file_clean(lixa_state_file_t *this);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_STATE_FILE_H */
