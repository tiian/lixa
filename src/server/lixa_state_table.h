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
#ifndef LIXA_STATE_TABLE_H
# define LIXA_STATE_TABLE_H



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
 * Possible statuses of a state table
 */
enum lixa_state_table_status_e {
    STATE_TABLE_UNDEFINED = 0,
    STATE_TABLE_FORMATTED,
    STATE_TABLE_USED,
    STATE_TABLE_FULL,
    STATE_TABLE_EXTENDED,
    STATE_TABLE_COPY_SOURCE,
    STATE_TABLE_COPY_TARGET,
    STATE_TABLE_SYNCH,
    STATE_TABLE_CLOSED,
    STATE_TABLE_DISPOSED
};



/**
 * LIXA State Table data type ("class")
 */
typedef struct lixa_state_table_s {
    /**
     * Current status of the state table
     */
    enum lixa_state_table_status_e    status;
    /**
     * File descriptor associated to the underlying file
     */
    int                              fd;
    /**
     * Open flags associated to the underlying file
     */
    int                              flags;
    /**
     * Name of the associated underlying file
     */
    char                            *pathname;
} lixa_state_table_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a StateTable object to manage a state table
     * @param[in,out] this object to be initialized
     * @param[in] pathname that must be used to open or create the underlying
     *            file
     * @return a reason code
     */
    int lixa_state_table_init(lixa_state_table_t *this,
                             const char *pathname);

    

    /**
     * Create a new underlying file for the state table object
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_create_new_file(lixa_state_table_t *this);

    

    /**
     * Synchronize the underlying state file
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_synchronize(lixa_state_table_t *this);


    
    /**
     * Close the underlying state file
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_close(lixa_state_table_t *this);


    
    /**
     * Check if the underlying file exists and can be opened
     * @param[in] this state table object
     * @return a reason code, LIXA_RC_OK if the file exists and can be opened
     */
    int lixa_state_table_exist_file(lixa_state_table_t *this);

    
    
    /**
     * Cleanup a StateTable object
     */
    int lixa_state_table_clean(lixa_state_table_t *this);


    
    /**
     * Return the pathname of the file associated to the state table object
     */
    static inline const char* lixa_state_table_get_pathname(
        const lixa_state_table_t *this) {
        return this->pathname;
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



#endif /* LIXA_STATE_TABLE_H */
