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
#ifndef LIXA_STATE_COMMON_H
# define LIXA_STATE_COMMON_H



#include "config.h"



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include "lixa_utils.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Number of state tables and log files; can NOT be less than 3
 */
#define LIXA_STATE_TABLES   3



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Conversion from buffer size to number of pages
     * @param[in] buffer_size in bytes
     * @return number of pages
     */
    static inline size_t lixa_state_common_buffer2pages(size_t buffer_size)
    {
        return (buffer_size / LIXA_SYSTEM_PAGE_SIZE) +
            (buffer_size % LIXA_SYSTEM_PAGE_SIZE ? 1 : 0);
    }



    /**
     * Conversion from number of pages to buffer size in bytes
     * @param[in] number_of_pages
     * @return the buffer size in bytes
     */
    static inline size_t lixa_state_common_pages2buffer(size_t number_of_pages)
    {
        return number_of_pages * LIXA_SYSTEM_PAGE_SIZE;
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



#endif /* LIXA_STATE_COMMON_H */
