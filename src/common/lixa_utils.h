/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_UTILS_H
# define LIXA_UTILS_H



#include <config.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_UTILS



/**
 * Minimun length to store a timestamp using ISO standard format; it comprises
 * the null terminator
 */
#define ISO_TIMESTAMP_BUFFER_SIZE 32



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve an ISO formatted timestamp from a timeval struct;
     * @param tv IN the timeval struct retrieved with gettimeofday system
     *              function
     * @param buf OUT the buffer will filled with the ISO formatted timestamp
     *                (it must be at least @ref ISO_TIMESTAMP_BUFFER_SIZE
     *                characters long)
     * @param buf_size IN the buffer size
     * @return a standardized reason code
     */
    int lixa_utils_iso_timestamp(const struct timeval *tv, char *buf,
                                 size_t buf_size);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_UTILS_H */
