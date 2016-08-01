/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
 */
#ifndef LIXA_UTILS_H
# define LIXA_UTILS_H



#include <config.h>



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef HAVE_TIME
# include <time.h>
#endif



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
/**
 * If @ref lixa_get_program_name is not able to retrieve the current program
 * name, it will retrieve this value
 */
#define DEFAULT_PROGRAM_NAME "lixac"



/**
 * Basic structure for object @ref lixa_timer_t
 */
struct lixa_timer_s {
    /**
     * Start time
     */
    struct timeval begin_time;
    /**
     * Stop time
     */
    struct timeval end_time;
};



/**
 * A timer object used to time an operation
 */
typedef struct lixa_timer_s lixa_timer_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Print software version info
     * @param stream IN stdio stream to use for fprintf function
     */
    void lixa_print_version(FILE *stream);
    


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



    /**
     * Retrieve the name of the current running program
     * NOTE: this function is strictly PLATFORM DEPENDENT and returns a
     * default constant value on a system there is no implementation for
     * @param buf OUT buffer will contain the output string; the returned
     *            string is NULL TERMINATED, ever
     * @param buf_size IN buffer size (trailing zero uses 1 char in the buffer)
     * @return a standardized reason code
     */
    int lixa_get_program_name(char *buf, size_t buf_size);



    /**
     * Micro seconds sleep based on select call
     * @param usec IN micro seconds
     */
    void lixa_micro_sleep(long usec);



    /**
     * Start a timing
     * @param lt IN/OUT timer object
     */
    void lixa_timer_start(lixa_timer_t *lt);


    
    /**
     * Stop a timing
     * @param lt IN/OUT timer object
     */
    void lixa_timer_stop(lixa_timer_t *lt);



    /**
     * Return the number of microseconds between @ref lixa_timer_start and
     * @ref lixa_timer_stop.
     * Warning: undefined result are if the timer was not properly started and
     * stopped
     * @param lt IN timer object
     * @return elapsed number of microseconds
     */
    static inline long lixa_timer_get_diff(const lixa_timer_t *lt) {
        return (lt->end_time.tv_sec - lt->begin_time.tv_sec) * 1000000 +
            lt->end_time.tv_usec - lt->begin_time.tv_usec;
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



#endif /* LIXA_UTILS_H */
