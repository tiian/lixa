/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXA_UTILS_H
# define LIXA_UTILS_H



#include "config.h"



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



#include "lixa_defines.h"



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
 * Size of a memory page in bytes
 */
extern size_t LIXA_SYSTEM_PAGE_SIZE;



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



/**
 * Number of char necessary for a session id: too many chars are not practical
 * during troubleshooting
 */
#define LIXA_SESSION_ID_LENGTH      (7+1)



/**
 * Basic structure for object @ref lixa_session_t
 */
struct lixa_session_s {
    /**
     * C string representation of a LIXA session id (null terminator included!)
     */
    char        sid[LIXA_SESSION_ID_LENGTH];
};



/**
 * LIXA client/server session identifier
 */
typedef struct lixa_session_s lixa_session_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Print software version info
     * @param[out] stream stdio stream to use for fprintf function
     */
    void lixa_print_version(FILE *stream);
    


    /**
     * Retrieve an ISO formatted timestamp from a timeval struct;
     * @param[in] tv the timeval struct retrieved with gettimeofday system
     *              function
     * @param[out] buf the buffer will filled with the ISO formatted timestamp
     *                (it must be at least @ref ISO_TIMESTAMP_BUFFER_SIZE
     *                characters long)
     * @param[in] buf_size the buffer size
     * @return a standardized reason code
     */
    int lixa_utils_iso_timestamp(const struct timeval *tv, char *buf,
                                 size_t buf_size);



    /**
     * Retrieve the name of the current running program
     * NOTE: this function is strictly PLATFORM DEPENDENT and returns a
     * default constant value on a system there is no implementation for
     * @param[out] buf buffer that will contain the output string; the returned
     *            string is NULL TERMINATED, ever
     * @param[in] buf_size buffer size (trailing zero uses 1 char in the
     *            buffer)
     * @return a standardized reason code
     */
    int lixa_get_program_name(char *buf, size_t buf_size);



    /**
     * Micro seconds sleep based on select call
     * @param[in] usec micro seconds
     */
    void lixa_micro_sleep(long usec);



    /**
     * Start a timing
     * @param[in,out] lt timer object
     */
    void lixa_timer_start(lixa_timer_t *lt);


    
    /**
     * Stop a timing
     * @param[in,out] lt timer object
     */
    void lixa_timer_stop(lixa_timer_t *lt);



    /**
     * Return the number of microseconds between @ref lixa_timer_start and
     * @ref lixa_timer_stop.
     * Warning: undefined result are if the timer was not properly started and
     * stopped
     * @param[in] lt timer object
     * @return elapsed number of microseconds
     */
    static inline long lixa_timer_get_diff(const lixa_timer_t *lt) {
        return (lt->end_time.tv_sec - lt->begin_time.tv_sec) * 1000000 +
            lt->end_time.tv_usec - lt->begin_time.tv_usec;
    }
    


    /**
     * Reset a session object
     * @param[out] session object to be resetted
     */
    void  lixa_session_reset(lixa_session_t *session);
    

    
    /**
     * Initialize a session object
     * @param[out] session object to be initialized
     * @param[in] fd file descriptor of the TCP/IP connection
     * @param[in] client boolean value, TRUE if the file descriptor is on the
     *           client side, FALSE if the file descriptor is on the server
     *           side
     * @return a reason code
     */
    int lixa_session_init(lixa_session_t *session, int fd, int client);



    /**
     * Return the C string representation of the session id
     */
    static inline const char *lixa_session_get_sid(
        const lixa_session_t *session) {
        return session->sid;
    }

    

    /**
     * Set session ID (sessid) in a session object
     * @param[in,out] session object
     * @param[in] sid session id
     * @return a reason code
     */
    int lixa_session_set_sid(lixa_session_t *session, const char *sid);


    
    /**
     * Credit: Laird Shaw https://creativeandcritical.net/str-replace-c
     * Replaces in the string str all the occurrences of the source string from
     * with the destination string to. The lengths of the strings from and to
     * may differ. The string to may be of any length, but the string from must
     * be of non-zero length - the penalty for providing an empty string for
     * the from parameter is an infinite loop. In addition, none of the three
     * parameters may be NULL.
     * @param[in] str source string
     * @param[in] from string to be replaced
     * @param[in] to string that will replace from one
     * @return The post-replacement string, or NULL if memory for the new
     *         string could not be allocated. Does not modify the original
     *         string. The memory for the returned post-replacement string may
     *         be deallocated with the standard library function free when it
     *         is no longer required.
     */
    char *lixa_str_replace(const char *str, const char *from, const char *to);



    /**
     * Credit: https://wiki.osdev.org/CRC32
     * Compute a CRC32
     * @param[in] buffer that must be computed
     * @param[in] buffer_len length of the buffer
     * @return the CRC32 code associated to the buffer
     */
    uint32_t lixa_crc32(const uint8_t *buffer, size_t buffer_len);


    
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
