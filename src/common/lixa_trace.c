/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <config.h>



#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_STDARG_H
# include <stdarg.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include "lixa_trace.h"



int lixa_trace_initialized = FALSE;

unsigned long lixa_trace_mask = 0;

enum lixa_trace_stack_value_e lixa_trace_stack_value;


/**
 * This mutex is used to avoid contention (bad output) on trace file
 */
GMutex lixa_trace_mutex;



/**
 * Initialize the library when the library is loaded.
 */
void lixa_trace_init(void)
{
    if (lixa_trace_initialized)
        return;
    g_mutex_lock(&lixa_trace_mutex);
    if (!lixa_trace_initialized) {
        /* retrieve environemnt variables */
        /* trace mask environment variable: */
        if (getenv(LIXA_TRACE_MASK_ENV_VAR) != NULL)
            lixa_trace_mask = strtoul(
                getenv(LIXA_TRACE_MASK_ENV_VAR), NULL, 0);
        else
            lixa_trace_mask = 0x0;
        /* stack trace environment variable: */
        lixa_trace_stack_value = STACK_TRACE_NONE;
        if (getenv(LIXA_TRACE_STACK_ENV_VAR) != NULL) {
            if (0 == strcmp(getenv(LIXA_TRACE_STACK_ENV_VAR),
                            LIXA_TRACE_STACK_VALUE_ALL))
                lixa_trace_stack_value = STACK_TRACE_ALL;
            else if (0 == strcmp(getenv(LIXA_TRACE_STACK_ENV_VAR),
                                 LIXA_TRACE_STACK_VALUE_WARNINGS))
                lixa_trace_stack_value = STACK_TRACE_WARNINGS_AND_ERRORS;
            else if (0 == strcmp(getenv(LIXA_TRACE_STACK_ENV_VAR),
                                 LIXA_TRACE_STACK_VALUE_ERRORS))
                lixa_trace_stack_value = STACK_TRACE_ONLY_ERRORS;
        } /* if (getenv(LIXA_STACK_TRACE_ENV_VAR) != NULL) */
        lixa_trace_initialized = TRUE;
    }
    /* remove the lock from mutex */
    g_mutex_unlock(&lixa_trace_mutex);
}



void lixa_trace(const char *fmt, ...)
{
    va_list args;
    struct tm broken_time;
    struct timeval tv;
    char buffer[2000];
    int nw1;

    va_start(args, fmt);
#ifdef HAVE_VSNPRINTF
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &broken_time);

    g_mutex_lock(&lixa_trace_mutex);
    /* default header */
    nw1 = snprintf(
        buffer, sizeof(buffer),
        "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%6.6d [" PID_T_FORMAT "/"
        PTHREAD_T_FORMAT "] ",
        broken_time.tm_year + 1900, broken_time.tm_mon + 1,
        broken_time.tm_mday, broken_time.tm_hour,
        broken_time.tm_min, broken_time.tm_sec, (int)tv.tv_usec,
        getpid(), pthread_self());
    if (nw1 < sizeof(buffer)) {
        /* custom message */
        vsnprintf(buffer+nw1, sizeof(buffer)-nw1, fmt, args);
        buffer[sizeof(buffer)-1] = '\0';
    }
    fputs(buffer, stderr);
# ifdef LIXA_DEBUG
    fflush(stderr);
#endif
    /* remove the lock from mutex */
    g_mutex_unlock(&lixa_trace_mutex);
#else
# error "vsnprintf is necessary for flom_trace function!"
#endif
    va_end(args);
}



void lixa_trace_hex_data(const char *prefix, const byte_t *data,
                         lixa_word_t size, FILE *out_stream)
{
    lixa_word_t i;
    struct tm broken_time;
    struct timeval tv;
    
#ifdef HAVE_VFPRINTF
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &broken_time);

    g_mutex_lock(&lixa_trace_mutex);
    /* default header */
    fprintf(out_stream,
            "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%6.6d [" PID_T_FORMAT "/"
            PTHREAD_T_FORMAT "] %s",
            broken_time.tm_year + 1900, broken_time.tm_mon + 1,
            broken_time.tm_mday, broken_time.tm_hour,
            broken_time.tm_min, broken_time.tm_sec, (int)tv.tv_usec,
            getpid(), pthread_self(), prefix);
    /* dump data */
    for (i = 0; i < size; ++i) {
        fprintf(out_stream, "%02x ", (data[i] & 0xff));
    } /* for (i = 0; i < size; ++i) */
    /* close trace record */
    fprintf(out_stream, "\n");
#ifdef LIXA_DEBUG
    fflush(out_stream);
#endif
    /* remove the lock from mutex */
    g_mutex_unlock(&lixa_trace_mutex);
#else
# error "vfprintf is necessary for lixa_trace_hex_data function!"
#endif
}



void lixa_trace_text_data(const char *prefix, const byte_t *data,
                          lixa_word_t size, FILE *out_stream)
{
    lixa_word_t i;
    struct tm broken_time;
    struct timeval tv;
    
#ifdef HAVE_VFPRINTF
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &broken_time);

    g_mutex_lock(&lixa_trace_mutex);
    /* default header */
    fprintf(out_stream,
            "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%6.6d [%d/"
            PTHREAD_T_FORMAT "] %s",
            broken_time.tm_year + 1900, broken_time.tm_mon + 1,
            broken_time.tm_mday, broken_time.tm_hour,
            broken_time.tm_min, broken_time.tm_sec, (int)tv.tv_usec,
            getpid(), pthread_self(), prefix);
    /* dump data */
    for (i = 0; i < size; ++i) {
        if (data[i] >= (byte_t)' ' && data[i] < (byte_t)0x80)
            putc((int)(data[i] & 0xff), out_stream);
        else
            putc((int)' ', out_stream);
    } /* for (i = 0; i < size; ++i) */
    /* close trace record */
    fprintf(out_stream, "\n");
#ifdef LIXA_DEBUG
    fflush(out_stream);
#endif
    /* remove the lock from mutex */
    g_mutex_unlock(&lixa_trace_mutex);
#else
# error "vfprintf is necessary for lixa_trace_hex_data function!"
#endif
}



void lixa_trace_stack(const char *function_name, const char *file_name,
                      int file_line,
                      int exception, int ret_cod, const char *ret_cod_text,
                      int error, long trace_module)
{
    struct tm broken_time;
    struct timeval tv;

    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &broken_time);
    g_mutex_lock(&lixa_trace_mutex);
    /* default header */
    fprintf(stderr,
            /* standard LIXA trace header */
            "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%6.6d [" PID_T_FORMAT "/"
            PTHREAD_T_FORMAT "] "
            /* JSON formatted trace body message */
            "{ "
            "\"type\": \"StackTrace\", "
            "\"date\": \"%4.4d-%2.2d-%2.2d\", "
            "\"time\": \"%2.2d:%2.2d:%2.2d.%6.6d\", "
            "\"process_id\": " PID_T_FORMAT ", "
            "\"thread_id\": " PTHREAD_T_FORMAT ", "
            "\"function_name\": \"%s\", "
            "\"source_file_name\": \"%s\", "
            "\"source_file_line\": \"%d\", "
            "\"exception\": %d, "
            "\"return_code\": %d, "
            "\"return_code_text\": \"%s\", "
            "\"errno\": %d, "
            "\"errno_text\": \"%s\", "
            "\"trace_module\": 0x%x "
            "}\n",
            /* standard LIXA trace header */
            broken_time.tm_year + 1900, broken_time.tm_mon + 1,
            broken_time.tm_mday, broken_time.tm_hour,
            broken_time.tm_min, broken_time.tm_sec, (int)tv.tv_usec,
            getpid(), pthread_self(),
            /* JSON formatted trace body message */
            broken_time.tm_year + 1900, broken_time.tm_mon + 1,
            broken_time.tm_mday, broken_time.tm_hour,
            broken_time.tm_min, broken_time.tm_sec, (int)tv.tv_usec,
            getpid(), pthread_self(),
            function_name, file_name, file_line, exception, ret_cod,
            ret_cod_text, error, strerror(error), trace_module);
# ifdef LIXA_DEBUG
    fflush(stderr);
#endif
    /* remove the lock from mutex */
    g_mutex_unlock(&lixa_trace_mutex);
}
