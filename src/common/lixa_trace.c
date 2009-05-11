/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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



#include "lixa_trace.h"



unsigned long lixa_trace_mask = 0;
pthread_mutex_t lixa_trace_mutex;
int lixa_trace_mutex_init = FALSE;



void lixa_trace_init(void)
{
    /* retrieve environemnt variable */
    if (getenv(LIXA_TRACE_MASK_ENV_VAR) != NULL)
        lixa_trace_mask = strtoul(getenv(LIXA_TRACE_MASK_ENV_VAR), NULL, 0);
    else
        lixa_trace_mask = 0x0;    
    /* initialize mutex */
    if (0 != pthread_mutex_init(&lixa_trace_mutex, NULL))
        perror("lixa_trace_init/pthread_mutex_init\n");
    lixa_trace_mutex_init = TRUE;
}


void lixa_trace(const char *fmt, ...)
{
    va_list args;
    struct tm broken_time;
    struct timeval tv;
    
    va_start(args, fmt);
#ifdef HAVE_VFPRINTF
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &broken_time);
    /* check the mutex has been initialized before a mutex lock will be
       performed */
    if (lixa_trace_mutex_init) {
        if (0 != pthread_mutex_lock(&lixa_trace_mutex))
            perror("lixa_trace/pthread_mutex_lock");
        /* default header */
        fprintf(stderr,
                "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%6.6d [%d/"
                PTHREAD_T_FORMAT "] ",
                broken_time.tm_year + 1900, broken_time.tm_mon + 1,
                broken_time.tm_mday, broken_time.tm_hour,
                broken_time.tm_min, broken_time.tm_sec, (int)tv.tv_usec,
                getpid(), pthread_self());
    } /* if (lixa_trace_mutex_init) */
    /* custom message */
    vfprintf(stderr, fmt, args);
#ifndef NDEBUG
    fflush(stderr);
#endif
    /* remove the lock from mutex */
    if (lixa_trace_mutex_init) {
        if (0 != pthread_mutex_unlock(&lixa_trace_mutex))
            perror("lixa_trace/pthread_mutex_unlock");
    } /* if (lixa_trace_mutex_init) */
#else
# error "vfprintf is necessary for lixa_trace function!"
#endif
    va_end(args);
}



void lixa_trace_hex_data(const byte_t *data, lixa_word_t size,
                       FILE *out_stream)
{
        lixa_word_t i;

        for (i = 0; i < size; ++i) {
                fprintf(out_stream, "%02x ", (data[i] & 0xff));
        } /* for (i = 0; i < size; ++i) */
}



void lixa_trace_text_data(const byte_t *data, lixa_word_t size,
                        FILE *out_stream)
{
        lixa_word_t i;

        for (i = 0; i < size; ++i) {
                if (data[i] >= (byte_t)' ' && data[i] < (byte_t)0x80)
                        putc((int)(data[i] & 0xff), out_stream);
                else
                        putc((int)' ', out_stream);
        } /* for (i = 0; i < size; ++i) */
}



