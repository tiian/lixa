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
#ifndef LIXA_DEFINES_H
#define LIXA_DEFINES_H

#include <config.h>



#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif /* HAVE_STDLIB_H */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif /* HAVE_STDINT_H */
#ifdef HAVE_SYS_INTTYPES_H
# include <sys/inttypes.h>
#endif /* HAVE_SYS_INTTYPES_H */
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_TIME_H
# include <time.h>
#endif /* HAVE_TIME_H */



/***********************************************************************
 *                                                                     *
 * Boolean macros                                                      *
 *                                                                     *
 ***********************************************************************/
#ifndef TRUE
# define TRUE 1
#endif /* TRUE */

#ifndef FALSE
# define FALSE 0
#endif /* FALSE */



/**
 * This value is the null file descriptor convention
 */
#define LIXA_NULL_FD    -1



/**
 * Format must be used in *printf family function to print a "uint32_t"
 * value
 */
#if (SIZEOF_UINT32_T == SIZEOF_INT)
# define UINT32_T_FORMAT "%u"
#elif SIZEOF_UINT32_T == SIZEOF_LONG_INT
# define UINT32_T_FORMAT "%lu"
#elif SIZEOF_UINT32_T == SIZEOF_LONG_LONG_INT
# define UINT32_T_FORMAT "%llu"
#else
# error Unable to determine sizeof(uint32_t)
#endif



#ifdef HAVE_NFDS_T
# if SIZEOF_NFDS_T == SIZEOF_LONG_INT
#  define NFDS_T_FORMAT "%lu"
# else
#  error Unable to determine sizeof(nfds_t)
# endif
#else
# error nfds_t type is not available
#endif



#ifdef HAVE_PTHREAD_T
# if SIZEOF_PTHREAD_T == SIZEOF_LONG_INT
#  define PTHREAD_T_FORMAT "%lu"
# else
#  error Unable to determine sizeof(pthread_t)
# endif
#else
# error pthread_t type is not available
#endif



#if SIZEOF_SIZE_T == SIZEOF_INT
# define SIZE_T_FORMAT "%u"
#elif SIZEOF_SIZE_T == SIZEOF_LONG_INT
# define SIZE_T_FORMAT "%lu"
#elif SIZEOF_SIZE_T == SIZEOF_LONG_LONG_INT
# define SIZE_T_FORMAT "%llu"
#else
# error Unable to determine sizeof(size_t)
#endif



#ifdef HAVE_TIME_H
# if SIZEOF_CLOCK_T == SIZEOF_INT
#  define CLOCK_T_FORMAT "%d"
# elif SIZEOF_CLOCK_T == SIZEOF_LONG_INT
#  define CLOCK_T_FORMAT "%ld"
# elif SIZEOF_CLOCK_T == SIZEOF_LONG_LONG_INT
#  define CLOCK_T_FORMAT "%lld"
# else
#  error Unable to determine sizeof(clock_t)
# endif
#endif /* HAVE_TIME_H */



/**
 * format for printing "off_t" values
 */
#if SIZEOF_OFF_T == SIZEOF_LONG_INT
# define OFF_T_FORMAT "%ld"
#elif SIZEOF_OFF_T == SIZEOF_LONG_LONG_INT
# define OFF_T_FORMAT "%lld"
#else
# error Unable to determine sizeof(off_t)
#endif



#define SSIZE_T_FORMAT "%ld"



#if SIZEOF_IN_PORT_T == SIZEOF_SHORT_INT
# define IN_PORT_T_FORMAT "%hu"
#else
# error Unable to determine format for in_port_t
#endif



#define LIXA_PATH_SEPARATOR '/'



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



#ifndef HAVE_UINT8_T
      /** 8 bit unsigned int */
      typedef unsigned char uint8_t;
#endif /* HAVE_UINT8_T */
#ifndef HAVE_UINT32_T
# if (SIZEOF_INT == 4)
      /** 32 bit unsigned int */
      typedef unsigned int uint32_t;
# else
#  error "Please specify a 32 bit wide unsigned int type"
# endif /* if (SIZEOF_INT == 4) */
#endif /* HAVE_UINT32_T */


      
    /**
     * A byte is the fundamental I/O unit
     */
    typedef uint8_t byte_t;

      

    /**
     * Control word used by journaled file (for example, journal records
     * are expressed as words
     */
    typedef uint32_t lixa_word_t;



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_DEFINES_H */

