/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_DEFINES_H
# define LIXA_DEFINES_H



#include "config.h"



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



#include "lixa_config.h"



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



/***********************************************************************
 *                                                                     *
 * Exception macros                                                    *
 *                                                                     *
 ***********************************************************************/
#ifndef TRY
# define TRY
#endif /* TRY */
        
#ifndef CATCH
# define CATCH ExcpHand:
#endif /* CATCH */

#ifndef THROW
# define THROW(e) { excp = e; goto ExcpHand; }
#endif /* THROW */



#ifndef STRORNULL
/**
 * STRing OR NULL:
 * it can be used to print C strings even if they could be NULL; if passed
 * string pointer is NULL, it returns a pointer to @ref LIXA_NULL_STRING
 */
# define STRORNULL(a)   (NULL != a ? (const char *)a : LIXA_NULL_STRING)
#endif



#ifndef STROREMPTY
/**
 * STRing OR EMPTY:
 * it can be used to print C strings even if they could be NULL; if passed
 * string pointer is NULL, it returns ""
 */
# define STROREMPTY(a)   (NULL != a ? (const char *)a : "")
#endif



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
#define UINT32_T_XFORMAT "0x%08x"
#define LIXA_WORD_T_FORMAT   UINT32_T_FORMAT



#ifdef HAVE_NFDS_T
# if SIZEOF_NFDS_T == SIZEOF_INT
#  define NFDS_T_FORMAT "%d"
# elif SIZEOF_NFDS_T == SIZEOF_LONG_INT
#  define NFDS_T_FORMAT "%lu"
# else
#  error Unable to determine sizeof(nfds_t)
# endif
#else
# error nfds_t type is not available
#endif



#ifdef HAVE_PID_T
# if SIZEOF_PID_T == SIZEOF_INT
#  define PID_T_FORMAT "%d"
# elif SIZEOF_PID_T == SIZEOF_LONG_INT
#  define PID_T_FORMAT "%l"
# else
#  error Unable to determine sizeof(pid_t)
# endif
#else
# error pid_t type is not available
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



/**
 * format for printing uintptr_t
 */
#if SIZEOF_UINTPTR_T == SIZEOF_INT
# define UINTPTR_T_FORMAT "%u"
#elif SIZEOF_UINTPTR_T == SIZEOF_LONG_INT
# define UINTPTR_T_FORMAT "%lu"
#elif SIZEOF_UINTPTR_T == SIZEOF_LONG_LONG_INT
# define UINTPTR_T_FORMAT "%llu"
#else
# error Unable to determine sizeof(uintptr_t)
#endif



#define SSIZE_T_FORMAT "%ld"



#if SIZEOF_IN_PORT_T == SIZEOF_SHORT_INT
# define IN_PORT_T_FORMAT "%hu"
#else
# error Unable to determine format for in_port_t
#endif



/* This is necessary for xid.formatID serialization */
#if (SIZEOF_LONG_INT == 4)
# define LIXA_SERIALIZED_LONG_INT 10
#elif (SIZEOF_LONG_INT == 8)
# define LIXA_SERIALIZED_LONG_INT 20
#else
# error "Unable to determine serialized length of a long int"
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
     * Control word
     * LIXA_WORD_T_FORMAT == UINT32_T_FORMAT
     */
    typedef uint32_t lixa_word_t;



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_DEFINES_H */

