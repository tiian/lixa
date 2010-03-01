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
#ifndef LIXA_TRACE_H
# define LIXA_TRACE_H



#include <config.h>



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif /* HAVE_STDIO_H */



#include <lixa_defines.h>



/**
 * Name of the environment variable must be used to set the trace mask
 */
#define LIXA_TRACE_MASK_ENV_VAR    "LIXA_TRACE_MASK"



/**
 * trace module for files do not need trace feature
 */
#define LIXA_TRACE_MOD_NO_TRACE           0x00000000

/**
 * trace module for generic server functions
 */
#define LIXA_TRACE_MOD_SERVER             0x00000001

/**
 * trace module for server configuration functions
 */
#define LIXA_TRACE_MOD_SERVER_CONFIG      0x00000002

/**
 * trace module for server listener functions
 */
#define LIXA_TRACE_MOD_SERVER_LISTENER    0x00000004

/**
 * trace module for server manager functions
 */
#define LIXA_TRACE_MOD_SERVER_MANAGER     0x00000008

/**
 * trace module for server status functions
 */
#define LIXA_TRACE_MOD_SERVER_STATUS      0x00000010

/**
 * trace module for server messages functions
 */
#define LIXA_TRACE_MOD_SERVER_MESSAGES    0x00000020

/**
 * trace module for server XA functions
 */
#define LIXA_TRACE_MOD_SERVER_XA          0x00000040

/**
 * trace module for server reply functions
 */
#define LIXA_TRACE_MOD_SERVER_REPLY       0x00000080

/**
 * trace module for server recovery functions
 */
#define LIXA_TRACE_MOD_SERVER_RECOVERY    0x00000100

/**
 * trace module for client TX standard functions
 */
#define LIXA_TRACE_MOD_CLIENT_TX          0x00001000

/**
 * trace module for client XA standard functions
 */
#define LIXA_TRACE_MOD_CLIENT_XA          0x00002000

/**
 * trace module for client connection functions
 */
#define LIXA_TRACE_MOD_CLIENT_CONN        0x00004000

/**
 * trace module for client config functions
 */
#define LIXA_TRACE_MOD_CLIENT_CONFIG      0x00008000

/**
 * trace module for client load functions
 */
#define LIXA_TRACE_MOD_CLIENT_LOAD        0x00010000

/**
 * trace module for client status functions
 */
#define LIXA_TRACE_MOD_CLIENT_STATUS      0x00020000

/**
 * trace module for common config functions
 */
#define LIXA_TRACE_MOD_COMMON_CONFIG      0x01000000

/**
 * trace module for common xml_msg functions
 */
#define LIXA_TRACE_MOD_COMMON_XML_MSG     0x02000000

/**
 * trace module for common status functions
 */
#define LIXA_TRACE_MOD_COMMON_STATUS      0x04000000

/**
 * trace module for common utils functions
 */
#define LIXA_TRACE_MOD_COMMON_UTILS       0x08000000



/**
 * Status of the trace: TRUE = initialized, FALSE = uninitialized
 */
extern int lixa_trace_initialized;



/**
 * This is the mask retrieved from environment var LIXA_TRACE_MASK and
 * determines which modules are traced
 */
extern unsigned long lixa_trace_mask;



/**
 * LIXA_TRACE_INIT macro is used to compile @ref lixa_trace_init function
 * only if _DEBUG macro is defined
 */
#ifdef _TRACE
# define LIXA_TRACE_INIT lixa_trace_init()
#else
# define LIXA_TRACE_INIT
#endif



/**
 * LIXA_TRACE macro is used to compile trace messages only if _DEBUG macro is
 * defined
 * trace message is printed only for modules (LIXA_TRACE_MODULE) covered by
 * trace mask (LIXA_TRACE_MASK) specified as environment variable
 */
#ifdef _TRACE
# define LIXA_TRACE(a)    (LIXA_TRACE_MODULE & lixa_trace_mask ? \
                           lixa_trace a : 0)
#else
# define LIXA_TRACE(a)
#endif /* _TRACE */



/**
 * LIXA_TRACE_HEX_DATA macro is used to compile trace messages only if _TRACE
 * macro is defined;
 * trace message is printed only for modules (LIXA_TRACE_MODULE) covered by
 * trace mask (LIXA_TRACE_MASK) specified as environment variable
 */
#ifdef _TRACE
# define LIXA_TRACE_HEX_DATA(a,b)   (LIXA_TRACE_MODULE & lixa_trace_mask ? \
                                     lixa_trace_hex_data(a,b,stderr) : 0)
#else
# define LIXA_TRACE_HEX_DATA(a,b)
#endif /* _TRACE */



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    /**
     * This method MUST be called BEFORE first log call to avoid lock
     * contention in multithread environments
     */
    void lixa_trace_init(void);
    

    
    /**
     * Send trace record to stderr
     * @param fmt IN record format
     * @param ... IN record data
     */
    void lixa_trace(const char *fmt, ...);

        

    /**
     * Dump the content of a piece of memory to a stream (hex format)
     * @param data IN pointer to base memory
     * @param size IN number of bytes to dump
     * @param out_stream IN destination standard I/O stream
     */
    void lixa_trace_hex_data(const byte_t *data , lixa_word_t size,
                             FILE *out_stream);


      
    /**
     * Dump the content of a piece of memory to a stream (text format)
     * @param data IN pointer to base memory
     * @param size IN number of bytes to dump
     * @param out_stream IN destination standard I/O stream
     */
    void lixa_trace_text_data(const byte_t *data, lixa_word_t size,
                              FILE *out_stream);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_TRACE_H */
