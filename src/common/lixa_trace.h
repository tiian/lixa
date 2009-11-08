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
