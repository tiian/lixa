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
#include <errno.h>



#include "lixa_defines.h"



/**
 * Name of the environment variable that must be used to set the trace mask
 */
#define LIXA_TRACE_MASK_ENV_VAR         "LIXA_TRACE_MASK"
/**
 * Name of the environment variable that must be used to activate stack trace
 */
#define LIXA_TRACE_STACK_ENV_VAR        "LIXA_STACK_TRACE"
/**
 * Value string for environment variable LIXA_STACK_TRACE
 */
#define LIXA_TRACE_STACK_VALUE_ERRORS   "ERRORS"
/**
 * Value string for environment variable LIXA_STACK_TRACE
 */
#define LIXA_TRACE_STACK_VALUE_WARNINGS "WARNINGS"
/**
 * Value string for environment variable LIXA_STACK_TRACE
 */
#define LIXA_TRACE_STACK_VALUE_ALL      "ALL"



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
 * trace module for XA Transaction API (XTA) functions
 */
#define LIXA_TRACE_MOD_XTA                0x00000020

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
 * trace module for server FSM (finite state machine) functions
 */
#define LIXA_TRACE_MOD_SERVER_FSM         0x00000200

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
 * trace module for client XA switch files provided by LIXA
 */
#define LIXA_TRACE_MOD_CLIENT_XA_SWITCH   0x00010000

/**
 * trace module for client status functions
 */
#define LIXA_TRACE_MOD_CLIENT_STATUS      0x00020000

/**
 * trace module for client recovery functions
 */
#define LIXA_TRACE_MOD_CLIENT_RECOVERY    0x00040000

/**
 * trace module for client generic functions
 */
#define LIXA_TRACE_MOD_CLIENT_GENERIC     0x00080000

/**
 * trace module for client tpm functions
 */
#define LIXA_TRACE_MOD_CLIENT_TPM         0x00100000

/**
 * trace module for server tpm functions
 */
#define LIXA_TRACE_MOD_SERVER_TPM         0x00200000

/*
 * a lot of unused bits ...
 */

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
 * trace module for common utils functions
 */
#define LIXA_TRACE_MOD_COMMON_XID         0x10000000


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
# define LIXA_TRACE(a)    (LIXA_TRACE_MODULE & lixa_trace_mask ?    \
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
# define LIXA_TRACE_HEX_DATA(a,b,c) (LIXA_TRACE_MODULE & lixa_trace_mask ? \
                                     lixa_trace_hex_data(a,b,c,stderr) : 0)
#else
# define LIXA_TRACE_HEX_DATA(a, b, c)
#endif /* _TRACE */



/**
 * LIXA_TRACE_STACK macro is used to compile stack trace messages only if
 * _TRACE macro is defined;
 * stack trace message is printed only if the severity of ret_cod matches the
 * value of the environment variable
 */
#ifdef _TRACE
# define LIXA_TRACE_STACK() ( \
        (STACK_TRACE_ALL == lixa_trace_stack_value) || \
        (STACK_TRACE_ONLY_ERRORS == lixa_trace_stack_value && \
         ret_cod < 0) || \
        (STACK_TRACE_WARNINGS_AND_ERRORS == lixa_trace_stack_value && \
         ret_cod != 0) ? \
        lixa_trace_stack(__func__, __FILE__, __LINE__, excp, ret_cod, lixa_strerror(ret_cod), \
                         errno, LIXA_TRACE_MODULE) : 0)
#else
# define LIXA_TRACE_STACK()
#endif


/**
 * Call something that make sense for tracing, like for example a Java stack
 * trace if the module has active tracing
 */
#ifdef _TRACE
# define LIXA_TRACE_SOMETHING(a)   (LIXA_TRACE_MODULE & lixa_trace_mask ? \
                                     a : 0)
#else
# define LIXA_TRACE_SOMETHING(a)
#endif /* _TRACE */



/**
 * Which functions will be stack traced (enum, type)
 */
enum lixa_trace_stack_value_e {
    /** don't produce stack trace messages */
    STACK_TRACE_NONE,
    /** produce stack trace messages only in the event of errors */
    STACK_TRACE_ONLY_ERRORS,
    /** produce stack trace messages in the event of errors and warnings */
    STACK_TRACE_WARNINGS_AND_ERRORS,
    /** produce stack trace messages in any condition */
    STACK_TRACE_ALL
};



/**
 * Which functions will be stack traced (variable, value)
 */
extern enum lixa_trace_stack_value_e lixa_trace_stack_value;



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
     * @param[in] fmt record format
     * @param[in] ... IN record data
     */
    void lixa_trace(const char *fmt, ...);

    

    /**
     * Dump the content of a piece of memory to a stream (hex format)
     * @param[in] prefix to print before dump (it is a fixed
     *               prefix, not a format with values)
     * @param[in] data pointer to base memory
     * @param[in] size number of bytes to dump
     * @param[in] out_stream destination standard I/O stream
     */
    void lixa_trace_hex_data(const char *prefix, const byte_t *data,
                             lixa_word_t size, FILE *out_stream);


    /**
     * Dump the content of a piece of memory to a stream (text format)
     * @param[in] prefix to print before dump (it is a fixed
     *               prefix, not a format with values)
     * @param[in] data pointer to base memory
     * @param[in] size number of bytes to dump
     * @param[in] out_stream destination standard I/O stream
     */
    void lixa_trace_text_data(const char *prefix, const byte_t *data,
                              lixa_word_t size, FILE *out_stream);


    
    /**
     * Send a trace message for stack tracing
     */
    void lixa_trace_stack(const char *function_name, const char *file_name,
                          int file_line,
                          int exception, int ret_cod, const char *ret_cod_text,
                          int error, long trace_module);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* LIXA_TRACE_H */
