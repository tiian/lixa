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
#ifndef LIXA_ERRORS_H
# define LIXA_ERRORS_H

#include <config.h>



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif /* HAVE_ERRNO_H */



#include <lixa_defines.h>



/*********************************************************
 *                                                       *
 * REASON / RETURN CODES                                 *
 *                                                       *
 *********************************************************/



/**
 * This is not a proper error, but an offset: when the client receive an
 * error from the server, it adds this offset to distinguish the reason as an
 * error generated in the server side
 */
#define LIXA_RC_ERROR_FROM_SERVER_OFFSET       1000



/**
 * Object not found
 */
#define LIXA_RC_OBJ_NOT_FOUND                   +1
/**
 * The container is empty
 */
#define LIXA_RC_EMPTY_CONTAINER                 +2
/**
 * An operation that can not be performed and can safely bypassed has been
 * requested: the program can go on as no operation was requested
 */
#define LIXA_RC_BYPASSED_OPERATION              +3
/**
 * Peer has closed TCP/IP connection
 */
#define LIXA_RC_CONNECTION_CLOSED               +4
/**
 * A truncation occurred because the destination is smaller then the source
 */
#define LIXA_RC_TRUNCATION_OCCURRED             +5



/**
 * Successfully completion
 */
#define LIXA_RC_OK                               0



/**
 * Internal error: unrecoverable status!
 */
#define LIXA_RC_INTERNAL_ERROR                  -1
/**
 * A parameter passed to a function is OUT OF RANGE
 */
#define LIXA_RC_OUT_OF_RANGE                    -2
/**
 * Configuration file is broken
 */
#define LIXA_RC_CONFIG_ERROR                    -3
/**
 * Unespected network event
 */
#define LIXA_RC_NETWORK_EVENT_ERROR             -4
/**
 * A passed object/option/arg is NULL and it can NOT be inferred from a default
 * value
 */
#define LIXA_RC_NULL_OBJECT                     -5
/**
 * The container is full and can NOT store more elements
 */
#define LIXA_RC_CONTAINER_FULL                  -6
/**
 * The process has been stopped to avoid a buffer overflow
 */
#define LIXA_RC_BUFFER_OVERFLOW                 -7
/**
 * A NOT initialized object has been passed to a method/function
 */
#define LIXA_RC_OBJ_NOT_INITIALIZED             -8
/**
 * A corrupted object has been passed to a function
 */
#define LIXA_RC_OBJ_CORRUPTED                   -9
/**
 * Status files are corrupted and the server can not start-up
 */
#define LIXA_RC_CORRUPTED_STATUS_FILE          -10
/**
 * A specified option is not valid for method and/or object status
 */
#define LIXA_RC_INVALID_OPTION                 -11
/**
 * A routine has been invoked in an improper context
 */
#define LIXA_RC_PROTOCOL_ERROR                 -12
/**
 * The status (value of any properties) of an object is invalid due to a bug
 * located elsewhere (a complex "internal error" condition)
 */
#define LIXA_RC_INVALID_STATUS                 -13
/**
 * Too many resource managers
 */
#define LIXA_RC_TOO_MANY_RSRMGRS               -14
/**
 * The number of chars of the prefix of the XML message 
 */
#define LIXA_RC_INVALID_PREFIX_SIZE            -15
/**
 * The XML message is empty and can not be processed
 */
#define LIXA_RC_EMPTY_XML_MSG                  -16
/**
 * The XML message is malformed and can not be processed
 */
#define LIXA_RC_MALFORMED_XML_MSG              -17
/**
 * The length of the XML message differs from prefix 
 */
#define LIXA_RC_INVALID_LENGTH_XML_MSG         -18
/**
 * The XML message is malformed and can not be processed
 */
#define LIXA_RC_PROPERTY_INVALID_VALUE         -19
/**
 * The XML contains a tag is not known or is in the wrong place
 */
#define LIXA_RC_XML_UNRECOGNIZED_TAG           -20
/**
 * An operation is referring to asynchronous mode that is not yet implemented
 */
#define LIXA_RC_ASYNC_NOT_IMPLEMENTED          -21
/**
 * The specified option might be valid, but it's not (yet) supported by LIXA
 */
#define LIXA_RC_UNSUPPORTED_OPTION             -22

/**
 * Generic error for an TX error (an TX return code not equal TX_OK)
 */
#define LIXA_RC_TX_ERROR                       -98
/**
 * Generic error for an XA error (an XA return code not equal XA_OK)
 */
#define LIXA_RC_XA_ERROR                       -99

/**
 * "malloc" function error
 */
#define LIXA_RC_MALLOC_ERROR                  -100
/**
 * "realloc" function error
 */
#define LIXA_RC_REALLOC_ERROR                 -101
/**
 * "strdup" function error
 */
#define LIXA_RC_STRDUP_ERROR                  -102
/**
 * "poll" function error
 */
#define LIXA_RC_POLL_ERROR                   -108
/**
 * "pipe" function error
 */
#define LIXA_RC_PIPE_ERROR                   -109
/**
 * "open" function error
 */
#define LIXA_RC_OPEN_ERROR                   -110
/**
 * "close" function error
 */
#define LIXA_RC_CLOSE_ERROR                  -111
/**
 * "truncate" function error
 */
#define LIXA_RC_TRUNCATE_ERROR               -112
/**
 * "write" function error
 */
#define LIXA_RC_WRITE_ERROR                  -113
/**
 * "fread" function error
 */
#define LIXA_RC_READ_ERROR                   -114
/**
 * "pathconf" function error
 */
#define LIXA_RC_PATHCONF_ERROR               -115
/**
 * "realpath" function error
 */
#define LIXA_RC_REALPATH_ERROR               -116
/**
 * "fflush" function error
define LIXA_RC_FFLUSH_ERROR                  -116
 */
/**
 * "fsync" function error
define LIXA_RC_FSYNC_ERROR                   -117
 */
/**
 * "fdatasync" function error
define LIXA_RC_FDATASYNC_ERROR               -118
 */
/**
 * "fputc"/"putc" function/macro error
define LIXA_RC_FPUTC_ERROR                   -119
 */
/**
 * "fgetc"/"getc" function/macro error
define LIXA_RC_FGETC_ERROR                   -120
 */
/**
 * "ftruncate" function error
define LIXA_RC_FTRUNCATE_ERROR               -121
 */
/**
 * "fileno" function error
define LIXA_RC_FILENO_ERROR                  -122
 */
/**
 * "rename" function error
define LIXA_RC_RENAME_ERROR                  -123
 */
/**
 * "stat" function error
 */
#define LIXA_RC_STAT_ERROR                    -123
/**
 * "fstat" function error
 */
#define LIXA_RC_FSTAT_ERROR                   -124
/**
 * "mmap" function error
 */
#define LIXA_RC_MMAP_ERROR                    -125
/**
 * "munmap" function error
 */
#define LIXA_RC_MUNMAP_ERROR                  -126
/**
 * "msync" function error
 */
#define LIXA_RC_MSYNC_ERROR                   -127

/**
 * "vsnprintf" function error
define LIXA_RC_VSNPRINTF_ERROR               -125
 */
/**
 * "times" function error
define LIXA_RC_TIMES_ERROR                   -126
 */
/**
 * "uuid_parse" function error
 */
#define LIXA_RC_UUID_PARSE_ERROR              -128
/**
 * "gettimeofday" function error
 */
#define LIXA_RC_GETTIMEOFDAY_ERROR            -129
/**
 * "unlink" function error
define LIXA_RC_UNLINK_ERROR                  -129
 */
/**
 * "socket" function error
 */
#define LIXA_RC_SOCKET_ERROR                   -130
/**
 * "setsockopt" function error
 */
#define LIXA_RC_SETSOCKOPT_ERROR               -131
/**
 * "bind" function error
 */
#define LIXA_RC_BIND_ERROR                     -132
/**
 * "bind" function error
 */
#define LIXA_RC_LISTEN_ERROR                   -133
/**
 * "accept" function error
 */
#define LIXA_RC_ACCEPT_ERROR                   -134
/**
 * "shutdown" function error
 */
#define LIXA_RC_SHUTDOWN_ERROR                 -135
/**
 * "getaddrinfo" function error
 */
#define LIXA_RC_GETADDRINFO_ERROR              -136
/**
 * "connect" function error
 */
#define LIXA_RC_CONNECT_ERROR                  -137
/**
 * "recv" function error
 */
#define LIXA_RC_SEND_ERROR                     -138
/**
 * "recv" function error
 */
#define LIXA_RC_RECV_ERROR                     -139
/**
 * "getsockname" function error
 */
#define LIXA_RC_GETSOCKNAME_ERROR              -140
/**
 * "getsockname" function error
 */
#define LIXA_RC_GETPEERNAME_ERROR              -141
/**
 * "pthread_create" function error
 */
#define LIXA_RC_PTHREAD_CREATE_ERROR           -150
/**
 * "pthread_mutex_lock" function error
 */
#define LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR       -151
/**
 * "pthread_mutex_unlock" function error
 */
#define LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR     -152
/**
 * "pthread_rwlock_wrlock" function error
 */
#define LIXA_RC_PTHREAD_RWLOCK_WRLOCK_ERROR    -153
/**
 * "pthread_rwlock_rlock" function error
 */
#define LIXA_RC_PTHREAD_RWLOCK_RDLOCK_ERROR    -154
/**
 * "pthread_rwlock_unlock" function error
 */
#define LIXA_RC_PTHREAD_RWLOCK_UNLOCK_ERROR    -155
/**
 * "xmlReadFile" function error
 */
#define LIXA_RC_XML_READ_FILE_ERROR            -200
/**
 * "xmlReadDoc" function error
 */
#define LIXA_RC_XML_READ_DOC_ERROR             -201
/**
 * "xmlReadMemory" function error
 */
#define LIXA_RC_XML_READ_MEMORY_ERROR          -202
/**
 * "xmlDocGetRootElement" function error
 */
#define LIXA_RC_XML_DOC_GET_ROOT_ELEMENT_ERROR -203
/**
 * A glib function returned a NULL pointer; the function is not documented as
 * returnig NULL. This is basically an internal error
 */
#define LIXA_RC_G_RETURNED_NULL                -300
/**
 * "g_module_open" function error
 */
#define LIXA_RC_G_MODULE_OPEN_ERROR            -301
/**
 * "g_module_close" function error
 */
#define LIXA_RC_G_MODULE_CLOSE_ERROR           -302
/**
 * "g_module_symbol" function error
 */
#define LIXA_RC_G_MODULE_SYMBOL_ERROR          -303
/**
 * "g_checksum_new" function error
 */
#define LIXA_RC_G_CHECKSUM_NEW_ERROR           -304
/**
 * "g_checksum_get_string" function error
 */
#define LIXA_RC_G_CHECKSUM_GET_STRING_ERROR    -305



/***********************************************************************
 *                                                                     *
 * Exception macros                                                    *
 *                                                                     *
 ***********************************************************************/
#ifndef TRY
#define TRY
#endif /* TRY */
        
#ifndef CATCH
#define CATCH ExcpHand:
#endif /* CATCH */

#ifndef THROW
#define THROW(e) { excp = e; goto ExcpHand; }
#endif /* THROW */



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



      /**
       * <B>PUBLIC METHOD</B><BR>
       * Retrieve the description associated to a return/reason code
       * @param ret_cod IN return/reason code of the desired description
       * @return a const string containing a description of reason code
       */
      const char *lixa_strerror(int ret_cod);
      
      

#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_ERRORS_H */


