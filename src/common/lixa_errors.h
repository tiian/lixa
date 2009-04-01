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
#ifndef LIXA_ERRORS_H
#define LIXA_ERRORS_H

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
 * Object not found
define LIXA_RC_OBJ_NOT_FOUND                   +1
 */
/**
 * An operation can not been performed but can safely bypassed has been
 * requested: the program can go on as no operation was requested
define LIXA_RC_BYPASSED_OPERATION              +2
 */



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
 * A passed object/option/arg is NULL and it can NOT be inferred from a default
 * value
define LIXA_RC_NULL_OBJECT                     -5
 */
/**
 * The container is full and can NOT store more elements
define LIXA_RC_CONTAINER_FULL                  -6
 */
/**
 * A used object (not void) has been passed to a function requiring a fresh
 * one
define LIXA_RC_OBJ_NOT_VOID                    -7
 */
/**
 * A NOT initialized object has been passed to a method/function
define LIXA_RC_OBJ_NOT_INITIALIZED             -8
 */
/**
 * A corrupted object has been passed to a function
define LIXA_RC_OBJ_CORRUPTED                   -9
 */
/**
 * A specified option is not valid for method and/or object status
define LIXA_RC_INVALID_OPTION                 -10
 */
/**
 * The status (value of any properties) of an object is invalid due to a bug
 * located elsewhere (a complex "internal error" condition)
define LIXA_RC_INVALID_STATUS                 -11
 */
/**
 * An invalid path/file name has been passed
define LIXA_RC_INVALID_PATH_NAME              -14
 */
/**
 * The file should not exist and a new one can not be safely created
define LIXA_RC_FILE_ALREADY_EXISTS            -15
 */
/**
 * The destination object is too small to store passed data
define LIXA_RC_DESTINATION_TOO_SMALL          -16
 */



/**
 * "malloc" function error
define LIXA_RC_MALLOC_ERROR                  -100
 */
/**
 * "realloc" function error
define LIXA_RC_REALLOC_ERROR                 -101
 */
/**
 * "open" function error
 */
#define LIXA_RC_OPEN_ERROR                   -110
/**
 * "fclose" function error
define LIXA_RC_FCLOSE_ERROR                  -111
 */
/**
 * "fwrite" function error
define LIXA_RC_FWRITE_ERROR                  -112
 */
/**
 * "fread" function error
define LIXA_RC_FREAD_ERROR                   -113
 */
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
 * "fstat" function error
define LIXA_RC_FSTAT_ERROR                   -124
 */
/**
 * "vsnprintf" function error
define LIXA_RC_VSNPRINTF_ERROR               -125
 */
/**
 * "times" function error
define LIXA_RC_TIMES_ERROR                   -126
 */
/**
 * "sysconf" function error
define LIXA_RC_SYSCONF_ERROR                 -127
 */
/**
 * "gettimeofday" function error
define LIXA_RC_GETTIMEOFDAY_ERROR            -128
 */
/**
 * "unlink" function error
define LIXA_RC_UNLINK_ERROR                  -129
 */

/**
 * "xmlReadFile" function error
 */
#define LIXA_RC_XML_READ_FILE_ERROR          -200



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


