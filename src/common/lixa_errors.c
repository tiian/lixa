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



#include <lixa_errors.h>



const char *lixa_strerror(int ret_cod)
{
        switch (ret_cod) {
/*
            case LIXA_RC_BYPASSED_OPERATION:
                return "WARNING: operation was not performed because it can "
                        "not be requested";
*/              
            case LIXA_RC_EMPTY_CONTAINER:
                return "WARNING: the container is empty";
            case LIXA_RC_OBJ_NOT_FOUND:
                return "WARNING: object not found";
            case LIXA_RC_OK:
                return "OK: no error";
            case LIXA_RC_INTERNAL_ERROR:
                return "ERROR: internal error / unexpected condition / "
                        "code bug";
            case LIXA_RC_OUT_OF_RANGE:
                return "ERROR: an argument is out of range";
            case LIXA_RC_CONFIG_ERROR:
                return "ERROR: configuration file is broken";
            case LIXA_RC_NETWORK_EVENT_ERROR:
                return "ERROR: an unespected network event raised";
            case LIXA_RC_NULL_OBJECT:
                return "ERROR: an argument is null";
            case LIXA_RC_CONTAINER_FULL:
                return "ERROR: the container is full and cannot store more "
                        "elements";
                /*
            case LIXA_RC_OBJ_NOT_VOID:
                return "ERROR: object is not void";
                */
            case LIXA_RC_OBJ_NOT_INITIALIZED:
                return "ERROR: object is not initialized";
            case  LIXA_RC_OBJ_CORRUPTED:
                return "ERROR: object is corrupted";
                /*
            case LIXA_RC_INVALID_OPTION:
                return "ERROR: a specified option is not valid";
            case LIXA_RC_INVALID_STATUS:
                return "ERROR: invalid object status";
            case LIXA_RC_INVALID_PATH_NAME:
                return "ERROR: invalid path name";
            case LIXA_RC_FILE_ALREADY_EXISTS:
                return "ERROR: file can not be created because it already "
                        "exists";
            case LIXA_RC_DESTINATION_TOO_SMALL:
                return "ERROR: destination object is too small to store "
                        "passed data";
                */
            case LIXA_RC_MALFORMED_XML_MSG:
                return "ERROR: the XML message is malformed and cannot be "
                    "interpreted";
            case LIXA_RC_UNKNOWN_XML_MSG_TYPE:
                return "ERROR: the XML message type is unknown";
            case LIXA_RC_XML_UNRECOGNIZED_TAG:
                return "ERROR: the XML contains a tag is not known or is "
                    "in the wrong place";
            case LIXA_RC_MALLOC_ERROR:
                return "ERROR: 'malloc' function returned an error condition";
            case LIXA_RC_REALLOC_ERROR:
                return "ERROR: 'realloc' function returned an error condition";
            case LIXA_RC_STRDUP_ERROR:
                return "ERROR: 'strdup' function returned an error condition";
            case LIXA_RC_POLL_ERROR:
                return "ERROR: 'poll' function returned an error condition";
            case LIXA_RC_PIPE_ERROR:
                return "ERROR: 'pipe' function returned an error condition";
            case LIXA_RC_OPEN_ERROR:
                return "ERROR: 'open' function returned an error condition";
            case LIXA_RC_CLOSE_ERROR:
                return "ERROR: 'close' function returned an error condition";
            case LIXA_RC_WRITE_ERROR:
                return "ERROR: 'write' function returned an error condition";
            case LIXA_RC_READ_ERROR:
                return "ERROR: 'read' function returned an error condition";
                /*
            case LIXA_RC_FFLUSH_ERROR:
                return "ERROR: 'fflush' function returned an error condition";
            case LIXA_RC_FSYNC_ERROR:
                return "ERROR: 'fsync' function returned an error condition";
            case LIXA_RC_FDATASYNC_ERROR:
                return "ERROR: 'fdatasync' function returned an error "
                        "condition";
            case LIXA_RC_FPUTC_ERROR:
                return "ERROR: 'fputc' function (or 'putc' macro) returned an "
                        "error condition";
            case LIXA_RC_FTRUNCATE_ERROR:
                return "ERROR: 'ftruncate' function returned an error "
                        "condition";
            case LIXA_RC_FILENO_ERROR:
                return "ERROR: 'fileno' function returned an error "
                        "condition";
            case LIXA_RC_RENAME_ERROR:
                return "ERROR: 'rename' function returned an error "
                        "condition";
                */
            case LIXA_RC_FSTAT_ERROR:
                return "ERROR: 'fstat' function returned an error "
                        "condition";
            case LIXA_RC_MMAP_ERROR:
                return "ERROR: 'mmap' function returned an error "
                        "condition";
            case LIXA_RC_MUNMAP_ERROR:
                return "ERROR: 'munmap' function returned an error "
                        "condition";
                /*
            case LIXA_RC_VSNPRINTF_ERROR:
                return "ERROR: 'vsnprintf' function returned an error "
                        "condition";
            case LIXA_RC_TIMES_ERROR:
                return "ERROR: 'times' function returned an error condition";
            case LIXA_RC_SYSCONF_ERROR:
                return "ERROR: 'sysconf' function returned an error condition";
                */
            case LIXA_RC_GETTIMEOFDAY_ERROR:
                return "ERROR: 'gettimeofday' function returned an error "
                        "condition";
                /*
            case LIXA_RC_UNLINK_ERROR:
                return "ERROR: 'unlink' function returned an error condition";
                */
            case LIXA_RC_SOCKET_ERROR:
                return "ERROR: 'socket' function returned an error condition";
            case LIXA_RC_SETSOCKOPT_ERROR:
                return "ERROR: 'setsockopt' function returned an error "
                    "condition";
            case LIXA_RC_BIND_ERROR:
                return "ERROR: 'bind' function returned an error condition";
            case LIXA_RC_LISTEN_ERROR:
                return "ERROR: 'listen' function returned an error condition";
            case LIXA_RC_ACCEPT_ERROR:
                return "ERROR: 'accept' function returned an error condition";
            case LIXA_RC_SHUTDOWN_ERROR:
                return "ERROR: 'shutdown' function returned an error "
                    "condition";
            case LIXA_RC_GETADDRINFO_ERROR:
                return "ERROR: 'getaddrinfo' function returned an error "
                    "condition";
            case LIXA_RC_CONNECT_ERROR:
                return "ERROR: 'connect' function returned an error "
                    "condition";
            case LIXA_RC_RECV_ERROR:
                return "ERROR: 'recv' function returned an error "
                    "condition";
            case LIXA_RC_GETSOCKNAME_ERROR:
                return "ERROR: 'getsockname' function returned an error "
                    "condition";
            case LIXA_RC_GETPEERNAME_ERROR:
                return "ERROR: 'getpeername' function returned an error "
                    "condition";
            case LIXA_RC_PTHREAD_CREATE_ERROR:
                return "ERROR: 'pthread_create' function returned an error "
                    "condition";
            case LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR:
                return "ERROR: 'pthread_mutex_lock' function returned an "
                    "error condition";
            case LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR:
                return "ERROR: 'pthread_mutex_unlock' function returned an "
                    "error condition";
            case LIXA_RC_PTHREAD_RWLOCK_WRLOCK_ERROR:
                return "ERROR: 'pthread_rwlock_wrlock' function returned an "
                    "error condition";
            case LIXA_RC_PTHREAD_RWLOCK_RDLOCK_ERROR:
                return "ERROR: 'pthread_rwlock_rdlock' function returned an "
                    "error condition";
            case LIXA_RC_PTHREAD_RWLOCK_UNLOCK_ERROR:
                return "ERROR: 'pthread_rwlock_unlock' function returned an "
                    "error condition";
            case LIXA_RC_XML_READ_FILE_ERROR:
                return "ERROR: 'xmlReadFile' function returned an error "
                    "condition";
            case LIXA_RC_XML_READ_MEMORY_ERROR:
                return "ERROR: 'xmlReadMemory' function returned an error "
                    "condition";
            case LIXA_RC_XML_DOC_GET_ROOT_ELEMENT_ERROR:
                return "ERROR: 'xmlDocGetRootElement' function returned an "
                    "error condition";
            case LIXA_RC_G_MODULE_OPEN_ERROR:
                return "ERROR: 'g_module_open' function returned an "
                    "error condition";
            case LIXA_RC_G_MODULE_CLOSE_ERROR:
                return "ERROR: 'g_module_close' function returned an "
                    "error condition";
            case LIXA_RC_G_MODULE_SYMBOL_ERROR:
                return "ERROR: 'g_module_symbol' function returned an "
                    "error condition";
            default:
                return "ERROR: unknown error";
        } /* switch (ret_cod) */
}
