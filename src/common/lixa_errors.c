/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <config.h>



#include <lixa_errors.h>



const char *lixa_strerror(int ret_cod)
{
    /* remove "error from server" offset */
    if (ret_cod > LIXA_RC_ERROR_FROM_SERVER_OFFSET)
        ret_cod -= LIXA_RC_ERROR_FROM_SERVER_OFFSET;
    else if (ret_cod < -LIXA_RC_ERROR_FROM_SERVER_OFFSET)
        ret_cod += LIXA_RC_ERROR_FROM_SERVER_OFFSET;
    
    switch (ret_cod) {
        case LIXA_RC_OTHER_BRANCH_ERROR:
            return "WARNING: the operation can not be completed due to an "
                "error happened in a different branch that's part of the "
                "same global transaction";
        case LIXA_RC_WOULD_BLOCK:
            return "WARNING: the operation would block the client, but 'NON "
                "BLOCKING' option has been specified";
        case LIXA_RC_OPERATION_POSTPONED:
            return "WARNING: an operation must be postponed because not all "
                "conditions are met";
        case LIXA_RC_NOT_CHAINABLE_BRANCH:
            return "WARNING: the Application Program wants to branch an "
                "existing superior branch, but the superior branch has "
                "already started to prepare the global transacation";
        case LIXA_RC_NO_SUPERIOR_BRANCH:
            return "WARNING: the Application Program wants to branch an "
                "existing superior branch, but it does not exist. The "
                "transaction can go on as a standard one";
        case LIXA_RC_MAINTENANCE_MODE:
            return "WARNING: maintenance mode execution only";
        case LIXA_RC_ASKED_SHUTDOWN:
            return "WARNING: shutdown must be performed";
        case LIXA_RC_THREAD_SWITCH:
            return "WARNING: the thread is serving the client must be "
                "switched to a different one";
        case LIXA_RC_LIXAC_CONF_CHANGED:
            return "WARNING: the digest of the lixac config file changed -> "
                "the client config file changed";
        case LIXA_RC_RECOVERY_PENDING_TX:
            return "WARNING: this thread of control should recover some "
                "recovery pending transactions";
        case LIXA_RC_TRUNCATION_OCCURRED:
            return "WARNING: a truncation occurred because the destination "
                "is smaller then the source";
        case LIXA_RC_CONNECTION_CLOSED:
            return "WARNING: peer has closed TCP/IP connection";
        case LIXA_RC_BYPASSED_OPERATION:
            return "WARNING: operation was not performed because it can "
                "not be requested";
        case LIXA_RC_EMPTY_CONTAINER:
            return "WARNING: the container is empty";
        case LIXA_RC_OBJ_NOT_FOUND:
            return "WARNING: object not found";
        case LIXA_RC_OK:
            return "OK: no error";
        case LIXA_RC_INTERNAL_ERROR:
            return "ERROR: internal error / unexpected condition / code bug";
        case LIXA_RC_OUT_OF_RANGE:
            return "ERROR: an argument is out of range";
        case LIXA_RC_CONFIG_ERROR:
            return "ERROR: configuration file is broken";
        case LIXA_RC_NETWORK_EVENT_ERROR:
            return "ERROR: an unespected network event raised";
        case LIXA_RC_NULL_OBJECT:
            return "ERROR: a passed or returned object/option/argument "
                "is NULL";
        case LIXA_RC_CONTAINER_FULL:
            return "ERROR: the container is full and cannot store more "
                "elements";
        case LIXA_RC_BUFFER_OVERFLOW:
            return "ERROR: the process has been stopped to avoid a buffer "
                "overflow";
        case LIXA_RC_OBJ_NOT_INITIALIZED:
            return "ERROR: object is not initialized";
        case  LIXA_RC_OBJ_CORRUPTED:
            return "ERROR: object is corrupted";
        case LIXA_RC_CORRUPTED_STATUS_FILE:
            return "ERROR: the status file is corrupted and can not be used";
        case LIXA_RC_INVALID_OPTION:
            return "ERROR: a specified option is not valid";
        case LIXA_RC_PROTOCOL_ERROR:
            return "ERROR: a routine has been invoked in an improper context";
        case LIXA_RC_INVALID_STATUS:
                  return "ERROR: invalid object status";
        case LIXA_RC_TOO_MANY_RSRMGRS:
            return "ERROR: too many resource managers";
        case LIXA_RC_INVALID_PREFIX_SIZE:
            return "ERROR: the number of chars of the prefix of the XML "
                "message";
        case LIXA_RC_EMPTY_XML_MSG:
            return "ERROR: the XML message is empty";
        case LIXA_RC_MALFORMED_XML_MSG:
            return "ERROR: the XML message is malformed and cannot be "
                "interpreted";
        case LIXA_RC_INVALID_LENGTH_XML_MSG:
            return "ERROR: the length of the XML message differs from prefix";
        case LIXA_RC_PROPERTY_INVALID_VALUE:
            return "ERROR: a value of a property is invalid";
        case LIXA_RC_XML_UNRECOGNIZED_TAG:
            return "ERROR: the XML contains a tag is not known or is "
                "in the wrong place";
        case LIXA_RC_ASYNC_NOT_IMPLEMENTED:
            return "ERROR: an operation is referring to asynchronous mode "
                "that is not yet implemented";
        case LIXA_RC_UNSUPPORTED_OPTION:
            return "ERROR: the specified option might be valid, but it's not "
                "(yet) supported by LIXA";
        case LIXA_RC_FILE_NOT_EXISTS:
            return "ERROR: a specified file can not be opened because it does "
                "not exist";
        case LIXA_RC_ABORTED_RECOVERY:
            return "ERROR: a transaction can not be recovered";
        case LIXA_RC_RECOVERY_INFO_MISMATCH:
            return "ERROR: client/server recovery configuration do not match";
        case LIXA_RC_MALFORMED_XID:
            return "ERROR: a malformed XID has been discovered";
        case LIXA_RC_MESSAGE_LEVEL_MISMATCH:
            return "ERROR: a message with a wrong level has been detected";
        case LIXA_RC_BRANCHES_ON_MULTIPLE_THREADS:
            return "ERROR: branches of the same global transaction are "
                "managed by multiple threads";
        case LIXA_RC_MULTIBRANCH_PREPARE_FAILED:
            return "ERROR: multiple branches prepare has been failed due to "
                "one or more branches";
        case LIXA_RC_LAST_STEP_EXCEEDED:
            return "ERROR: last step for the verb has been already reached";
        case LIXA_RC_INVALID_STATE_TRANSITION:
            return "ERROR: an invalid state transtion has been requested to "
                "a Finite State Machine";
        case LIXA_RC_MESSAGE_TIMEOUT_EXPIRED:
            return "ERROR: a message has not been received before timeout "
                "expiration";
        case LIXA_RC_RM_ERROR:
            return "ERROR: generic errof for a Resource Manager operation";
        case LIXA_RC_TX_FAIL:
            return "ERROR: the client status is unknown due to a "
                "previous TX_FAIL";
        case LIXA_RC_TX_ERROR:
            return "ERROR: generic error for a TX error (a TX return code "
                "not equal TX_OK)";
        case LIXA_RC_XA_ERROR:
            return "ERROR: an XA function returned an unexpected return code";
        case LIXA_RC_MALLOC_ERROR:
            return "ERROR: 'malloc'/'g_malloc' function returned an error "
                "condition";
        case LIXA_RC_REALLOC_ERROR:
            return "ERROR: 'realloc' function returned an error condition";
        case LIXA_RC_STRDUP_ERROR:
            return "ERROR: 'strdup' function returned an error condition";
        case LIXA_RC_FORK_ERROR:
            return "ERROR: 'fork' function returned an error condition";
        case LIXA_RC_KILL_ERROR:
            return "ERROR: 'kill' function returned an error condition";
        case LIXA_RC_POLL_ERROR:
            return "ERROR: 'poll' function returned an error condition";
        case LIXA_RC_PIPE_ERROR:
            return "ERROR: 'pipe' function returned an error condition";
        case LIXA_RC_OPEN_ERROR:
            return "ERROR: 'open' function returned an error condition";
        case LIXA_RC_CLOSE_ERROR:
            return "ERROR: 'close' function returned an error condition";
        case LIXA_RC_TRUNCATE_ERROR:
            return "ERROR: 'truncate' function returned an error condition";
        case LIXA_RC_WRITE_ERROR:
            return "ERROR: 'write' function returned an error condition";
        case LIXA_RC_READ_ERROR:
            return "ERROR: 'read' function returned an error condition";
        case LIXA_RC_PATHCONF_ERROR:
            return "ERROR: 'pathconf' function returned an error condition";
        case LIXA_RC_REALPATH_ERROR:
            return "ERROR: 'realpath' function returned an error condition";
        case LIXA_RC_FOPEN_ERROR:
            return "ERROR: 'fopen' function returned an error condition";
        case LIXA_RC_FCLOSE_ERROR:
            return "ERROR: 'fclose' function returned an error condition";
        case LIXA_RC_FGETS_ERROR:
            return "ERROR: 'fgets' function returned an error condition";
        case LIXA_RC_WAITPID_ERROR:
            return "ERROR: 'waitpid' function returned an error condition";
            /*
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
        case LIXA_RC_STAT_ERROR:
            return "ERROR: 'stat' function returned an error condition";
        case LIXA_RC_FSTAT_ERROR:
            return "ERROR: 'fstat' function returned an error condition";
        case LIXA_RC_MMAP_ERROR:
            return "ERROR: 'mmap' function returned an error condition";
        case LIXA_RC_MUNMAP_ERROR:
            return "ERROR: 'munmap' function returned an error condition";
        case LIXA_RC_MSYNC_ERROR:
            return "ERROR: 'msync' function returned an error condition";
            /*
              case LIXA_RC_VSNPRINTF_ERROR:
              return "ERROR: 'vsnprintf' function returned an error "
              "condition";
              case LIXA_RC_TIMES_ERROR:
              return "ERROR: 'times' function returned an error condition";
            */
        case LIXA_RC_UUID_PARSE_ERROR:
            return "ERROR: 'uuid_parse' function returned an error condition";
        case LIXA_RC_LOCALTIME_ERROR:
            return "ERROR: 'localtime/localtime_r' function returned an error "
                "condition";
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
            return "ERROR: 'setsockopt' function returned an error condition";
        case LIXA_RC_GETSOCKOPT_ERROR:
            return "ERROR: 'getsockopt' function returned an error condition";
        case LIXA_RC_BIND_ERROR:
            return "ERROR: 'bind' function returned an error condition";
        case LIXA_RC_LISTEN_ERROR:
            return "ERROR: 'listen' function returned an error condition";
        case LIXA_RC_ACCEPT_ERROR:
            return "ERROR: 'accept' function returned an error condition";
        case LIXA_RC_SHUTDOWN_ERROR:
            return "ERROR: 'shutdown' function returned an error condition";
        case LIXA_RC_GETADDRINFO_ERROR:
            return "ERROR: 'getaddrinfo' function returned an error condition";
        case LIXA_RC_CONNECT_ERROR:
            return "ERROR: 'connect' function returned an error condition";
        case LIXA_RC_SEND_ERROR:
            return "ERROR: 'send' function returned an error condition";
        case LIXA_RC_RECV_ERROR:
            return "ERROR: 'recv' function returned an error condition";
        case LIXA_RC_GETSOCKNAME_ERROR:
            return "ERROR: 'getsockname' function returned an error condition";
        case LIXA_RC_GETPEERNAME_ERROR:
            return "ERROR: 'getpeername' function returned an error condition";
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
            return "ERROR: 'xmlReadFile' function returned an error condition";
        case LIXA_RC_XML_READ_DOC_ERROR:
            return "ERROR: 'xmlReadDoc' function returned an error "
                "condition";
        case LIXA_RC_XML_READ_MEMORY_ERROR:
            return "ERROR: 'xmlReadMemory' function returned an error "
                "condition";
        case LIXA_RC_XML_DOC_GET_ROOT_ELEMENT_ERROR:
            return "ERROR: 'xmlDocGetRootElement' function returned an "
                "error condition";
        case LIXA_RC_XML_CHAR_STRDUP_ERROR:
            return "ERROR: 'xmlCharStrdup' function returned a NULL pointer";
        case LIXA_RC_XML_STRDUP_ERROR:
            return "ERROR: 'xmlStrdup' function returned a NULL pointer";
        case LIXA_RC_G_ARRAY_NEW_ERROR:
            return "ERROR: 'g_array_new' function returned an "
                "error condition";
        case LIXA_RC_G_BASE64_DECODE_ERROR:
            return "ERROR: 'g_base64_decode' function returned an "
                "error condition";
        case LIXA_RC_G_BASE64_ENCODE_ERROR:
            return "ERROR: 'g_base64_encode' function returned an "
                "error condition";
        case LIXA_RC_G_COMPUTE_CHECKSUM_FOR_DATA:
            return "ERROR: 'g_compute_checksum_for_data' function returned "
                "an error condition";
        case LIXA_RC_G_CHECKSUM_GET_STRING_ERROR:
            return "ERROR: 'g_checksum_get_string' function returned an "
                "error condition";
        case LIXA_RC_G_CHECKSUM_NEW_ERROR:
            return "ERROR: 'g_checksum_new' function returned an "
                "error condition";
        case LIXA_RC_G_HASH_TABLE_NEW_ERROR:
            return "ERROR: 'g_hash_table_new' function returned an "
                "error condition";
        case LIXA_RC_G_MODULE_CLOSE_ERROR:
            return "ERROR: 'g_module_close' function returned an "
                "error condition";
        case LIXA_RC_G_MODULE_OPEN_ERROR:
            return "ERROR: 'g_module_open' function returned an "
                "error condition";
        case LIXA_RC_G_MODULE_SYMBOL_ERROR:
            return "ERROR: 'g_module_symbol' function returned an "
                "error condition";
        case LIXA_RC_G_PTR_ARRAY_NEW_ERROR:
            return "ERROR: 'g_ptr_array_new' function returned an "
                "error condition";
        case LIXA_RC_G_RETURNED_NULL:
            return "ERROR:  a glib function returned a NULL pointer; the "
                "function is not documented as returnig NULL. This is "
                "an internal error";
        case LIXA_RC_G_STRCONCAT_ERROR:
            return "ERROR: 'g_strconcat' function returned an error condition";
        case LIXA_RC_G_STRDUP_ERROR:
            return "ERROR: 'g_strdup' function returned an error condition";
        case LIXA_RC_G_THREAD_CREATE_ERROR:
            return "ERROR: 'g_thread_create' function returned an "
                "error condition";
        case LIXA_RC_G_TRY_MALLOC_ERROR:
            return "ERROR: 'g_try_malloc'/'g_try_malloc0' function returned "
                "an error condition";
        case LIXA_RC_RESOURCE_ALREADY_REGISTERED:
            return "ERROR: the XA Resource has been already registered to a "
                "different Transaction Manager";
        case LIXA_RC_TX_ROLLBACK:
            return "ERROR: the XA transaction was rolled back (like "
                "TX_ROLLBACK in TX transaction demarcation specification)";
        case LIXA_RC_TX_MIXED:
            return "ERROR: the XA transaction was partially committed and "
                "partially rolled back (like TX_MIXED in TX transaction "
                "demarcation specification)";
        case LIXA_RC_TX_HAZARD:
            return "ERROR: the XA transaction may have been partially "
                "committed and partially rolled back (like TX_HAZARD in TX "
                "transaction demarcation specification)";
        case LIXA_RC_NON_BRANCHABLE_TX:
            return "ERROR: the XA transaction can not be branched because it "
                "has been create as non branchable";
        case LIXA_RC_NON_REUSABLE_TX:
            return "ERROR: the XA transaction can not be reused, a new one "
                "must be created";
        case LIXA_RC_NON_DISPOSABLE_TX:
            return "ERROR: the XA transaction object can not be safely "
                "disposed because the transaction has not yet ended";
        /* JNI related errors; JNI is used in XTA for Java */
        case LIXA_RC_FIND_CLASS_ERROR:
            return "ERROR: 'JNI FindClass' function returned NULL pointer";
        case LIXA_RC_GET_DIRECT_BUFFER_ADDRESS_ERROR:
            return "ERROR: 'JNI GetDirectBufferAddress' function returned "
                "NULL pointer";
        case LIXA_RC_GET_ENV_ERROR:
            return "ERROR: 'JNI GetEnv' function returned NULL pointer";
        case LIXA_RC_GET_FIELD_ID_ERROR:
            return "ERROR: 'JNI GetFieldID' function returned NULL pointer";
        case LIXA_RC_GET_JAVA_VM_ERROR:
            return "ERROR: 'JNI GetJavaVM' function returned NULL pointer";
        case LIXA_RC_GET_METHOD_ID_ERROR:
            return "ERROR: 'JNI GetMethodID' function returned NULL pointer";
        case LIXA_RC_GET_OBJECT_CLASS_ERROR:
            return "ERROR: 'JNI GetObjectClass' function returned NULL "
                "pointer";
        case LIXA_RC_GET_OBJECT_FIELD_ERROR:
            return "ERROR: 'JNI GetObjectField' function returned NULL "
                "pointer";
        case LIXA_RC_GET_STRING_UTF_CHARS_ERROR:
            return "ERROR: 'JNI GetStringUTFChars' function returned an error";
        case LIXA_RC_GET_VERSION_ERROR:
            return "ERROR: 'JNI GetVersion' function returned an error";
        case LIXA_RC_NEW_BYTE_ARRAY_ERROR:
            return "ERROR: 'JNI NewByteArray' function returned NULL "
                "pointer";
        case LIXA_RC_NEW_DIRECT_BYTE_BUFFER_ERROR:
            return "ERROR: 'JNI NewDirectByteBuffer' function returned NULL "
                "pointer";
        case LIXA_RC_NEW_GLOBAL_REF_ERROR:
            return "ERROR: 'JNI NewGlobalRef' function returned NULL pointer";
        case LIXA_RC_NEW_OBJECT_ERROR:
            return "ERROR: 'JNI NewObject' function returned NULL pointer";
        case LIXA_RC_NEW_STRING_UTF_ERROR:
            return "ERROR: 'JNI NewStringUTF' function returned NULL pointer";
        case LIXA_RC_SET_BYTE_ARRAY_REGION_ERROR:
            return "ERROR: 'JNI SetByteArrayRegion' function returned NULL "
                "pointer";
        case LIXA_RC_SET_OBJECT_FIELD_ERROR:
            return "ERROR: 'JNI SetObjectField' function returned NULL "
                "pointer";
        default:
            return "ERROR: unknown error";
    } /* switch (ret_cod) */
}
