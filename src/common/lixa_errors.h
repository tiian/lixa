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
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXA_ERRORS_H
# define LIXA_ERRORS_H



/*********************************************************
 *                                                       *
 * REASON / RETURN CODES                                 *
 *                                                       *
 * for LIXA and XTA functions                            *
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
#define LIXA_RC_OBJ_NOT_FOUND                    +1
/**
 * The container is empty
 */
#define LIXA_RC_EMPTY_CONTAINER                  +2
/**
 * An operation that can not be performed and can safely bypassed has been
 * requested: the program can go on as no operation was requested
 */
#define LIXA_RC_BYPASSED_OPERATION               +3
/**
 * Peer has closed TCP/IP connection
 */
#define LIXA_RC_CONNECTION_CLOSED                +4
/**
 * A truncation occurred because the destination is smaller then the source
 */
#define LIXA_RC_TRUNCATION_OCCURRED              +5
/**
 * This thread of control should recover some recovery pending transactions
 */
#define LIXA_RC_RECOVERY_PENDING_TX              +6
/**
 * The digest of the lixac config file changed -> the client config file
 * changed
 */
#define LIXA_RC_LIXAC_CONF_CHANGED               +7
/**
 * The thread is serving the client must be switched to a different one
 */
#define LIXA_RC_THREAD_SWITCH                    +8
/**
 * Shutdown must be performed
 */
#define LIXA_RC_ASKED_SHUTDOWN                   +9
/**
 * Only maintenance mode is allowed
 */
#define LIXA_RC_MAINTENANCE_MODE                +10
/**
 * The Application Program wants to branch an existing superior branch, but
 * it does not exist. The transaction can go on as a standard one.
 */
#define LIXA_RC_NO_SUPERIOR_BRANCH              +11
/**
 * The Application Program wants to branch an existing superior branch, but
 * the superior branch has already started to prepare the global transacation
 */
#define LIXA_RC_NOT_CHAINABLE_BRANCH            +12
/**
 * An operation must be postponed because not all conditions are met
 */
#define LIXA_RC_OPERATION_POSTPONED             +13
/**
 * The operation would block the client, but "NON BLOCK" option has been
 * specified
 */
#define LIXA_RC_WOULD_BLOCK                     +14
/**
 * The operation can not be completed due to an error happened in a different
 * branch that's part of the same global transaction
 */
#define LIXA_RC_OTHER_BRANCH_ERROR              +15



/**
 * Successfully completion
 */
#define LIXA_RC_OK                                0



/**
 * Internal error: unrecoverable status!
 */
#define LIXA_RC_INTERNAL_ERROR                   -1
/**
 * A parameter passed to a function is OUT OF RANGE
 */
#define LIXA_RC_OUT_OF_RANGE                     -2
/**
 * Configuration file is broken
 */
#define LIXA_RC_CONFIG_ERROR                     -3
/**
 * Unespected network event
 */
#define LIXA_RC_NETWORK_EVENT_ERROR              -4
/**
 * A passed or returned object/option/arg is NULL and it can NOT be inferred
 * from a default value
 */
#define LIXA_RC_NULL_OBJECT                      -5
/**
 * The container is full and can NOT store more elements
 */
#define LIXA_RC_CONTAINER_FULL                   -6
/**
 * The process has been stopped to avoid a buffer overflow
 */
#define LIXA_RC_BUFFER_OVERFLOW                  -7
/**
 * A NOT initialized object has been passed to a method/function
 */
#define LIXA_RC_OBJ_NOT_INITIALIZED              -8
/**
 * A corrupted object has been passed to a function
 */
#define LIXA_RC_OBJ_CORRUPTED                    -9
/**
 * Status files are corrupted and the server can not start-up
 */
#define LIXA_RC_CORRUPTED_STATUS_FILE           -10
/**
 * A specified option is not valid for method and/or object status
 */
#define LIXA_RC_INVALID_OPTION                  -11
/**
 * A routine has been invoked in an improper context
 */
#define LIXA_RC_PROTOCOL_ERROR                  -12
/**
 * The status (value of any properties) of an object is invalid due to a bug
 * located elsewhere (a complex "internal error" condition)
 */
#define LIXA_RC_INVALID_STATUS                  -13
/**
 * Too many resource managers
 */
#define LIXA_RC_TOO_MANY_RSRMGRS                -14
/**
 * The number of chars of the prefix of the XML message 
 */
#define LIXA_RC_INVALID_PREFIX_SIZE             -15
/**
 * The XML message is empty and can not be processed
 */
#define LIXA_RC_EMPTY_XML_MSG                   -16
/**
 * The XML message is malformed and can not be processed
 */
#define LIXA_RC_MALFORMED_XML_MSG               -17
/**
 * The length of the XML message differs from prefix 
 */
#define LIXA_RC_INVALID_LENGTH_XML_MSG          -18
/**
 * The XML message is malformed and can not be processed
 */
#define LIXA_RC_PROPERTY_INVALID_VALUE          -19
/**
 * The XML contains a tag is not known or is in the wrong place
 */
#define LIXA_RC_XML_UNRECOGNIZED_TAG            -20
/**
 * An operation is referring to asynchronous mode that is not yet implemented
 */
#define LIXA_RC_ASYNC_NOT_IMPLEMENTED           -21
/**
 * The specified option might be valid, but it's not (yet) supported by LIXA
 */
#define LIXA_RC_UNSUPPORTED_OPTION              -22
/**
 * A specified file can not be opened because it does not exist
 */
#define LIXA_RC_FILE_NOT_EXISTS                 -23
/**
 * A transaction can not be recovered
 */
#define LIXA_RC_ABORTED_RECOVERY                -24
/**
 * Client/server recovery configuration do not match 
 */
#define LIXA_RC_RECOVERY_INFO_MISMATCH          -25
/**
 * A malformed XID has been discovered
 */
#define LIXA_RC_MALFORMED_XID                   -26
/**
 * A message with a wrong level has been detected
 */
#define LIXA_RC_MESSAGE_LEVEL_MISMATCH          -27
/**
 * Branches of the same global transaction are managed by multiple threads
 */
#define LIXA_RC_BRANCHES_ON_MULTIPLE_THREADS    -28
/**
 * Multiple branches prepare has been failed due to one or more branches
 */
#define LIXA_RC_MULTIBRANCH_PREPARE_FAILED      -29
/**
 * Last step for the verb has been already reached
 */
#define LIXA_RC_LAST_STEP_EXCEEDED              -30
/**
 * An invalid state transtion has been requested to a Finite State Machine
 */
#define LIXA_RC_INVALID_STATE_TRANSITION        -31
/**
 * A message has not been received before timeout expiration and the TCP
 * socket has been closed
 */
#define LIXA_RC_MSG_TIMEOUT_SOCKET_CLOSED       -32
/**
 * The magic number in the state file is not valid for the current version of
 * the state engine
 */
#define LIXA_RC_INVALID_MAGIC_NUMBER            -33



/**
 * Generic error for a Resource Manager operation
 */
#define LIXA_RC_RM_ERROR                        -96
/**
 * The client status is unknown due to a previous TX_FAIL
 */
#define LIXA_RC_TX_FAIL                         -97
/**
 * Generic error for an TX error (an TX return code not equal TX_OK)
 */
#define LIXA_RC_TX_ERROR                        -98
/**
 * Generic error for an XA error (an XA return code not equal XA_OK)
 */
#define LIXA_RC_XA_ERROR                        -99

/**
 * "malloc"/"g_malloc" function error
 */
#define LIXA_RC_MALLOC_ERROR                   -100
/**
 * "realloc" function error
 */
#define LIXA_RC_REALLOC_ERROR                  -101
/**
 * "strdup" function error
 */
#define LIXA_RC_STRDUP_ERROR                   -102
/**
 * "fork" function error
 */
#define LIXA_RC_FORK_ERROR                     -103
/**
 * "kill" function error
 */
#define LIXA_RC_KILL_ERROR                     -104
/**
 * "poll" function error
 */
#define LIXA_RC_POLL_ERROR                     -108
/**
 * "pipe" function error
 */
#define LIXA_RC_PIPE_ERROR                     -109
/**
 * "open" function error
 */
#define LIXA_RC_OPEN_ERROR                     -110
/**
 * "close" function error
 */
#define LIXA_RC_CLOSE_ERROR                    -111
/**
 * "truncate" or "ftruncate" function error
 */
#define LIXA_RC_TRUNCATE_ERROR                 -112
/**
 * "write" function error
 */
#define LIXA_RC_WRITE_ERROR                    -113
/**
 * "fread" function error
 */
#define LIXA_RC_READ_ERROR                     -114
/**
 * "pathconf" function error
 */
#define LIXA_RC_PATHCONF_ERROR                 -115
/**
 * "realpath" function error
 */
#define LIXA_RC_REALPATH_ERROR                 -116
/**
 * "fopen" function error
 */
#define LIXA_RC_FOPEN_ERROR                    -117
/**
 * "fclose" function error
 */
#define LIXA_RC_FCLOSE_ERROR                   -118
/**
 * "fgets" function/macro error
 */
#define LIXA_RC_FGETS_ERROR                    -119
/**
 * "waitpit" function error
 */
#define LIXA_RC_WAITPID_ERROR                  -120
/**
 * "posix_memalign" function error
 */
#define LIXA_RC_POSIX_MEMALIGN_ERROR           -121
/**
 * "stat" function error
 */
#define LIXA_RC_STAT_ERROR                     -122
/**
 * "fstat" function error
 */
#define LIXA_RC_FSTAT_ERROR                    -123
/**
 * "mmap" function error
 */
#define LIXA_RC_MMAP_ERROR                     -124
/**
 * "munmap" function error
 */
#define LIXA_RC_MUNMAP_ERROR                   -125
/**
 * "msync" function error
 */
#define LIXA_RC_MSYNC_ERROR                    -126

/**
 * "uuid_parse" function error
 */
#define LIXA_RC_UUID_PARSE_ERROR               -127
/**
 * "localtime/localtime_r" function error
 */
#define LIXA_RC_LOCALTIME_ERROR                -128
/**
 * "gettimeofday" function error
 */
#define LIXA_RC_GETTIMEOFDAY_ERROR             -129
/**
 * "socket" function error
 */
#define LIXA_RC_SOCKET_ERROR                   -130
/**
 * "setsockopt" function error
 */
#define LIXA_RC_SETSOCKOPT_ERROR               -131
/**
 * "getsockopt" function error
 */
#define LIXA_RC_GETSOCKOPT_ERROR               -132
/**
 * "bind" function error
 */
#define LIXA_RC_BIND_ERROR                     -133
/**
 * "bind" function error
 */
#define LIXA_RC_LISTEN_ERROR                   -134
/**
 * "accept" function error
 */
#define LIXA_RC_ACCEPT_ERROR                   -135
/**
 * "shutdown" function error
 */
#define LIXA_RC_SHUTDOWN_ERROR                 -136
/**
 * "getaddrinfo" function error
 */
#define LIXA_RC_GETADDRINFO_ERROR              -137
/**
 * "connect" function error
 */
#define LIXA_RC_CONNECT_ERROR                  -138
/**
 * "recv" function error
 */
#define LIXA_RC_SEND_ERROR                     -139
/**
 * "recv" function error
 */
#define LIXA_RC_RECV_ERROR                     -140
/**
 * "getsockname" function error
 */
#define LIXA_RC_GETSOCKNAME_ERROR              -141
/**
 * "getsockname" function error
 */
#define LIXA_RC_GETPEERNAME_ERROR              -142
/**
 * "unlink" function error
 */
#define LIXA_RC_UNLINK_ERROR                   -143
/**
 * "pthread_create" function error
 */
#define LIXA_RC_PTHREAD_CREATE_ERROR           -150
/**
 * "pthread_cond_destroy" function error
 */
#define LIXA_RC_PTHREAD_COND_DESTROY_ERROR     -151
/**
 * "pthread_cond_init" function error
 */
#define LIXA_RC_PTHREAD_COND_INIT_ERROR        -152
/**
 * "pthread_cond_signal" function error
 */
#define LIXA_RC_PTHREAD_COND_SIGNAL_ERROR      -153
/**
 * "pthread_cond_wait" function error
 */
#define LIXA_RC_PTHREAD_COND_WAIT_ERROR        -154
/**
 * "pthread_join" function error
 */
#define LIXA_RC_PTHREAD_JOIN_ERROR             -155
/**
 * "pthread_mutex_destroy" function error
 */
#define LIXA_RC_PTHREAD_MUTEX_DESTROY_ERROR    -156
/**
 * "pthread_mutex_init" function error
 */
#define LIXA_RC_PTHREAD_MUTEX_INIT_ERROR       -157
/**
 * "pthread_mutex_lock" function error
 */
#define LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR       -158
/**
 * "pthread_mutex_unlock" function error
 */
#define LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR     -159
/**
 * "pthread_rwlock_wrlock" function error
 */
#define LIXA_RC_PTHREAD_RWLOCK_WRLOCK_ERROR    -160
/**
 * "pthread_rwlock_rlock" function error
 */
#define LIXA_RC_PTHREAD_RWLOCK_RDLOCK_ERROR    -161
/**
 * "pthread_rwlock_unlock" function error
 */
#define LIXA_RC_PTHREAD_RWLOCK_UNLOCK_ERROR    -162
/**
 * "pread" function error
 */
#define LIXA_RC_PREAD_ERROR                    -163
/**
 * "pwrite" function error
 */
#define LIXA_RC_PWRITE_ERROR                   -164
/**
 * "lseek" function error
 */
#define LIXA_RC_LSEEK_ERROR                    -165
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
 * "xmlCharStrdup" function error
 */
#define LIXA_RC_XML_CHAR_STRDUP_ERROR          -204
/**
 * "xmlStrdup" function error
 */
#define LIXA_RC_XML_STRDUP_ERROR               -205
/**
 * "g_array_new" function error
 */
#define LIXA_RC_G_ARRAY_NEW_ERROR              -300
/**
 * "g_base64_decode" function error
 */
#define LIXA_RC_G_BASE64_DECODE_ERROR          -301
/**
 * "g_base64_encode" function error
 */
#define LIXA_RC_G_BASE64_ENCODE_ERROR          -302
/**
 * "g_checksum_get_string" function error
 */
#define LIXA_RC_G_CHECKSUM_GET_STRING_ERROR    -303
/**
 * "g_checksum_new" function error
 */
#define LIXA_RC_G_CHECKSUM_NEW_ERROR           -304
/**
 * "g_compute_checksum_for_data" function error
 */
#define LIXA_RC_G_COMPUTE_CHECKSUM_FOR_DATA    -305
/**
 * "g_hash_table_new" function error
 */
#define LIXA_RC_G_HASH_TABLE_NEW_ERROR         -306
/**
 * "g_module_close" function error
 */
#define LIXA_RC_G_MODULE_CLOSE_ERROR           -307
/**
 * "g_module_open" function error
 */
#define LIXA_RC_G_MODULE_OPEN_ERROR            -308
/**
 * "g_module_symbol" function error
 */
#define LIXA_RC_G_MODULE_SYMBOL_ERROR          -309
/**
 * "g_ptr_array_new" function error
 */
#define LIXA_RC_G_PTR_ARRAY_NEW_ERROR          -310
/**
 * A glib function returned a NULL pointer; the function is not documented as
 * returnig NULL. This is basically an internal error
 */
#define LIXA_RC_G_RETURNED_NULL                -311
/**
 * "g_strconcat" function error
 */
#define LIXA_RC_G_STRCONCAT_ERROR              -312
/**
 * "g_strdup" function error
 */
#define LIXA_RC_G_STRDUP_ERROR                 -313
/**
 * "g_thread_create" function error
 */
#define LIXA_RC_G_THREAD_CREATE_ERROR          -314
/**
 * "g_try_malloc"/"g_try_malloc0" function error
 */
#define LIXA_RC_G_TRY_MALLOC_ERROR             -315



/*
 * NOTE:
 * Range [-500,-599] is reserved for XTA specific error codes and must NOT
 * be used for LIXA generic codes
 */
/**
 * The XA Resource has been already registered to a different Transaction
 * Manager
 */
#define LIXA_RC_RESOURCE_ALREADY_REGISTERED    -500
/**
 * The XA transaction was rolled back (like TX_ROLLBACK in TX transaction
 * demarcation specification)
 */
#define LIXA_RC_TX_ROLLBACK                    -502
/**
 * The XA transaction was partially committed and partially rolled back (like
 * TX_MIXED in TX transaction demarcation specification)
 */
#define LIXA_RC_TX_MIXED                       -503
/**
 * The XA transaction may have been partially committed and partially rolled
 * back (like TX_HAZARD in TX transaction demarcation specification)
 */
#define LIXA_RC_TX_HAZARD                      -504
/**
 * The XA transaction can not be branched because it has been create as non
 * branchable
 */
#define LIXA_RC_NON_BRANCHABLE_TX              -505
/**
 * The XA transaction object can not be reused: a new one must be created
 */
#define LIXA_RC_NON_REUSABLE_TX                -506
/**
 * The XA transaction object can not be safely disposed because the transaction
 * has not yet ended
 */
#define LIXA_RC_NON_DISPOSABLE_TX              -507


/* JNI related errors; JNI is used in XTA for Java */

/**
 * JNI "FindClass" function error
 */
#define LIXA_RC_FIND_CLASS_ERROR                -510
/**
 * JNI "GetDirectBufferAddress" function error
 */
#define LIXA_RC_GET_DIRECT_BUFFER_ADDRESS_ERROR -511
/**
 * JNI "GetEnv" function error
 */
#define LIXA_RC_GET_ENV_ERROR                   -512
/**
 * JNI "GetFieldID" function error
 */
#define LIXA_RC_GET_FIELD_ID_ERROR              -513
/**
 * JNI "GetJavaVM" function error
 */
#define LIXA_RC_GET_JAVA_VM_ERROR               -514
/**
 * JNI "GetMethodID" function error
 */
#define LIXA_RC_GET_METHOD_ID_ERROR             -515
/**
 * JNI "GetObjectClass" function error
 */
#define LIXA_RC_GET_OBJECT_CLASS_ERROR          -516
/**
 * JNI "GetObjectField" function error
 */
#define LIXA_RC_GET_OBJECT_FIELD_ERROR          -517
/**
 * JNI "GetStringUTFChars" function error
 */
#define LIXA_RC_GET_STRING_UTF_CHARS_ERROR      -518
/**
 * JNI "GetVersion" function error
 */
#define LIXA_RC_GET_VERSION_ERROR               -519
/**
 * JNI "NewByteArray" function error
 */
#define LIXA_RC_NEW_BYTE_ARRAY_ERROR            -520
/**
 * JNI "NewDirectByteBuffer" function error
 */
#define LIXA_RC_NEW_DIRECT_BYTE_BUFFER_ERROR    -521
/**
 * JNI "NewGlobalRef" function error
 */
#define LIXA_RC_NEW_GLOBAL_REF_ERROR            -522
/**
 * JNI "NewObject" function error
 */
#define LIXA_RC_NEW_OBJECT_ERROR                -523
/**
 * JNI "NewObject" function error
 */
#define LIXA_RC_NEW_STRING_UTF_ERROR            -524
/**
 * JNI "SetByteArrayRegion" function error
 */
#define LIXA_RC_SET_BYTE_ARRAY_REGION_ERROR     -525
/**
 * JNI "SetObjectField" function error
 */
#define LIXA_RC_SET_OBJECT_FIELD_ERROR          -526



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



      /**
       * <B>PUBLIC METHOD</B><BR>
       * Retrieve the description associated to a return/reason code
       * @param[in] ret_cod return/reason code of the desired description
       * @return a const string containing a description of reason code
       */
      const char *lixa_strerror(int ret_cod);
      
      

#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_ERRORS_H */


