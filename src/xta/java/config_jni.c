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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#include <jni.h>



/* This macro is necessary to avoid header files related to native resources:
   they are not used by XTA for Java */
#define XTA_FOR_JAVA
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "client_config.h"
/* XTA includes */
#include "xta.h"
#include "config_jni.h"
#include "xtaexception_jni.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



/*
 * Create a reference to the C native pointer
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Config_newJNI(
    JNIEnv *env, jobject this_obj, xta_config_t *config)
{
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , NEW_DIRECT_BYTE_BUFFER_ERROR
                     , SET_OBJECT_FIELD_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_newJNI\n"));
    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator for nativeObject */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "nativePointer",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR);
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)config, sizeof(xta_config_t))) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR);
        /* set ByteBuffer reference */
        (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
        if ((*env)->ExceptionCheck(env))
            THROW(SET_OBJECT_FIELD_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_OBJECT_CLASS_ERROR:
                ret_cod = LIXA_RC_GET_OBJECT_CLASS_ERROR;
                break;
            case GET_FIELD_ID_ERROR:
                ret_cod = LIXA_RC_GET_FIELD_ID_ERROR;
                break;
            case NEW_DIRECT_BYTE_BUFFER_ERROR:
                ret_cod = LIXA_RC_NEW_DIRECT_BYTE_BUFFER_ERROR;
                break;
            case SET_OBJECT_FIELD_ERROR:
                ret_cod = LIXA_RC_SET_OBJECT_FIELD_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* throw Java exception */
        if (LIXA_RC_OK != ret_cod)
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_newJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



/* This is an helper internal function, it's not seen by JNI */
xta_config_t*
Java_org_tiian_lixa_xta_Config_getNativePointer(JNIEnv *env, jobject this_obj)
{
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , GET_OBJECT_FIELD_ERROR
                     , GET_DIRECT_BUFFER_ADDRESS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    xta_config_t *config = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_getNativePointer\n"));
    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "nativePointer",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR);
        /* get ByteBuffer reference */
        if (NULL == (byte_buffer = (*env)->GetObjectField(
                         env, this_obj, field_id)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_FIELD_ERROR);
        /* cast to xta_xid_t */
        if (NULL == (config = (xta_config_t *)
                     (*env)->GetDirectBufferAddress(
                         env, byte_buffer)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_DIRECT_BUFFER_ADDRESS_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_OBJECT_CLASS_ERROR:
                ret_cod = LIXA_RC_GET_OBJECT_CLASS_ERROR;
                break;
            case GET_FIELD_ID_ERROR:
                ret_cod = LIXA_RC_GET_FIELD_ID_ERROR;
                break;
            case GET_OBJECT_FIELD_ERROR:
                ret_cod = LIXA_RC_GET_OBJECT_FIELD_ERROR;
                break;
            case GET_DIRECT_BUFFER_ADDRESS_ERROR:
                ret_cod = LIXA_RC_GET_DIRECT_BUFFER_ADDRESS_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* throw Java exception */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_getNativePointer"
                "/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return config;
}



/*
 * Class:     org_tiian_lixa_xta_Config
 * Method:    getConnectionTimeout
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_Config_getConnectionTimeout
  (JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    jint ret = LIXA_CLIENT_CONNECTION_TIMEOUT_NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_getConnectionTimeout\n"));
    TRY {
        xta_config_t *config = NULL;
    
        /* retrieve the native xta_xid_t C object */
        if (NULL == (config = Java_org_tiian_lixa_xta_Config_getNativePointer(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        ret = (jint)xta_config_get_connection_timeout(config);
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* throw Java exception */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_getConnectionTimeout/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret;
}



/*
 * Class:     org_tiian_lixa_xta_Config
 * Method:    setConnectionTimeout
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Config_setConnectionTimeout
  (JNIEnv *env, jobject this_obj, jint value)
{
    enum Exception { NULL_OBJECT
                     , SET_CONNECTION_TIMEOUT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_setConnectionTimeout\n"));
    TRY {
        xta_config_t *config = NULL;
    
        /* retrieve the native xta_xid_t C object */
        if (NULL == (config = Java_org_tiian_lixa_xta_Config_getNativePointer(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        if (LIXA_RC_OK != (ret_cod = xta_config_set_connection_timeout(
                               config, (int)value)))
            THROW(SET_CONNECTION_TIMEOUT);
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case SET_CONNECTION_TIMEOUT:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* throw Java exception */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Config_setConnectionTimeout/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}
