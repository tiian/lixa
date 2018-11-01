/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
/* XTA includes */
#include "xta.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Xta_initJNI(JNIEnv *env)
{
    xta_init();
}



JNIEXPORT jstring JNICALL Java_org_tiian_lixa_xta_ErrorCodes_getText
(JNIEnv *env, jclass this_obj, jint code)
{
    return (*env)->NewStringUTF(env, lixa_strerror(code));
}



/* Allocate a new TransactionManager (C native object) */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_TransactionManager_newJNI(
    JNIEnv *env, jobject this_obj)
{
    enum Exception { MALLOC_ERROR
                     , GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , NEW_DIRECT_BYTE_BUFFER_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI\n"));
    TRY {
        jclass this_class;
        jfieldID field_id;
        jobject byte_buffer;
        xta_transaction_manager_t *tm = NULL;

        /* create a new native object */
        if (NULL == (tm = xta_transaction_manager_new()))
            THROW(MALLOC_ERROR);
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI: "
                        "this_class == NULL\n"));
            THROW(GET_OBJECT_CLASS_ERROR);
        }
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;"))) {
            LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI: "
                        "field_id == NULL\n"));
            THROW(GET_FIELD_ID_ERROR);
        }
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)tm, sizeof(xta_transaction_manager_t))))
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR);
        /* set ByteBuffer reference */
        (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case GET_OBJECT_CLASS_ERROR:
                ret_cod = LIXA_RC_GET_OBJECT_CLASS_ERROR;
                break;
            case GET_FIELD_ID_ERROR:
                ret_cod = LIXA_RC_GET_FIELD_ID_ERROR;
                break;
            case NEW_DIRECT_BYTE_BUFFER_ERROR:
                ret_cod = LIXA_RC_NEW_DIRECT_BYTE_BUFFER_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



/* This is an helper internal function, it's not seen by JNI */
xta_transaction_manager_t*
Java_org_tiian_lixa_xta_TransactionManager_getNativeObject(
    JNIEnv *env, jobject this_obj)
{
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    xta_transaction_manager_t *tm;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "getNativeObject\n"));

    /* get a reference to this object's class */
    if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
        LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                    "getNativeObject: this_class == NULL\n"));
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_TransactionManager_getNativeObject/"
            "GetObjectClass returned NULL");
    }

    /* get the field identificator */
    if (NULL == (field_id = (*env)->GetFieldID(
                     env, this_class, "NativeObject",
                     "Ljava/nio/ByteBuffer;"))) {
        LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                    "getNativeObject: field_id == NULL\n"));
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
                        "JNI/Java_org_tiian_lixa_xta_TransactionManager"
            "_getNativeObject/GetFieldID returned NULL");
    }
    /* get ByteBuffer reference */
    byte_buffer = (*env)->GetObjectField(env, this_obj, field_id);
    /* cast to xta_transaction_manager_t */
    if (NULL == (tm = (xta_transaction_manager_t *)
                 (*env)->GetDirectBufferAddress(
                     env, byte_buffer))) {
        LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                    "getNativeObject: tm == NULL\n"));
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_TransactionManager_"
            "getNativeObject/GetDirectBufferAddress returned NULL");
    }
    return tm;
}



JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_TransactionManager_deleteJNI(
    JNIEnv *env, jobject this_obj)
{
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_deleteJNI\n"));
    xta_transaction_manager_delete(
        Java_org_tiian_lixa_xta_getNativeHandle(env, this_obj));
}
