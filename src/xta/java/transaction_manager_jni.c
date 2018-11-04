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
/* other JNI function includes */
#include "transaction_jni.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



/* Allocate a new TransactionManager (C native object) */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_TransactionManager_newJNI(
    JNIEnv *env, jobject this_obj)
{
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    xta_transaction_manager_t *tm = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI\n"));
    /* create a new native object */
    if (NULL == (tm = xta_transaction_manager_new())) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_TransactionManager_newJNI/"
            "xta_transaction_manager_new() returned NULL");
    }
    /* get a reference to this object's class */
    if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_TransactionManager_newJNI/"
            "GetObjectClass() returned NULL");
    }
    /* get the field identificator */
    if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;"))) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_TransactionManager_newJNI/"
            "GetFieldID() returned NULL");
    }
    /* create ByteBuffer */
    if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                     env, (void *)tm, sizeof(xta_transaction_manager_t)))) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_TransactionManager_newJNI/"
            "NewDirectByteBuffer() returned NULL");
    }
    /* set ByteBuffer reference */
    (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI: "
                "NativeObject set to %p\n", byte_buffer));
    return;
}



/* This is an helper internal function, it's not seen by JNI */
xta_transaction_manager_t*
Java_org_tiian_lixa_xta_TransactionManager_getNativeObject(
    JNIEnv *env, jobject this_obj)
{
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    xta_transaction_manager_t *tm = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "getNativeObject\n"));

    /* get a reference to this object's class */
    if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
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
        Java_org_tiian_lixa_xta_TransactionManager_getNativeObject(
            env, this_obj));
}



JNIEXPORT jobject JNICALL
Java_org_tiian_lixa_xta_TransactionManager_createTransactionJNI
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , FIND_CLASS_ERROR
                     , GET_METHOD_ID_ERROR
                     , NEW_OBJECT_ERROR
                     , SET_NATIVE_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    xta_transaction_manager_t *tm = NULL;
    xta_transaction_t *tx = NULL;
    jobject jtx = NULL;
    jclass class = NULL;
    jmethodID constructor = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI\n"));
    TRY {        
        /* retrieve the current Transaction Manager object */
        tm = Java_org_tiian_lixa_xta_TransactionManager_getNativeObject(
            env, this_obj);
        /* create a new native C Transaction object */
        if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI/"
                "xta_transaction_manager_create_transaction returned NULL");
            THROW(NULL_OBJECT);
        }
        /* retrieve Java Transaction class */
        if (NULL == (class = (*env)->FindClass(
                         env, "org/tiian/lixa/xta/Transaction"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI/"
                "FindClass() returned NULL");
            THROW(FIND_CLASS_ERROR);
        }
        /* retrieve Transaction constructor */
        if (NULL == (constructor = (*env)->GetMethodID(
                         env, class, "<init>", "()V"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI/"
                "GetMethodID() returned NULL");
            THROW(GET_METHOD_ID_ERROR);
        }
        /* create a new Transaction object */
        if (NULL == (jtx = (*env)->NewObject(env, class, constructor))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI/"
                "NewObject() returned NULL");
            THROW(NEW_OBJECT_ERROR);
        }
        /* populate Java object with tx native C Transaction */
        if (LIXA_RC_OK != (
                ret_cod = 
                Java_org_tiian_lixa_xta_Transaction_set_native_object(
                    env, jtx, tx))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI/"
                "error returned by Java_org_tiian_lixa_xta_Transaction_"
                "set_native_object");
            THROW(SET_NATIVE_OBJECT);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
            case FIND_CLASS_ERROR:
            case SET_NATIVE_OBJECT:
            case NONE:
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
        
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransactionJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    /* return the Java object */
    return jtx;
}
