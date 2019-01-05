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
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , NULL_OBJECT
                     , NEW_DIRECT_BYTE_BUFFER_ERROR
                     , SET_OBJECT_FIELD_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    xta_transaction_manager_t *tm = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI\n"));

    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR);
        /* create a new native object */
        if (NULL == (tm = xta_transaction_manager_new()))
            THROW(NULL_OBJECT);
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)tm,
                         sizeof(xta_transaction_manager_t))) ||
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
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_newJNI: "
                "NativeObject set to %p\n", byte_buffer));
    return;
}



/* This is an helper internal function, it's not seen by JNI */
xta_transaction_manager_t*
Java_org_tiian_lixa_xta_TransactionManager_getNativeObject(
    JNIEnv *env, jobject this_obj)
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
    xta_transaction_manager_t *tm = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "getNativeObject\n"));
    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR);
        /* get ByteBuffer reference */
        if (NULL == (byte_buffer = (*env)->GetObjectField(
                         env, this_obj, field_id)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_FIELD_ERROR);
        /* cast to xta_transaction_manager_t */
        if (NULL == (tm = (xta_transaction_manager_t *)
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
        if (LIXA_RC_OK != ret_cod)
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "getNativeObject/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
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



/*
 * Class:     org_tiian_lixa_xta_TransactionManager
 * Method:    createTransaction
 * Signature: ()Lorg/tiian/lixa/xta/Transaction;
 */
JNIEXPORT jobject JNICALL
Java_org_tiian_lixa_xta_TransactionManager_createTransaction
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , FIND_CLASS_ERROR
                     , GET_METHOD_ID_ERROR
                     , NEW_OBJECT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    xta_transaction_manager_t *tm = NULL;
    xta_transaction_t *tx = NULL;
    jobject jtx = NULL;
    jclass class = NULL;
    jmethodID constructor = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransaction\n"));
    TRY {        
        /* retrieve the current Transaction Manager object */
        if (NULL == (tm =
                     Java_org_tiian_lixa_xta_TransactionManager_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT1);
        /* create a new native C Transaction object */
        if (NULL == (tx = xta_transaction_manager_create_transaction(tm)))
            THROW(NULL_OBJECT2);
        /* retrieve Java Transaction class */
        if (NULL == (class = (*env)->FindClass(
                         env, "org/tiian/lixa/xta/Transaction")) ||
            (*env)->ExceptionCheck(env))
            THROW(FIND_CLASS_ERROR);
        /* retrieve Transaction constructor */
        if (NULL == (constructor = (*env)->GetMethodID(
                         env, class, "<init>", "()V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR);
        /* create a new Transaction object */
        if (NULL == (jtx = (*env)->NewObject(env, class, constructor)) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_OBJECT_ERROR);
        /* populate Java object with tx native C Transaction */
        Java_org_tiian_lixa_xta_Transaction_newJNI(env, jtx, tx);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case FIND_CLASS_ERROR:
                ret_cod = LIXA_RC_FIND_CLASS_ERROR;
                break;
            case GET_METHOD_ID_ERROR:
                ret_cod = LIXA_RC_GET_METHOD_ID_ERROR;
                break;
            case NEW_OBJECT_ERROR:
                ret_cod = LIXA_RC_NEW_OBJECT_ERROR;
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_TransactionManager_"
                "createTransaction/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    /* return the Java object */
    return jtx;
}
