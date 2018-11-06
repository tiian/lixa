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



/* This is an helper function, is not directly called by JNI */
int Java_org_tiian_lixa_xta_Transaction_set_native_object(
    JNIEnv *env, jobject this_obj, xta_transaction_t *tx)
{
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , NEW_DIRECT_BYTE_BUFFER_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_set_native_object\n"));

    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_set_native_object/"
                "GetObjectClass() returned NULL");
            THROW(GET_OBJECT_CLASS_ERROR);
        }
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_set_native_object/"
                "GetFieldID() returned NULL");
            THROW(GET_FIELD_ID_ERROR);
        }
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)tx, sizeof(xta_transaction_t)))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_set_native_object/"
                "NewDirectByteBuffer() returned NULL");
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR);
        }
        /* set ByteBuffer reference */
        (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
        
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
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_set_native_object"
                "/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



/* This is an helper internal function, it's not seen by JNI */
xta_transaction_t*
Java_org_tiian_lixa_xta_Transaction_getNativeObject(
    JNIEnv *env, jobject this_obj)
{
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    xta_transaction_t *tx;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_"
                "getNativeObject\n"));

    /* get a reference to this object's class */
    if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_Transaction_getNativeObject/"
            "GetObjectClass returned NULL");
    }

    /* get the field identificator */
    if (NULL == (field_id = (*env)->GetFieldID(
                     env, this_class, "NativeObject",
                     "Ljava/nio/ByteBuffer;"))) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
                        "JNI/Java_org_tiian_lixa_xta_Transaction"
            "_getNativeObject/GetFieldID returned NULL");
    }
    /* get ByteBuffer reference */
    byte_buffer = (*env)->GetObjectField(env, this_obj, field_id);
    /* cast to xta_transaction_t */
    if (NULL == (tx = (xta_transaction_t *)
                 (*env)->GetDirectBufferAddress(
                     env, byte_buffer))) {
        jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
        (*env)->ThrowNew(
            env, Exception,
            "JNI/Java_org_tiian_lixa_xta_Transaction_"
            "getNativeObject/GetDirectBufferAddress returned NULL");
    }
    return tx;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    deleteJNI
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_deleteJNI(
    JNIEnv *env, jobject this_obj)
{
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_deleteJNI\n"));
    xta_transaction_delete(
        Java_org_tiian_lixa_xta_Transaction_getNativeObject(env, this_obj));
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    enlistResourceJNI
 * Signature: (Ljavax/transaction/xa/XAResource;)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI(
    JNIEnv *env, jobject this_object, jobject xa_resource)
{
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI\n"));
}
