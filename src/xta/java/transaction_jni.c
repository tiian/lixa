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
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



/* This macro is necessary to avoid header files related to native resources:
   they are not used by XTA for Java */
#define XTA_FOR_JAVA
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
/* XTA includes */
#include "xta.h"
#include "xta_java_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



/* Allocate a list for XA C resources */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_newJNI(
    JNIEnv *env, jobject this_obj, xta_transaction_t *tx)
{
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR1
                     , NEW_DIRECT_BYTE_BUFFER_ERROR1
                     , G_PTR_ARRAY_NEW_ERROR
                     , GET_FIELD_ID_ERROR2
                     , NEW_DIRECT_BYTE_BUFFER_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    GPtrArray *array = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_newJNI\n"));
    
    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_newJNI/"
                "GetObjectClass() returned NULL");
            THROW(GET_OBJECT_CLASS_ERROR);
        }
        
        /* get the field identificator for NativeObjcet */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_newJNI/"
                "GetFieldID() returned NULL");
            THROW(GET_FIELD_ID_ERROR1);
        }

        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)tx, sizeof(xta_transaction_t)))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_newJNI/"
                "NewDirectByteBuffer() returned NULL");
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR1);
        }
        
        /* create an array to store the resources that will be enlisted */
        if (NULL == (array = g_ptr_array_new())) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_newJNI/"
                "g_ptr_array_new() returned NULL");
            THROW(G_PTR_ARRAY_NEW_ERROR);
        }

        /* get the field identificator for NativeObjcet */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeResources",
                         "Ljava/nio/ByteBuffer;"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_newJNI/"
                "GetFieldID() returned NULL");
            THROW(GET_FIELD_ID_ERROR2);
        }

        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)array, sizeof(GPtrArray)))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_newJNI/"
                "NewDirectByteBuffer() returned NULL");
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR2);
        }
        
        /* set ByteBuffer reference */
        (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_OBJECT_CLASS_ERROR:
                ret_cod = LIXA_RC_GET_OBJECT_CLASS_ERROR;
                break;
            case GET_FIELD_ID_ERROR1:
            case GET_FIELD_ID_ERROR2:
                ret_cod = LIXA_RC_GET_FIELD_ID_ERROR;
                break;
            case NEW_DIRECT_BYTE_BUFFER_ERROR1:
            case NEW_DIRECT_BYTE_BUFFER_ERROR2:
                ret_cod = LIXA_RC_NEW_DIRECT_BYTE_BUFFER_ERROR;
                break;
            case G_PTR_ARRAY_NEW_ERROR:
                ret_cod = LIXA_RC_G_PTR_ARRAY_NEW_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* recover memory if necessary */
    if (NONE != excp && NULL != array)
        g_ptr_array_free(array, TRUE);
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_newJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



/* This is an helper internal function, it's not seen by JNI */
xta_transaction_t*
Java_org_tiian_lixa_xta_Transaction_getNativeObject(
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
    xta_transaction_t *tx;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_"
                "getNativeObject\n"));

    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_getNativeObject/"
                "GetObjectClass returned NULL");
            THROW(GET_OBJECT_CLASS_ERROR);
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
            THROW(GET_FIELD_ID_ERROR);
        }
        
        /* get ByteBuffer reference */
        if (NULL == (byte_buffer = (*env)->GetObjectField(
                         env, this_obj, field_id))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_"
                "getNativeObject/GetObjectField returned NULL");
            THROW(GET_OBJECT_FIELD_ERROR);
        }
        
        /* cast to xta_transaction_t */
        if (NULL == (tx = (xta_transaction_t *)
                     (*env)->GetDirectBufferAddress(
                         env, byte_buffer))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_"
                "getNativeObject/GetDirectBufferAddress returned NULL");
            THROW(GET_DIRECT_BUFFER_ADDRESS_ERROR);
        }
        
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
    } /* TRY-CATCH */
    return tx;
}



/* This is an helper internal function, it's not seen by JNI */
GPtrArray* Java_org_tiian_lixa_xta_Transaction_getNativeResources(
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
    GPtrArray *array = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_"
                "getNativeResources\n"));

    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_getNativeResources/"
                "GetObjectClass returned NULL");
            THROW(GET_OBJECT_CLASS_ERROR);
        }

        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeResources",
                         "Ljava/nio/ByteBuffer;"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction"
                "_getNativeResources/GetFieldID returned NULL");
            THROW(GET_FIELD_ID_ERROR);
        }
        
        /* get ByteBuffer reference */
        if (NULL == (byte_buffer = (*env)->GetObjectField(
                         env, this_obj, field_id))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_"
                "getNativeResources/GetObjectField returned NULL");
            THROW(GET_OBJECT_FIELD_ERROR);
        }
        
        /* cast to xta_transaction_t */
        if (NULL == (array = (GPtrArray *)
                     (*env)->GetDirectBufferAddress(
                         env, byte_buffer))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction_"
                "getNativeResources/GetDirectBufferAddress returned NULL");
            THROW(GET_DIRECT_BUFFER_ADDRESS_ERROR);
        }
        
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
    } /* TRY-CATCH */
    return array;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    deleteJNI
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_deleteJNI(
    JNIEnv *env, jobject this_obj)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_deleteJNI\n"));
    
    TRY {
        GPtrArray *array = NULL;
        /* delete native xta_transaction_t object */
        xta_transaction_delete(
            Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                env, this_obj));
        /* remove all XA resources */
        g_ptr_array_foreach(array, (GFunc)xta_java_xa_resource_delete, NULL);
        /* remove the array */
        g_ptr_array_free(array, TRUE);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    enlistResourceJNI
 * Signature: (Ljavax/transaction/xa/XAResource;)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI(
    JNIEnv *env, jobject this_object, jobject xa_resource)
{
    enum Exception { NULL_OBJECT
                     , GET_NATIVE_RESOURCE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xta_java_xa_resource_t *xjxr = NULL;
    GPtrArray *array = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI\n"));
    TRY {
        /* create a new native XA Java resource */
        if (NULL == (xjxr = xta_java_xa_resource_new(
                         xa_resource, "Java XA Resource"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_Transaction"
                "_enlistResourceJNI/xta_java_xa_resource_new() returned NULL");
            THROW(NULL_OBJECT);
        }
        /* add the resource to array of native resources */
        if (NULL == (array =
                     Java_org_tiian_lixa_xta_Transaction_getNativeResources(
                         env, this_object)))
            THROW(GET_NATIVE_RESOURCE_ERROR);
        
        g_ptr_array_add(array, (gpointer)xjxr);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GET_NATIVE_RESOURCE_ERROR:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* recover memory if necessary */
    if (NONE != excp && NULL != xjxr)
        xta_java_xa_resource_delete(xjxr);
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI"
                "/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}
