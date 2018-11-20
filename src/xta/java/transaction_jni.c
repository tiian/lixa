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



/*
 * Create a reference to C native object and allocate a list for XA C resources
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_newJNI(
    JNIEnv *env, jobject this_obj, xta_transaction_t *tx)
{
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR1
                     , NEW_DIRECT_BYTE_BUFFER_ERROR1
                     , SET_OBJECT_FIELD_ERROR1
                     , G_PTR_ARRAY_NEW_ERROR
                     , GET_FIELD_ID_ERROR2
                     , NEW_DIRECT_BYTE_BUFFER_ERROR2
                     , SET_OBJECT_FIELD_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    GPtrArray *array = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_newJNI\n"));
    
    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator for NativeObjcet */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR1);
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)tx, sizeof(xta_transaction_t))) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR1);
        /* set ByteBuffer reference */
        (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
        if ((*env)->ExceptionCheck(env))
            THROW(SET_OBJECT_FIELD_ERROR1);        
        /* create an array to store the resources that will be enlisted */
        if (NULL == (array = g_ptr_array_new()))
            THROW(G_PTR_ARRAY_NEW_ERROR);
        /* get the field identificator for NativeResources */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeResources",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR2);
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)array, sizeof(GPtrArray))) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_DIRECT_BYTE_BUFFER_ERROR2);
        /* set ByteBuffer reference */
        (*env)->SetObjectField(env, this_obj, field_id, byte_buffer);
        if ((*env)->ExceptionCheck(env))
            THROW(SET_OBJECT_FIELD_ERROR2);
        
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
            case SET_OBJECT_FIELD_ERROR1:
            case SET_OBJECT_FIELD_ERROR2:
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
        /* cast to xta_transaction_t */
        if (NULL == (tx = (xta_transaction_t *)
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
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeResources",
                         "Ljava/nio/ByteBuffer;")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_FIELD_ID_ERROR);
        /* get ByteBuffer reference */
        if (NULL == (byte_buffer = (*env)->GetObjectField(
                         env, this_obj, field_id)))
            THROW(GET_OBJECT_FIELD_ERROR);
        /* cast to xta_transaction_t */
        if (NULL == (array = (GPtrArray *)
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getNativeResources/"
                "excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return array;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    deleteJNI
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_deleteJNI(
    JNIEnv *env, jobject this_obj, jboolean already_opened)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_deleteJNI\n"));
    
    TRY {
        GPtrArray *array = NULL;
        xta_transaction_t * tx = NULL;
        
        /* retrieve the current Transaction object */
        /* close LIXA transaction is opened */
        if (already_opened) {
            tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                env, this_obj);
            if (NULL != tx &&
                LIXA_RC_OK != (ret_cod = xta_transaction_close(tx)))
                LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_"
                            "deleteJNI/xta_transaction_close: ret_cod=%d\n",
                            ret_cod));
        }
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_deleteJNI/excp=%d/"
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
    enum Exception { GET_JAVA_VM_ERROR
                     , GET_VERSION_ERROR
                     , GET_OBJECT_CLASS_ERROR
                     , GET_METHOD_ID_ERROR1
                     , NULL_OBJECT1
                     , GET_NATIVE_RESOURCE_ERROR
                     , NULL_OBJECT2
                     , TRANSACTION_ENLIST_RESOURCE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xta_java_xa_resource_t *xjxr = NULL;
    xta_transaction_t *tx = NULL;
    GPtrArray *array = NULL;
    JavaVM *java_vm = NULL;
    jint java_jni_version;
    jclass class = NULL;
    jmethodID start_method = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI\n"));
    TRY {
        /* retrieve the current Java Virtual Machine */
        if (0 != (*env)->GetJavaVM(env, &java_vm) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_JAVA_VM_ERROR);
        /* retrieve the JNI version */
        java_jni_version = (*env)->GetVersion(env);
        if ((*env)->ExceptionCheck(env))
            THROW(GET_VERSION_ERROR);
        /* retrieve the class of object xa_resource (XAResource or subclass) */
        if (NULL == (class = (*env)->GetObjectClass(env, xa_resource)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* retrieve start method */
        if (NULL == (start_method = (*env)->GetMethodID(
                         env, class, "start",
                         "(Ljavax/transaction/xa/Xid;I)V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR1);
        /* create a new native XA Java resource */
        if (NULL == (xjxr = xta_java_xa_resource_new(
                         "Java XA Resource", java_vm, java_jni_version,
                         xa_resource, start_method)))
            THROW(NULL_OBJECT1);
        /* add the resource to array of native resources */
        if (NULL == (array =
                     Java_org_tiian_lixa_xta_Transaction_getNativeResources(
                         env, this_object)))
            THROW(GET_NATIVE_RESOURCE_ERROR);
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_object)))
            THROW(NULL_OBJECT2);
        /* enlist the resource in the native library */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_enlist_resource(
                               tx, (xta_xa_resource_t *)xjxr)))
            THROW(TRANSACTION_ENLIST_RESOURCE_ERROR);
        
        g_ptr_array_add(array, (gpointer)xjxr);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_JAVA_VM_ERROR:
                ret_cod = LIXA_RC_GET_JAVA_VM_ERROR;
                break;
            case GET_VERSION_ERROR:
                ret_cod = LIXA_RC_GET_VERSION_ERROR;
                break;
            case GET_OBJECT_CLASS_ERROR:
                ret_cod = LIXA_RC_GET_OBJECT_CLASS_ERROR;
                break;
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GET_METHOD_ID_ERROR1:
                ret_cod = LIXA_RC_GET_METHOD_ID_ERROR;
                break;
            case GET_NATIVE_RESOURCE_ERROR:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case TRANSACTION_ENLIST_RESOURCE_ERROR:
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
    /* recover memory if necessary */
    if (NONE != excp && NULL != xjxr)
        xta_java_xa_resource_delete(xjxr);
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResourceJNI"
                "/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



JNIEXPORT jobject JNICALL
Java_org_tiian_lixa_xta_Transaction_getXidJNI
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , FIND_CLASS_ERROR
                     , GET_METHOD_ID_ERROR
                     , NEW_OBJECT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    xta_transaction_t *tx = NULL;
    xta_xid_t *xid = NULL;
    jobject jxid = NULL;
    jclass class = NULL;
    jmethodID constructor = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getXidJNI\n"));
    TRY {        
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT1);
        
        /* create a new native C XID object */
        if (NULL == (xid = xta_xid_new_from_XID(
                         xta_xid_get_xa_xid(
                             xta_transaction_get_xid(tx)))))
            THROW(NULL_OBJECT2);
        /* retrieve Java XtaXid class */
        if (NULL == (class = (*env)->FindClass(
                         env, "org/tiian/lixa/xta/XtaXid")) ||
            (*env)->ExceptionCheck(env))
            THROW(FIND_CLASS_ERROR);
        /* retrieve XtaXid constructor */
        if (NULL == (constructor = (*env)->GetMethodID(
                         env, class, "<init>", "()V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR);
        /* create a new XtaXid object */
        if (NULL == (jxid = (*env)->NewObject(env, class, constructor)) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_OBJECT_ERROR);
        /* populate Java object with tx native C Transaction */
        Java_org_tiian_lixa_xta_XtaXid_newJNI(env, jxid, xid);

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
        
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getXidJNI/"
                "excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    /* return the Java object */
    return jxid;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    start
 * Signature: (Z)V
 */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_Transaction_startJNI
(JNIEnv *env, jobject this_obj, jboolean multiple_branches,
 jboolean already_opened)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_OPEN_ERROR
                     , TRANSACTION_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_startJNI\n"));
    TRY {
        xta_transaction_t * tx = NULL;
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* call open if necessary */
        if (!already_opened &&
            LIXA_RC_OK != (ret_cod = xta_transaction_open(tx)))
            THROW(TRANSACTION_OPEN_ERROR);
        /* call native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_start(
                               tx, (int)multiple_branches)))
            THROW(TRANSACTION_START_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TRANSACTION_OPEN_ERROR:
            case TRANSACTION_START_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* generate a new Java exception if not already thrown */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_startJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    commitJNI
 * Signature: (Z)I
 */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_Transaction_commitJNI
(JNIEnv *env, jobject this_obj, jboolean non_blocking)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_COMMIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_commitJNI\n"));
    TRY {
        xta_transaction_t * tx = NULL;
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* call native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_commit(
                               tx, (int)non_blocking)))
            THROW(TRANSACTION_COMMIT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TRANSACTION_COMMIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_commitJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
