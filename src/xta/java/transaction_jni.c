/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
/* other JNI function includes */
#include "xtaexception_jni.h"
#include "xtaxid_jni.h"
#include "config_jni.h"



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
        /* get the field identificator for nativeObject */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "nativeObject",
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
        /* get the field identificator for nativeResources */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "nativeResources",
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
    LIXA_TRACE_STACK();
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
    xta_transaction_t *tx = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_"
                "getNativeObject\n"));

    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj)) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_OBJECT_CLASS_ERROR);
        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "nativeObject",
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
    LIXA_TRACE_STACK();
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
                         env, this_class, "nativeResources",
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
    LIXA_TRACE_STACK();
    return array;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    enlistResource
 * Signature: (Ljavax/transaction/xa/XAResource;Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_enlistResource(
    JNIEnv *env, jobject this_object, jobject xa_resource, jstring name,
    jstring identifier)
{
    enum Exception { NULL_OBJECT0
                     , GET_JAVA_VM_ERROR
                     , GET_VERSION_ERROR
                     , GET_OBJECT_CLASS_ERROR
                     , GET_METHOD_ID_ERROR1
                     , GET_METHOD_ID_ERROR2
                     , GET_METHOD_ID_ERROR3
                     , GET_METHOD_ID_ERROR4
                     , GET_METHOD_ID_ERROR5
                     , GET_METHOD_ID_ERROR6
                     , GET_STRING_UTF_CHARS_ERROR1
                     , GET_STRING_UTF_CHARS_ERROR2
                     , NULL_OBJECT1
                     , GET_NATIVE_RESOURCE_ERROR
                     , NULL_OBJECT2
                     , TRANSACTION_ENLIST_RESOURCE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xta_java_xa_resource_t *xjxr = NULL;
    const char *resource_name = NULL;
    const char *resource_identifier = NULL;
    xta_transaction_t *tx = NULL;
    GPtrArray *array = NULL;
    JavaVM *java_vm = NULL;
    jint java_jni_version;
    jclass class = NULL;
    jmethodID start_method = NULL;
    jmethodID end_method = NULL;
    jmethodID prepare_method = NULL;
    jmethodID commit_method = NULL;
    jmethodID rollback_method = NULL;
    jmethodID forget_method = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResource\n"));
    TRY {
        /* check xa_resource */
        if (NULL == xa_resource)
            THROW(NULL_OBJECT0);
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
        /* retrieve end method */
        if (NULL == (end_method = (*env)->GetMethodID(
                         env, class, "end",
                         "(Ljavax/transaction/xa/Xid;I)V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR2);
        /* retrieve prepare method */
        if (NULL == (prepare_method = (*env)->GetMethodID(
                         env, class, "prepare",
                         "(Ljavax/transaction/xa/Xid;)I")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR3);
        /* retrieve commit method */
        if (NULL == (commit_method = (*env)->GetMethodID(
                         env, class, "commit",
                         "(Ljavax/transaction/xa/Xid;Z)V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR4);
        /* retrieve rollback method */
        if (NULL == (rollback_method = (*env)->GetMethodID(
                         env, class, "rollback",
                         "(Ljavax/transaction/xa/Xid;)V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR5);
        /* retrieve forget method */
        if (NULL == (forget_method = (*env)->GetMethodID(
                         env, class, "forget",
                         "(Ljavax/transaction/xa/Xid;)V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR6);
        /* convert resource name from Java to C */
        if (NULL == (resource_name = (*env)->GetStringUTFChars(
                         env, name, 0)))
            THROW(GET_STRING_UTF_CHARS_ERROR1);
        /* convert resource identifier from Java to C */
        if (NULL == (resource_identifier = (*env)->GetStringUTFChars(
                         env, identifier, 0)))
            THROW(GET_STRING_UTF_CHARS_ERROR2);
        /* create a new native XA Java resource */
        if (NULL == (xjxr = xta_java_xa_resource_new(
                         resource_name, resource_identifier,
                         java_vm, java_jni_version,
                         xa_resource, start_method, end_method, prepare_method,
                         commit_method, rollback_method, forget_method)))
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
            case NULL_OBJECT0:
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GET_METHOD_ID_ERROR1:
            case GET_METHOD_ID_ERROR2:
            case GET_METHOD_ID_ERROR3:
            case GET_METHOD_ID_ERROR4:
            case GET_METHOD_ID_ERROR5:
            case GET_METHOD_ID_ERROR6:
                ret_cod = LIXA_RC_GET_METHOD_ID_ERROR;
                break;
            case GET_STRING_UTF_CHARS_ERROR1:
            case GET_STRING_UTF_CHARS_ERROR2:
                ret_cod = LIXA_RC_GET_STRING_UTF_CHARS_ERROR;
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
    if (NULL != resource_name)
        (*env)->ReleaseStringUTFChars(env, name, resource_name);
    if (NULL != resource_identifier)
        (*env)->ReleaseStringUTFChars(env, identifier, resource_identifier);
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_enlistResource"
                "/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    getXidJNI
 * Signature: ()Lorg/tiian/lixa/xta/XtaXid;
 */
JNIEXPORT jobject JNICALL
Java_org_tiian_lixa_xta_Transaction_getXid
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
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getXid\n"));
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
        
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getXid/"
                "excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    /* return the Java object */
    return jxid;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    getConfig
 * Signature: ()Lorg/tiian/lixa/xta/Config;
 */
JNIEXPORT jobject JNICALL Java_org_tiian_lixa_xta_Transaction_getConfig
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
    xta_config_t *config = NULL;
    jobject jconfig = NULL;
    jclass class = NULL;
    jmethodID constructor = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getConfig\n"));
    TRY {
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT1);
        
        /* retrieve C pointer to xta_config_t */
        if (NULL == (config = xta_transaction_get_config(tx)))
            THROW(NULL_OBJECT2);
        
        /* retrieve Java Config class */
        if (NULL == (class = (*env)->FindClass(
                         env, "org/tiian/lixa/xta/Config")) ||
            (*env)->ExceptionCheck(env))
            THROW(FIND_CLASS_ERROR);
        
        /* retrieve Config constructor */
        if (NULL == (constructor = (*env)->GetMethodID(
                         env, class, "<init>", "()V")) ||
            (*env)->ExceptionCheck(env))
            THROW(GET_METHOD_ID_ERROR);
        
        /* create a new Config object */
        if (NULL == (jconfig = (*env)->NewObject(env, class, constructor)) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_OBJECT_ERROR);
        
        /* populate Java object with tx native C Transaction */
        Java_org_tiian_lixa_xta_Config_newJNI(env, jconfig, config);
        
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
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_getConfig/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return jconfig;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    start
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_start
(JNIEnv *env, jobject this_obj, jboolean multiple_branches)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_start\n"));
    TRY {
        xta_transaction_t *tx = NULL;
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    commit
 * Signature: (Z)I
 */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_Transaction_commit
(JNIEnv *env, jobject this_obj, jboolean non_blocking)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_COMMIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_commit\n"));
    TRY {
        xta_transaction_t * tx = NULL;
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* call native method */
        ret_cod = xta_transaction_commit(tx, (int)non_blocking);
        if (non_blocking && LIXA_RC_WOULD_BLOCK == ret_cod)
            ; // this is OK
        else if (LIXA_RC_OK != ret_cod)
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
        /* generate a new Java exception if not already thrown */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    rollback
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_rollback
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_ROLLBACK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_rollback\n"));
    TRY {
        xta_transaction_t * tx = NULL;
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* call native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_rollback(tx)))
            THROW(TRANSACTION_ROLLBACK_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TRANSACTION_ROLLBACK_ERROR:
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    resumeJNI
 * Signature: (Ljava/lang/String)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_resume
(JNIEnv *env, jobject this_obj, jstring xid_string)
{
    enum Exception { NULL_OBJECT
                     , GET_STRING_UTF_CHARS_ERROR
                     , TRANSACTION_RESUME_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    const char *xid = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_resume\n"));
    TRY {
        xta_transaction_t *tx = NULL;
        
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* convert resource name from Java to C */
        if (NULL == (xid = (*env)->GetStringUTFChars(
                         env, xid_string, 0)))
            THROW(GET_STRING_UTF_CHARS_ERROR);
        /* call resume native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_resume(
                               tx, xid, TMRESUME)))
            THROW(TRANSACTION_RESUME_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GET_STRING_UTF_CHARS_ERROR:
                ret_cod = LIXA_RC_GET_STRING_UTF_CHARS_ERROR;
                break;
            case TRANSACTION_RESUME_ERROR:
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
        /* recovery memory */
        if (NULL != xid)
            (*env)->ReleaseStringUTFChars(env, xid_string, xid);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_resume/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    suspend
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_suspend
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_SUSPEND_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_suspend\n"));
    TRY {
        xta_transaction_t * tx = NULL;
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* call suspend native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_suspend(tx, TMNOFLAGS)))
            THROW(TRANSACTION_SUSPEND_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TRANSACTION_SUSPEND_ERROR:
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_suspend/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    branchJNI
 * Signature: (Ljava/lang/String)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_branch
(JNIEnv *env, jobject this_obj, jstring xid_string)
{
    enum Exception { NULL_OBJECT
                     , GET_STRING_UTF_CHARS_ERROR
                     , TRANSACTION_BRANCH_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    const char *xid = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_branch\n"));
    TRY {
        xta_transaction_t *tx = NULL;
        
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* convert resource name from Java to C */
        if (NULL == (xid = (*env)->GetStringUTFChars(
                         env, xid_string, 0)))
            THROW(GET_STRING_UTF_CHARS_ERROR);
        /* call resume native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_branch(tx, xid)))
            THROW(TRANSACTION_BRANCH_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GET_STRING_UTF_CHARS_ERROR:
                ret_cod = LIXA_RC_GET_STRING_UTF_CHARS_ERROR;
                break;
            case TRANSACTION_BRANCH_ERROR:
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
        /* recovery memory */
        if (NULL != xid)
            (*env)->ReleaseStringUTFChars(env, xid_string, xid);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_branch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



/*
 * Class:     org_tiian_lixa_xta_Transaction
 * Method:    recover
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Transaction_recover
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , TRANSACTION_RECOVER_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_recover\n"));
    TRY {
        xta_transaction_t *tx = NULL;
        
        /* retrieve the current Transaction object */
        if (NULL == (tx = Java_org_tiian_lixa_xta_Transaction_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        /* call resume native method */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_recover(tx)))
            THROW(TRANSACTION_RECOVER_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TRANSACTION_RECOVER_ERROR:
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_Transaction_recover/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}
