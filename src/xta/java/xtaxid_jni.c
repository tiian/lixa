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
#include "xtaxid_jni.h"
#include "xtaexception_jni.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



/*
 * Create a reference to C native object
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_XtaXid_newJNI(
    JNIEnv *env, jobject this_obj, xta_xid_t *xid)
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
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_newJNI\n"));
    
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
            THROW(GET_FIELD_ID_ERROR);
        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)xid, sizeof(xta_xid_t))))
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
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_newJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}


/* This is an helper internal function, it's not seen by JNI */
jobject JNICALL Java_org_tiian_lixa_xta_XtaXid_new(JNIEnv *env)
{
    enum Exception { FIND_CLASS_ERROR
                     , GET_METHOD_ID_ERROR
                     , NEW_OBJECT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass class = NULL;
    jmethodID constructor = NULL;
    jobject jxid = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_new\n"));
    TRY {
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
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case FIND_CLASS_ERROR:
            case GET_METHOD_ID_ERROR:
            case NEW_OBJECT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return jxid;
}



/* This is an helper internal function, it's not seen by JNI */
xta_xid_t*
Java_org_tiian_lixa_xta_XtaXid_getNativeObject(JNIEnv *env, jobject this_obj)
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
    xta_xid_t *xid = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getNativeObject\n"));
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
        /* cast to xta_xid_t */
        if (NULL == (xid = (xta_xid_t *)
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_"
                "getNativeObject/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return xid;
}



/*
 * Class:     org_tiian_lixa_xta_XtaXid
 * Method:    getBranchQualifier
 * Signature: ()[B
 */
JNIEXPORT jbyteArray JNICALL Java_org_tiian_lixa_xta_XtaXid_getBranchQualifier
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , OBJ_CORRUPTED
                     , NEW_BYTE_ARRAY_ERROR
                     , SET_BYTE_ARRAY_REGION_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    jbyteArray ret = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getBranchQualifier\n"));
    TRY {
        jint len = 0;
        jbyte data[XIDDATASIZE];
        xta_xid_t *xid = NULL;
    
        /* retrieve the native xta_xid_t C object */
        if (NULL == (xid = Java_org_tiian_lixa_xta_XtaXid_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        if (0 >= (len = xta_xid_get_bqual(xid, (char *)data)))
            THROW(OBJ_CORRUPTED);
        ret = (*env)->NewByteArray(env, len);
        if ((*env)->ExceptionCheck(env))
            THROW(NEW_BYTE_ARRAY_ERROR);
        (*env)->SetByteArrayRegion(env, ret, 0, (jint)len, data);
        if ((*env)->ExceptionCheck(env))
            THROW(SET_BYTE_ARRAY_REGION_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NEW_BYTE_ARRAY_ERROR:
                ret_cod = LIXA_RC_NEW_BYTE_ARRAY_ERROR;
                break;
            case SET_BYTE_ARRAY_REGION_ERROR:
                ret_cod = LIXA_RC_SET_BYTE_ARRAY_REGION_ERROR;
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getBranchQualifier/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret;
}



/*
 * Class:     org_tiian_lixa_xta_XtaXid
 * Method:    getFormatId
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_XtaXid_getFormatId
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    jint ret = 0;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getFormatId\n"));
    TRY {
        xta_xid_t *xid = NULL;
    
        /* retrieve the native xta_xid_t C object */
        if (NULL == (xid = Java_org_tiian_lixa_xta_XtaXid_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        ret = (jint)xta_xid_get_formatID(xid);
                
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getFormatId/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret;
}



/*
 * Class:     org_tiian_lixa_xta_XtaXid
 * Method:    getGlobalTransactionId
 * Signature: ()[B
 */
JNIEXPORT jbyteArray JNICALL
Java_org_tiian_lixa_xta_XtaXid_getGlobalTransactionId
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT
                     , OBJ_CORRUPTED
                     , NEW_BYTE_ARRAY_ERROR
                     , SET_BYTE_ARRAY_REGION_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    jbyteArray ret = NULL;

    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getGlobalTransactionId\n"));
    TRY {
        jint len = 0;
        jbyte data[XIDDATASIZE];
        xta_xid_t *xid = NULL;
    
        /* retrieve the native xta_xid_t C object */
        if (NULL == (xid = Java_org_tiian_lixa_xta_XtaXid_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT);
        if (0 >= (len = xta_xid_get_gtrid(xid, (char *)data)))
            THROW(OBJ_CORRUPTED);
        ret = (*env)->NewByteArray(env, len);
        if ((*env)->ExceptionCheck(env))
            THROW(NEW_BYTE_ARRAY_ERROR);
        (*env)->SetByteArrayRegion(env, ret, 0, (jint)len, data);
        if ((*env)->ExceptionCheck(env))
            THROW(SET_BYTE_ARRAY_REGION_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NEW_BYTE_ARRAY_ERROR:
                ret_cod = LIXA_RC_NEW_BYTE_ARRAY_ERROR;
                break;
            case SET_BYTE_ARRAY_REGION_ERROR:
                ret_cod = LIXA_RC_SET_BYTE_ARRAY_REGION_ERROR;
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getGlobalTransactionId/"
                "excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret;
}



/*
 * Class:     org_tiian_lixa_xta_XtaXid
 * Method:    toString
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_tiian_lixa_xta_XtaXid_toString
(JNIEnv *env, jobject this_obj)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , NEW_STRING_UTF_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    jstring xid_string = NULL;
    char *tmp_string = NULL;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_toString\n"));
    TRY {
        xta_xid_t *xid = NULL;
        
        /* retrieve the native xta_xid_t C object */
        if (NULL == (xid = Java_org_tiian_lixa_xta_XtaXid_getNativeObject(
                         env, this_obj)))
            THROW(NULL_OBJECT1);
        /* retrieve the XID as a string */
        if (NULL == (tmp_string = xta_xid_to_string(xid)))
            THROW(NULL_OBJECT2);
        /* create the java string object */
        if (NULL == (xid_string = (*env)->NewStringUTF(env, tmp_string)) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_STRING_UTF_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NEW_STRING_UTF_ERROR:
                ret_cod = LIXA_RC_NEW_STRING_UTF_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover memory */
        if (NULL != tmp_string) {
            free(tmp_string);
            tmp_string = NULL;
        }
        /* throw Java exception */
        if (LIXA_RC_OK != ret_cod && !(*env)->ExceptionCheck(env))
            Java_org_tiian_lixa_xta_XtaException_throw(env, ret_cod);
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_toString/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return xid_string;
}

