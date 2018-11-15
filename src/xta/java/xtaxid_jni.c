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



/*
 * Create a reference to C native object and allocate a list for XA C resources
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_XtaXid_newJNI(
    JNIEnv *env, jobject this_obj, xta_xid_t *xid)
{
    enum Exception { GET_OBJECT_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , NEW_DIRECT_BYTE_BUFFER_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jclass this_class;
    jfieldID field_id;
    jobject byte_buffer;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_newJNI\n"));
    
    TRY {
        /* get a reference to this object's class */
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid_newJNI/"
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
                "JNI/Java_org_tiian_lixa_xta_XtaXid_newJNI/"
                "GetFieldID() returned NULL");
            THROW(GET_FIELD_ID_ERROR);
        }

        /* create ByteBuffer */
        if (NULL == (byte_buffer = (*env)->NewDirectByteBuffer(
                         env, (void *)xid, sizeof(xta_xid_t)))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid_newJNI/"
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_newJNI/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
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
        if (NULL == (this_class = (*env)->GetObjectClass(env, this_obj))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid_"
                "getNativeObject/GetObjectClass returned NULL");
            THROW(GET_OBJECT_CLASS_ERROR);
        }

        /* get the field identificator */
        if (NULL == (field_id = (*env)->GetFieldID(
                         env, this_class, "NativeObject",
                         "Ljava/nio/ByteBuffer;"))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid"
                "_getNativeObject/GetFieldID returned NULL");
            THROW(GET_FIELD_ID_ERROR);
        }
        /* get ByteBuffer reference */
        if (NULL == (byte_buffer = (*env)->GetObjectField(
                         env, this_obj, field_id))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid"
                "_getNativeObject/GetObjectField returned NULL");
            THROW(GET_OBJECT_FIELD_ERROR);
        }
        /* cast to xta_xid_t */
        if (NULL == (xid = (xta_xid_t *)
                     (*env)->GetDirectBufferAddress(
                         env, byte_buffer))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid_"
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
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_"
                "getNativeObject/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
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
        if (0 >= (len = xta_xid_get_bqual(xid, data))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid"
                "_getBranchQualifier/xta_xid_get_bqual returned NULL");
            THROW(OBJ_CORRUPTED);
        }
        ret = (*env)->NewByteArray(env, len);
        (*env)->SetByteArrayRegion(env, ret, 0, (jint)len, data);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getBranchQualifier/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
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
                     , OBJ_CORRUPTED
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
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getFormatId/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
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
        if (0 >= (len = xta_xid_get_gtrid(xid, data))) {
            jclass Exception = (*env)->FindClass(env, "java/lang/Exception");
            (*env)->ThrowNew(
                env, Exception,
                "JNI/Java_org_tiian_lixa_xta_XtaXid"
                "_getGlobalTransactionId/xta_xid_get_gtrid returned NULL");
            THROW(OBJ_CORRUPTED);
        }
        ret = (*env)->NewByteArray(env, len);
        (*env)->SetByteArrayRegion(env, ret, 0, (jint)len, data);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaXid_getGlobalTransactionId/"
                "excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret;
}
