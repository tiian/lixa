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



