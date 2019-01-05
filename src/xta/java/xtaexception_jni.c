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
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



/* This macro is necessary to avoid header files related to native resources:
   they are not used by XTA for Java */
#define XTA_FOR_JAVA
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_syslog.h"
/* JNI stuff includes */
#include "xtaexception_jni.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



void Java_org_tiian_lixa_xta_XtaException_throw(JNIEnv *env,
                                                jint throw_ret_cod)
{
    enum Exception { FIND_CLASS_ERROR
                     , GET_METHOD_ID_ERROR
                     , NEW_OBJECT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaException_throw: %d ('%s')\n",
                throw_ret_cod, lixa_strerror(throw_ret_cod)));
    TRY {
        jclass excp_class;
        jmethodID excp_constr;
        jobject excp_obj;
        
        if (NULL == (excp_class = (*env)->FindClass(
                         env, "org/tiian/lixa/xta/XtaException"))) {
            LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaException_throw"
                        "/FindClass returned NULL\n"));
            THROW(FIND_CLASS_ERROR);
        }
        if (NULL == (excp_constr = (*env)->GetMethodID(
                         env, excp_class, "<init>", "(I)V"))) {
            LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaException_throw"
                        "/GetMethodID returned NULL\n"));
            THROW(GET_METHOD_ID_ERROR);
        }
        if (NULL == (excp_obj = (*env)->NewObject(
                         env, excp_class, excp_constr, throw_ret_cod))) {
            LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaException_throw"
                        "/NewObject returned NULL\n"));
            THROW(NEW_OBJECT_ERROR);
        }
        /* throw the XTA Exception */
        (*env)->Throw(env, excp_obj);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
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
    if (LIXA_RC_OK != ret_cod) {
        /* at this point only a syslog message can be produced... */
        syslog(LOG_ERR, LIXA_SYSLOG_LXC032E, ret_cod, lixa_strerror(ret_cod));
    }
    LIXA_TRACE(("Java_org_tiian_lixa_xta_XtaException_throw/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}

