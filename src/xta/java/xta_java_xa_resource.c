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
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
/* XTA for Java resource */
#include "xta_java_xa_resource.h"
#include "xtaxid_jni.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



/**
 * Interface with XA function pointers
 */
const static struct xta_iface_s xta_java_iface = {
    "Java XAResource",
    TMNOFLAGS,
    0,
    xta_java_xa_open,
    xta_java_xa_close,
    xta_java_xa_start,
    xta_java_xa_end,
    xta_java_xa_rollback,
    xta_java_xa_prepare,
    xta_java_xa_commit,
    xta_java_xa_recover,
    xta_java_xa_forget
};



xta_java_xa_resource_t *xta_java_xa_resource_new(
    const char *name, const char *identifier,
    JavaVM *java_vm, jint java_jni_version,
    jobject java_object, jmethodID start, jmethodID end, jmethodID prepare,
    jmethodID commit, jmethodID rollback, jmethodID forget)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , XTA_JAVA_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_java_xa_resource_t *this = NULL;
    
    LIXA_TRACE(("xta_java_xa_resource_new\n"));
    TRY {
        /* allocate sufficient memory for the object */
        if (NULL == (this = (xta_java_xa_resource_t *)g_try_malloc0(
                         sizeof(xta_java_xa_resource_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* initialize "class" properties */
        if (LIXA_RC_OK != (ret_cod = xta_java_xa_resource_init(
                               this, name, identifier,
                               java_vm, java_jni_version,
                               java_object, start, end, prepare, commit,
                               rollback, forget)))
            THROW(XTA_JAVA_XA_RESOURCE_INIT_ERROR);
        /* reset java_xid, they will be set by start method */
        this->java_xid = NULL;
        this->xta_xid = NULL;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case XTA_JAVA_XA_RESOURCE_INIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_resource_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_java_xa_resource_delete(xta_java_xa_resource_t *xa_resource)
{
    enum Exception { GET_ENV_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    JNIEnv *env = NULL;
    
    LIXA_TRACE(("xta_java_xa_resource_delete\n"));
    TRY {
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(xa_resource->java_vm))->GetEnv(
                xa_resource->java_vm, (void **)&env,
                xa_resource->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* release the Global Reference acquired by _init() */
        (*env)->DeleteGlobalRef(env, xa_resource->java_object);
        /* release the Global Reference acquired by _start() */
        if (NULL != xa_resource->java_xid)
            (*env)->DeleteGlobalRef(env, xa_resource->java_xid);
        /* clean the XTA Xid cached by the resource */
        xta_xid_delete(xa_resource->xta_xid);
        xa_resource->xta_xid = NULL;
        /* clean the object before releasing */
        xta_java_xa_resource_clean(xa_resource);
        /* release memory allocated for the object */
        g_free(xa_resource);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_ENV_ERROR:
                ret_cod = LIXA_RC_GET_ENV_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_resource_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



int xta_java_xa_resource_init(xta_java_xa_resource_t *xa_resource,
                              const char *name, const char *identifier,
                              JavaVM *java_vm, jint java_jni_version,
                              jobject java_object, jmethodID start,
                              jmethodID end, jmethodID prepare,
                              jmethodID commit, jmethodID rollback,
                              jmethodID forget)
{
    enum Exception { NULL_OBJECT
                     , GET_ENV_ERROR
                     , NEW_GLOBAL_REF_ERROR
                     , XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    JNIEnv *env = NULL;
    jobject global_res = NULL;
    
    LIXA_TRACE(("xta_java_xa_resource_init\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT);
        /* initialize "base class" (xta_acquired_xa_resource_t) properties */
        if (LIXA_RC_OK != (ret_cod = xta_acquired_xa_resource_init(
                               (xta_acquired_xa_resource_t *)xa_resource,
                               &xta_java_iface, name, identifier)))
            THROW(XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR);
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(java_vm))->GetEnv(
                java_vm, (void **)&env, java_jni_version))
            THROW(GET_ENV_ERROR);
        /* create a global reference for the XA Resource */
        if (NULL == (global_res = (*env)->NewGlobalRef(env, java_object)))
            THROW(NEW_GLOBAL_REF_ERROR);
        /* set references to Java XAResource */
        xa_resource->java_vm = java_vm;
        xa_resource->java_jni_version = java_jni_version;
        xa_resource->java_object = global_res;
        xa_resource->java_method_start = start;
        xa_resource->java_method_end = end;
        xa_resource->java_method_prepare = prepare;
        xa_resource->java_method_commit = commit;
        xa_resource->java_method_rollback = rollback;
        xa_resource->java_method_forget = forget;
        LIXA_TRACE(("xta_java_xa_resource_init: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_start=%p, java_method_end=%p, "
                    "java_method_prepare=%p, java_method_commit=%p, "
                    "java_method_rollback=%p, java_method_forget=%p\n",
                    xa_resource->java_vm, xa_resource->java_jni_version,
                    xa_resource->java_object, xa_resource->java_method_start,
                    xa_resource->java_method_end,
                    xa_resource->java_method_prepare,
                    xa_resource->java_method_commit,
                    xa_resource->java_method_rollback,
                    xa_resource->java_method_forget));
        /* set resource interface */
        lixa_iface_set_xta(
            &xa_resource->xa_resource.act_rsrmgr_config.lixa_iface,
            &xta_java_iface, (xta_xa_resource_t *)xa_resource);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GET_ENV_ERROR:
                ret_cod = LIXA_RC_GET_ENV_ERROR;
                break;
            case NEW_GLOBAL_REF_ERROR:
                ret_cod = LIXA_RC_NEW_GLOBAL_REF_ERROR;
                break;
            case XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_resource_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void xta_java_xa_resource_clean(xta_java_xa_resource_t *xa_resource)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_java_xa_resource_clean\n"));
    TRY {
        /* clean Java XA function pointers */
        xa_resource->xa_resource.act_rsrmgr_config.lixa_iface.std = NULL;
        /* clean "base class" (xta_acquired_xa_resource_t) properties */
        xta_acquired_xa_resource_clean(
            (xta_acquired_xa_resource_t *)xa_resource);
        
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
    LIXA_TRACE(("xta_java_xa_resource_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



int xta_java_xa_resource_rc(JNIEnv *env)
{
    enum Exception { EXCEPTION_OCCURRED_ERROR
                     , FIND_CLASS_ERROR
                     , GET_FIELD_ID_ERROR
                     , GET_INT_FIELD_ERROR
                     , NONE } excp;
    int ret_cod = XAER_RMFAIL;
    
    LIXA_TRACE(("xta_java_xa_resource_rc\n"));
    TRY {
        jthrowable exception = NULL;
        jclass class = NULL;
        jfieldID fieldID = NULL;
        jint error_code = 0;
        /* check if an exception occurred */
        if ((*env)->ExceptionCheck(env)) {
            LIXA_TRACE(("xta_java_xa_resource_rc: caught Java exception\n"));
            if (NULL == (exception = (*env)->ExceptionOccurred(env)))
                THROW(EXCEPTION_OCCURRED_ERROR);
            if (NULL == (class = (*env)->FindClass(
                             env, "javax/transaction/xa/XAException")))
                THROW(FIND_CLASS_ERROR);
            if (NULL == (fieldID = (*env)->GetFieldID(
                             env, class, "errorCode", "I")))
                THROW(GET_FIELD_ID_ERROR);
            error_code = (*env)->GetIntField(env, exception, fieldID);
            LIXA_TRACE(("xta_java_xa_resource_rc: XAException.errorCode=%d\n",
                        error_code));
            (*env)->ExceptionDescribe(env);
            /* reset exception, it must not be propagated */
            // (*env)->ExceptionClear(env);
            ret_cod = error_code;
        } else
            ret_cod = XA_OK;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case EXCEPTION_OCCURRED_ERROR:
            case FIND_CLASS_ERROR:
            case GET_FIELD_ID_ERROR:
            case GET_INT_FIELD_ERROR:
                ret_cod = XAER_RMFAIL;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMFAIL;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_resource_rc/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_resource_set_xid(xta_java_xa_resource_t *res,
                              const XID *xid, JNIEnv *env)
{
    enum Exception { XTAXID_NEW
                     , NULL_OBJECT
                     , XTAXID_NEWJNI
                     , NEW_GLOBAL_REF_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    jobject jxid = NULL;
    
    LIXA_TRACE(("xta_java_resource_set_xid\n"));
    TRY {
        /* create a new Java XtaXid object */
        if (NULL == (jxid = Java_org_tiian_lixa_xta_XtaXid_new(env)) ||
            (*env)->ExceptionCheck(env))
            THROW(XTAXID_NEW);
        /* create an XTA Xid */
        if (NULL == (res->xta_xid = xta_xid_new_from_XID(xid)))
            THROW(NULL_OBJECT);
        /* populate Java object with tx native C Transaction */
        Java_org_tiian_lixa_xta_XtaXid_newJNI(env, jxid, res->xta_xid);
        if ((*env)->ExceptionCheck(env))
            THROW(XTAXID_NEWJNI);
        /* create a Global Reference for Xid */
        if (NULL == (res->java_xid = (*env)->NewGlobalRef(env, jxid)) ||
            (*env)->ExceptionCheck(env))
            THROW(NEW_GLOBAL_REF_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case XTAXID_NEW:
            case XTAXID_NEWJNI:
                break;
            case NEW_GLOBAL_REF_ERROR:
                ret_cod = LIXA_RC_NEW_GLOBAL_REF_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_resource_set_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_xa_open(xta_xa_resource_t *context, char *xa_info,
                      int rmid, long flags)
{
    LIXA_TRACE(("xta_java_xa_open: dummy method\n"));
    return XA_OK;
}



int xta_java_xa_close(xta_xa_resource_t *context, char *xa_info,
                      int rmid, long flags)
{
    LIXA_TRACE(("xta_java_xa_close: dummy method\n"));
    return XA_OK;
}



int xta_java_xa_start(xta_xa_resource_t *context,
                      const XID *xid, int rmid, long flags)
{
    enum Exception { XA_PROTOCOL_ERROR
                     , GET_ENV_ERROR
                     , SET_XID_ERROR
                     , NONE } excp;
    int ret_cod = XAER_RMERR;

    LIXA_TRACE(("xta_java_xa_start: rmid=%d, flags=%0x\n", rmid, flags));
    TRY {
        JNIEnv *env;
        xta_java_xa_resource_t *res = (xta_java_xa_resource_t *)context;
        LIXA_TRACE(("xta_java_xa_start: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_start=%p\n",
                    res->java_vm, res->java_jni_version,
                    res->java_object, res->java_method_start));
        /* check the cached xta_xid is not set */
        if (NULL != res->xta_xid)
            THROW(XA_PROTOCOL_ERROR);
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(res->java_vm))->GetEnv(
                res->java_vm, (void **)&env, res->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* populate XID in resource context */
        if (LIXA_RC_OK != xta_java_resource_set_xid(res, xid, env))
            THROW(SET_XID_ERROR);
        /* call Java start method */
        (*env)->CallVoidMethod(env, res->java_object, res->java_method_start,
                               res->java_xid, (jint)flags);
        /* retrieve the return code form the Java exception */
        ret_cod = xta_java_xa_resource_rc(env);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XA_PROTOCOL_ERROR:
                ret_cod = XAER_PROTO;
                break;
            case GET_ENV_ERROR:
                ret_cod = XAER_RMERR;
                break;
            case SET_XID_ERROR:
                ret_cod = XAER_INVAL;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_xa_end(xta_xa_resource_t *context, const XID *xid,
                    int rmid, long flags)
{
    enum Exception { XA_PROTOCOL_ERROR
                     , GET_ENV_ERROR
                     , XTAXID_NEW
                     , NULL_OBJECT
                     , XTAXID_NEWJNI
                     , NONE } excp;
    int ret_cod = XAER_RMERR;

    LIXA_TRACE(("xta_java_xa_end: rmid=%d, flags=%0x\n", rmid, flags));
    TRY {
        JNIEnv *env;
        xta_java_xa_resource_t *res = (xta_java_xa_resource_t *)context;
        LIXA_TRACE(("xta_java_xa_end: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_end=%p, java_xid=%p\n",
                    res->java_vm, res->java_jni_version,
                    res->java_object, res->java_method_end, res->java_xid));
        /* check Xid is OK */
        if (NULL == res->java_xid)
            THROW(XA_PROTOCOL_ERROR);
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(res->java_vm))->GetEnv(
                res->java_vm, (void **)&env, res->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* call Java start method */
        (*env)->CallVoidMethod(env, res->java_object, res->java_method_end,
                               res->java_xid, (jint)flags);
        /* retrieve the return code form the Java exception */
        ret_cod = xta_java_xa_resource_rc(env);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XA_PROTOCOL_ERROR:
                ret_cod = XAER_PROTO;
                break;
            case GET_ENV_ERROR:
            case XTAXID_NEW:
            case XTAXID_NEWJNI:
                break;
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_xa_rollback(xta_xa_resource_t *context, const XID *xid,
                         int rmid, long flags)
{
    enum Exception { GET_ENV_ERROR
                     , SET_XID_ERROR
                     , NONE } excp;
    int ret_cod = XAER_RMERR;

    LIXA_TRACE(("xta_java_xa_rollback: rmid=%d, flags=%0x\n", rmid, flags));
    TRY {
        JNIEnv *env;
        xta_java_xa_resource_t *res = (xta_java_xa_resource_t *)context;
        LIXA_TRACE(("xta_java_xa_rollback: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_rollback=%p, java_xid=%p\n",
                    res->java_vm, res->java_jni_version,
                    res->java_object, res->java_method_rollback,
                    res->java_xid));
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(res->java_vm))->GetEnv(
                res->java_vm, (void **)&env, res->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* check Xid is OK */
        if (NULL == res->java_xid) {
            /* populate XID in resource context */
            if (LIXA_RC_OK != xta_java_resource_set_xid(res, xid, env))
                THROW(SET_XID_ERROR);
        }
        /* call Java start method */
        (*env)->CallVoidMethod(env, res->java_object,
                               res->java_method_rollback,
                               res->java_xid);
        /* retrieve the return code form the Java exception */
        ret_cod = xta_java_xa_resource_rc(env);
        /* clean the XTA Xid cached by the resource, now useless! */
        xta_xid_delete(res->xta_xid);
        res->xta_xid = NULL;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_ENV_ERROR:
                ret_cod = XAER_RMERR;
                break;
            case SET_XID_ERROR:
                ret_cod = XAER_INVAL;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_xa_prepare(xta_xa_resource_t *context, const XID *xid,
                        int rmid, long flags)
{
    enum Exception { XA_PROTOCOL_ERROR
                     , GET_ENV_ERROR
                     , NONE } excp;
    int ret_cod = XAER_RMERR;

    jint xa_rc;
    
    LIXA_TRACE(("xta_java_xa_prepare: rmid=%d, flags=%0x\n", rmid, flags));
    TRY {
        JNIEnv *env;
        xta_java_xa_resource_t *res = (xta_java_xa_resource_t *)context;
        LIXA_TRACE(("xta_java_xa_prepare: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_prepare=%p, java_xid=%p\n",
                    res->java_vm, res->java_jni_version,
                    res->java_object, res->java_method_prepare,
                    res->java_xid));
        /* check Xid is OK */
        if (NULL == res->java_xid)
            THROW(XA_PROTOCOL_ERROR);
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(res->java_vm))->GetEnv(
                res->java_vm, (void **)&env, res->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* call Java start method */
        xa_rc = (*env)->CallIntMethod(env, res->java_object,
                                      res->java_method_prepare, res->java_xid);
        if ((*env)->ExceptionCheck(env))
            /* retrieve the return code from the Java exception */
            ret_cod = xta_java_xa_resource_rc(env);
        else
            /* if anything OK, return what returned by XAResource.prepare() */
            ret_cod = (int)xa_rc;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XA_PROTOCOL_ERROR:
                ret_cod = XAER_PROTO;
                break;
            case GET_ENV_ERROR:
                ret_cod = XAER_RMERR;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_xa_commit(xta_xa_resource_t *context, const XID *xid,
                       int rmid, long flags)
{
    enum Exception { GET_ENV_ERROR
                     , SET_XID_ERROR
                     , NONE } excp;
    int ret_cod = XAER_RMERR;

    jboolean one_phase;
    
    LIXA_TRACE(("xta_java_xa_commit: rmid=%d, flags=%0x\n", rmid, flags));
    TRY {
        JNIEnv *env;
        xta_java_xa_resource_t *res = (xta_java_xa_resource_t *)context;
        LIXA_TRACE(("xta_java_xa_commit: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_commit=%p, java_xid=%p\n",
                    res->java_vm, res->java_jni_version,
                    res->java_object, res->java_method_commit, res->java_xid));
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(res->java_vm))->GetEnv(
                res->java_vm, (void **)&env, res->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* check Xid is OK */
        if (NULL == res->java_xid) {
            /* populate XID in resource context */
            if (LIXA_RC_OK != xta_java_resource_set_xid(res, xid, env))
                THROW(SET_XID_ERROR);
        }
        /* set onePhase */
        if (TMONEPHASE & flags)
            one_phase = JNI_TRUE;
        else
            one_phase = JNI_FALSE;
        /* call Java start method */
        (*env)->CallVoidMethod(env, res->java_object, res->java_method_commit,
                               res->java_xid, one_phase);
        /* retrieve the return code form the Java exception */
        ret_cod = xta_java_xa_resource_rc(env);
        /* clean the XTA Xid cached by the resource, now useless! */
        xta_xid_delete(res->xta_xid);
        res->xta_xid = NULL;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_ENV_ERROR:
                ret_cod = XAER_RMERR;
                break;
            case SET_XID_ERROR:
                ret_cod = XAER_INVAL;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_java_xa_recover(xta_xa_resource_t *context,
                        XID *xids, long count, int rmid, long flags)
{
    LIXA_TRACE(("xta_java_xa_recover: dummy method\n"));
    return XA_OK;
}



int xta_java_xa_forget(xta_xa_resource_t *context, const XID *xid,
                       int rmid, long flags)
{
    enum Exception { GET_ENV_ERROR
                     , XTAXID_NEW
                     , NULL_OBJECT
                     , XTAXID_NEWJNI
                     , NONE } excp;
    int ret_cod = XAER_RMERR;

    jobject jxid = NULL;
    xta_xid_t *native_xid = NULL;
    
    LIXA_TRACE(("xta_java_xa_forget: rmid=%d, flags=%0x\n", rmid, flags));
    TRY {
        JNIEnv *env;
        xta_java_xa_resource_t *res = (xta_java_xa_resource_t *)context;
        LIXA_TRACE(("xta_java_xa_forget: java_vm=%p, "
                    "java_jni_version=%d, java_object=%p, "
                    "java_method_forget=%p\n",
                    res->java_vm, res->java_jni_version,
                    res->java_object, res->java_method_forget));
        /* retrieve Java environement for this thread */
        if (JNI_OK != (*(res->java_vm))->GetEnv(
                res->java_vm, (void **)&env, res->java_jni_version))
            THROW(GET_ENV_ERROR);
        /* create a new Java XtaXid object */
        if (NULL == (jxid = Java_org_tiian_lixa_xta_XtaXid_new(env)) ||
            (*env)->ExceptionCheck(env))
            THROW(XTAXID_NEW);
        /* create an XTA Xid */
        if (NULL == (native_xid = xta_xid_new_from_XID(xid)))
            THROW(NULL_OBJECT);
        /* populate Java object with tx native C Transaction */
        Java_org_tiian_lixa_xta_XtaXid_newJNI(env, jxid, native_xid);
        if ((*env)->ExceptionCheck(env))
            THROW(XTAXID_NEWJNI);
        /* call Java forget method */
        (*env)->CallVoidMethod(env, res->java_object, res->java_method_forget,
                               jxid, (jint)flags);
        /* retrieve the return code form the Java exception */
        ret_cod = xta_java_xa_resource_rc(env);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GET_ENV_ERROR:
            case XTAXID_NEW:
            case XTAXID_NEWJNI:
                break;
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
        /* clean-up native XID, now useless */
        if (NULL != native_xid) {
            xta_xid_delete(native_xid);
            native_xid = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_java_xa_forget/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


