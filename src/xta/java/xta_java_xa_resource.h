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
#ifndef XTA_JAVA_XA_RESOURCE_H
# define XTA_JAVA_XA_RESOURCE_H



/* Java Native Interface */
#include <jni.h>
/* XTA includes */
#include "xta_acquired_xa_resource.h"



/**
 * XTA Java XA Resource data type
 */
typedef struct {
    union {
        xta_xa_resource_t           xa_resource;
        xta_acquired_xa_resource_t  acquired_xa_resource;
    };
    /**
     * Java Virtual Machine object
     */
    JavaVM                         *java_vm;
    /**
     * Java JNI version
     */
    jint                            java_jni_version;
    /**
     * Java XA Resource Global Reference
     */
    jobject                         java_object;
    /**
     * Java Xid Global Reference
     * Unfortunately it must be cached because some Resource Managers, for
     * example pgjdbc (PostgreSQL JDBC) check the Xid object, not the content
     * of the Xid object in the calls that follow "start"; if XTA creates a new
     * Xid object on the fly, such Resource Managers complains it's a "new one"
     */
    jobject                         java_xid;
    /**
     * This XTA Xid native object is necessary as the underlying object for the
     * above Java Xid object
     */
    xta_xid_t                      *xta_xid;
    /**
     * Java "start" method reference
     */
    jmethodID                       java_method_start;
    /**
     * Java "end" method reference
     */
    jmethodID                       java_method_end;
    /**
     * Java "prepare" method reference
     */
    jmethodID                       java_method_prepare;
    /**
     * Java "commit" method reference
     */
    jmethodID                       java_method_commit;
    /**
     * Java "rollback" method reference
     */
    jmethodID                       java_method_rollback;
    /**
     * Java "forget" method reference
     */
    jmethodID                       java_method_forget;
} xta_java_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new object to represent a Java XA resource
     * @param[in] name : of the Resource Manager, for example "PostgreSQL"
     * @param[in] identifier : a string to distinguish a specific Resource
     *            Manger instance
     * @param[in] java_vm : Java Virtual Machine
     * @param[in] java_jni_version : JNI version
     * @param[in] java_object : reference to Java XAResource object
     * @param[in] start : Java XAResource method
     * @param[in] end : Java XAResource method
     * @param[in] prepare : Java XAResource method
     * @param[in] commit : Java XAResource method
     * @param[in] rollback : Java XAResource method
     * @param[in] forget : Java XAResource method
     * @return a new object or NULL in the event of error
     */
    xta_java_xa_resource_t *xta_java_xa_resource_new(
        const char *name, const char *identifier,
        JavaVM *java_vm, jint java_jni_version,
        jobject java_object, jmethodID start, jmethodID end, jmethodID prepare,
        jmethodID commit, jmethodID rollback, jmethodID forget);

    

    /**
     * Delete an object that represent a Java XA Resource
     * @param[in] xa_resource : Java XA Resource
     */
    void xta_java_xa_resource_delete(xta_java_xa_resource_t *xa_resource);
    
    

    /**
     * Initialize the propeties of a Java XA resource
     * @param[in,out] xa_resource : Java XA Resource object
     * @param[in] name : of the Resource Manager, for example "PostgreSQL"
     * @param[in] identifier : a string to distinguish a specific Resource
     *            Manger instance
     * @param[in] java_vm : Java Virtual Machine
     * @param[in] java_jni_version : JNI version
     * @param[in] java_object : reference to Java XAResource object
     * @param[in] start : Java XAResource method
     * @param[in] end : Java XAResource method
     * @param[in] prepare : Java XAResource method
     * @param[in] commit : Java XAResource method
     * @param[in] rollback : Java XAResource method
     * @param[in] forget : Java XAResource method
     * @return a reason code
     */
    int xta_java_xa_resource_init(xta_java_xa_resource_t *xa_resource,
                                  const char *name, const char *identifier,
                                  JavaVM *java_vm, jint java_jni_version,
                                  jobject java_object, jmethodID start,
                                  jmethodID end, jmethodID prepare,
                                  jmethodID commit, jmethodID rollback,
                                  jmethodID forget);



    /**
     * Clean the properties of a Java XA Resource; this function
     * must be called after @ref xta_java_xa_resource_init during
     * object deletion to avoid memory leaks
     * @param[in,out] xa_resource : Java XA Resource object
     */
    void xta_java_xa_resource_clean(xta_java_xa_resource_t *xa_resource);



    /**
     * Retrieve the return code returned by the XA Java function: it catches
     * the XAException that has been thrown by a class that implements the
     * XAResource interface
     * @param[in,out] env : JNI environment
     * @return a standard XA return code (XAException.errorCode in the event
     *         of exception)
     */
    int xta_java_xa_resource_rc(JNIEnv *env);

        

    /**
     * Close a Java XAResource; it's a dummy method because Java XAResource
     * does not need and does not provide such method
     * @param[in,out] context : dummy parameter
     * @param[in] xa_info : dummy parameter
     * @param[in] rmid : dummy parameter
     * @param[in] flags : dummy parameter
     * @return a XA return code (it must be XA_OK)
     */
    int xta_java_xa_close(xta_xa_resource_t *context, char *xa_info,
                          int rmid, long flags);



    /**
     * Commit work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] xid : transaction identifier, XA spec
     * @param[in] rmid : dummy parameter
     * @param[in] flags : @ref TMONEPHASE or @ref TMNOFLAGS
     * @return a XA return code
     */
    int xta_java_xa_commit(xta_xa_resource_t *context, const XID *xid,
                           int rmid, long flags);



    /**
     * Ends the work performed on behalf of a transaction branch. The
     * resource manager disassociates the XA resource from the transaction
     * branch specified and lets the transaction complete.
     * @param[in,out] context  : XTA resource context
     * @param[in] xid : transaction identifier, XA spec
     * @param[in] rmid : dummy parameter
     * @param[in] flags : @ref TMSUSPEND, @ref TMFAIL, @ref TMSUCCESS
     * @return a XA return code
     */
    int xta_java_xa_end(xta_xa_resource_t *context, const XID *xid,
                        int rmid, long flags);



    /**
     * Tells the resource manager to forget about a heuristically completed
     * transaction branch.
     * @param[in,out] context : XTA resource context
     * @param[in] xid : transaction identifier, XA spec
     * @param[in] rmid : dummy parameter
     * @param[in] flags : dummy parameter
     * @return a XA return code
     */
    int xta_java_xa_forget(xta_xa_resource_t *context, const XID *xid,
                           int rmid, long flags);



    /**
     * Open a Java XAResource; it's a dummy method because Java XAResource
     * does not need and does not provide such method
     * @param[in,out] context : dummy parameter
     * @param[in] xa_info : dummy parameter
     * @param[in] rmid : dummy parameter
     * @param[in] flags : dummy parameter
     * @return a XA return code (it must be XA_OK)
     */
    int xta_java_xa_open(xta_xa_resource_t *context, char *xa_info,
                         int rmid, long flags);



    /**
     * Ask the resource manager to prepare for a transaction commit of the
     * transaction specified in xid.
     * @param[in,out] context : XTA resource context
     * @param[in] xid : transaction identifier, XA spec
     * @param[in] rmid : dummy parameter
     * @param[in] flags : dummy parameter
     * @return a XA return code
     */
    int xta_java_xa_prepare(xta_xa_resource_t *context, const XID *xid,
                            int rmid, long flags);



    /**
     * Obtains a list of prepared transaction branches from a resource
     * manager. The transaction manager calls this method during recovery to
     * obtain the list of transaction branches that are currently in prepared
     * or heuristically completed states. 
     * @param[in,out] context : XTA resource context
     * @param[in] xids : an array into which the resource manager places XIDs
     *                   for list of transaction branches that are currently
     *                   in a prepared or heuristically completed state
     * @param[in] count : the maximum number of XIDs that fit into that array
     * @param[in] rmid : dummy parameter
     * @param[in] flags : @ref TMSTARTRSCAN, @ref TMENDRSCAN, @ref TMNOFLAGS
     * @return a XA return code
     */
    int xta_java_xa_recover(xta_xa_resource_t *context,
                            XID *xids, long count, int rmid, long flags);



    /**
     * Informs the resource manager to roll back work done on behalf of a
     * transaction branch. 
     * @param[in,out] context : XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid : dummy parameter
     * @param[in] flags : dummy parameter
     * @return a XA return code
     */
    int xta_java_xa_rollback(xta_xa_resource_t *context, const XID *xid,
                             int rmid, long flags);



    /**
     * Start work on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] xid : transaction identifier, XA spec
     * @param[in] rmid : dummy parameter
     * @param[in] flags : @ref TMNOFLAGS, @ref TMJOIN, @ref TMRESUME
     * @return a XA return code
     */
    int xta_java_xa_start(xta_xa_resource_t *context, const XID *xid,
                          int rmid, long flags);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* XTA_JAVA_XA_RESOURCE_H */
