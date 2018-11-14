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
package org.tiian.lixa.xta;



import java.nio.ByteBuffer;
import javax.transaction.xa.XAResource;



/*
 * XTA Transaction
 */
public class Transaction {
    static {
        org.tiian.lixa.xta.Xta.init();
    }
    /**
     * This is the opaque wrapper of a xta_transaction_manager_t object used
     * by the native library
     */
    private ByteBuffer NativeObject;
    /**
     * This is the opaque wrapper of a xta_transaction_manager_t object used
     * by the native library
     */
    private ByteBuffer NativeResources;
    /**
     * Verifies that the current object is not corrupted
     */
    private void nullCheck() throws XtaException {
        if (null == NativeObject)
            throw new XtaException(ErrorCodes.LIXA_RC_OBJ_CORRUPTED);
    }
    /*
     * Allocate a C list to collect the C native resources that will be
     * enlisted
     */
    private native void newJNI();
    /**
     * This class does not have a public constructor because it's factory is
     * TransactionManager.createTransaction() and there's no usage of a
     * Transaction object that has not been created by a Transaction Manager
     * factory. This constructor is necessary only to allocate the Java object,
     * but the content is populate by a JNI function called by the factory.
     */
    Transaction() {
        return;
    }
    /*
     * Delete the native xta_transaction_t object and the native C XA resources
     * Called by finalize method
     */
    private native void deleteJNI();
    /**
     * Release the C native object when finalization is executed
     */
    protected void finalize() {
        if (null != NativeResources) {
            deleteJNI();
            NativeObject = null;
            NativeResources = null;
        }
    }
    /*
     * Link a Java XAResource to a C xta_java_xa_resource_t
     */
    private native void enlistResourceJNI(XAResource xaRes);
    /**
     * Enlist the resource specified with the transaction associated with the
     * target Transaction object.
     */
    public boolean enlistResource(XAResource xaRes) throws XtaException {
        enlistResourceJNI(xaRes);
        return true;
    }
    private native void openJNI();
    /**
     * Open all the Resource Managers associated to enlisted resources; this is
     * a dummy method because XAResource does not require open()
     */
    public void open() {
        return;
    }
    /*
     * Create a native xta_xid_t from xta_transaction_t
     */
    private native XtaXid getXidJNI();
    /**
     * Return the XID associated to the current transaction. It calls the
     * C native interface and it's the factory that MUST be used to create
     * XtaXid objects.
     * @throws XtaException if the underlying native C functions returns an
     * error condition
     */
    public XtaXid getXid() throws XtaException {
        return getXidJNI();
    }
}
