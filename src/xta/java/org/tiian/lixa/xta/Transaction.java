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
import javax.transaction.xa.XAException;



/**
 * The Transaction class implements the methods necessary to manage XTA
 * transactions, like {@link Transaction#commit commit} and
 * {@link Transaction#rollback rollback}.
 * This class does not have a public constructor because it's factory is
 * {@link TransactionManager#createTransaction createTransaction} and there
 * would be no usage of a Transaction object that has not been created by a
 * Transaction Manager factory.
 * @author Christian Ferrari
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
     * Java XA Resources do not use the "open" method, but the LIXA underlying
     * logic requires it. This is a flag.
     */
    private boolean AlreadyOpened;
    /**
     * This is the opaque wrapper of a xta_transaction_manager_t object used
     * by the native library
     */
    private ByteBuffer NativeResources;
    /**
     * Verifies that the current object is not corrupted
     */
    /*
     * Allocate a C list to collect the C native resources that will be
     * enlisted
     */
    private native void newJNI() throws XtaException;
    /**
     * This class does not have a public constructor because it's factory is
     * TransactionManager.createTransaction() and there's no usage of a
     * Transaction object that has not been created by a Transaction Manager
     * factory. This constructor is necessary only to allocate the Java object,
     * but the content is populate by a JNI function called by the factory.
     */
    Transaction() {
        AlreadyOpened = false;
        return;
    }
    /*
     * Delete the native xta_transaction_t object and the native C XA resources
     * Called by finalize method
     */
    private native void deleteJNI(boolean AlreadyOpened);
    /**
     * Release the C native object when Java object finalization is executed
     */
    protected void finalize() {
        if (null != NativeResources) {
            deleteJNI(AlreadyOpened);
            AlreadyOpened = false;
            NativeObject = null;
            NativeResources = null;
        }
    }
    /**
     * Enlist the resource specified with the transaction associated with the
     * target Transaction object.
     * @param xaRes The XAResource object associated with the Resource Manager
     * @param name The name of the Resource Manager associated to the
     *        XAResource, for example "PostgreSQL"
     * @param identifier A string to distinguish the specific Resource Manager
     *        instance
     */
    public native void enlistResource(XAResource xaRes,
                                      String name,
                                      String identifier)
        throws XtaException;
    /*
     * Create a native xta_xid_t from xta_transaction_t
     */
    private native XtaXid getXidJNI() throws XtaException;
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
    /*
     * Native method wrapper for start
     */
    private native int startJNI(
        boolean multipleBranches, boolean alreadyOpened)
        throws XtaException, XAException;
    /**
     * Start a new Transaction
     * @param multipleBranches must be true only for transactions that will
     * span more applications and it will be followed by branch method
     */
    public void start(boolean multipleBranches)
        throws XtaException, XAException {
        startJNI(multipleBranches, AlreadyOpened);
        AlreadyOpened = true;
    }
    /**
     * Start a new Transaction
     */
    public void start() throws XtaException, XAException {
        start(false);
    }
    /*
     * Native method wrapper for commit
     */
    private native int commitJNI(boolean nonBlocking)
        throws XtaException, XAException;
    /**
     * Commit a Transacton
     * @param nonBlocking boolean value: true = xa_prepare will not block the
     *        caller, false = xa_prepare will block the caller; the option is
     *        used only for multiple branch transactions
     */
    public void commit(boolean nonBlocking)
        throws XtaException, XAException {
        commitJNI(nonBlocking);
    }
    public void commit() throws XtaException, XAException {
        commit(false);
    }
    /*
     * Native method wrapper for rollback
     */
    private native int rollbackJNI() throws XtaException, XAException;
    /**
     * Rollback a Transacton
     */
    public void rollback() throws XtaException, XAException {
        rollbackJNI();
    }
}
