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
package org.tiian.lixa.xta;



import java.nio.ByteBuffer;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;



/**
 * The Transaction class implements the methods necessary to manage XTA
 * transactions, like {@link Transaction#commit commit} and
 * {@link Transaction#rollback rollback}.
 * This class does not have a public constructor because its factory is
 * {@link TransactionManager#createTransaction createTransaction} and there
 * would be no usage of a Transaction object that has not been created by a
 * Transaction Manager factory.
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
        return;
    }
    /**
     * Release the C native object when Java object finalization is executed
     */
    protected void finalize() {
        if (null != NativeResources) {
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
     *        instance. Use distinct identifier for different Resource Manager
     *        instances, schema owners and so on: uniqueness is essential to
     *        avoid undesired and/or wrong automatic recoveries
     */
    public native void enlistResource(XAResource xaRes,
                                      String name,
                                      String identifier)
        throws XtaException;
    /**
     * Return the XID associated to the current transaction. It calls the
     * C native interface and it's the factory that MUST be used to create
     * XtaXid objects.
     * @throws XtaException if the underlying native C functions returns an
     * error condition
     */
    public native XtaXid getXid() throws XtaException;
    /**
     * Start a new 2 phase commit transaction: this version of start method
     * is necessary only if the transaction will be branched by other
     * applications by mean of calling branch method. For single applications,
     * use the version without parameters.
     * @param multipleBranches must be true only for transactions that will
     * span more applications and it will be followed by a call to branch
     * method
     */
    public native void start(boolean multipleBranches)
        throws XtaException, XAException;
    /**
     * Start a new 2 phase commit transaction: this version of start method can
     * be used only if the transaction is not branched by 2 or more
     * applications
     */
    public void start() throws XtaException, XAException {
        start(false);
    }
    /**
     * Commit the current transaction; this version of commit method is
     * used to implement the Multiple Applications Concurrent Branches Pseudo
     * Synchronous pattern. For all the other use cases, the method without
     * non Blocking argument is suggested.
     * @param nonBlocking boolean value: true = xa_prepare will not block the
     *        caller, false = xa_prepare will block the caller; the option is
     *        used only for multiple branch transactions
     * @see <a href="http://www.tiian.org/lixa/manuals/html/">LIXA Reference Guide / Developing Application Programs using XTA</a>
     */
    public native void commit(boolean nonBlocking)
        throws XtaException, XAException;
    /**
     * Commit the current transaction
     */
    public void commit() throws XtaException, XAException {
        commit(false);
    }
    /**
     * Rollback the current transaction
     */
    public native void rollback() throws XtaException, XAException;
    /**
     * Resume the transaction represented by xidString in this transaction
     * object; the transaction has been previously suspended with
     * {@link Transaction#suspend suspend}
     * @param xidString serialized identifier of the transaction that must
     *        be resumed
     */
    public native void resume(String xidString)
        throws XtaException, XAException;
    /**
     * Suspend the transaction represented by this transaction object; the
     * transaction can be resumed with
     * {@link Transaction#resume resume} at a later time 
     */
    public native void suspend() throws XtaException, XAException;
    /**
     * Create a new branch of the transaction represented by xid in this
     * transaction object; the global transaction has been previously started
     * @param xidString serialized identifier of the transaction that must
     *        be resumed
     */
    public native void branch(String xidString)
        throws XtaException, XAException;    
    /**
     * Explicitly open and close all the enlisted resource to look for
     * recovery pending transaction in the LIXA state server. In normal
     * condition, this is not necessary, because the same happens when
     * {@link Transaction#start start}, {@link Transaction#resume resume} and
     * {@link Transaction#branch branch} are called
     */
    public native void recover() throws XtaException, XAException;
}
