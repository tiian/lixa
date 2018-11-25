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



/**
 * TransactionManager class provides the factory to create
 * {@link Transaction Transaction} objects. During its initialization, LIXA
 * configurations for the underlying C libraries are loaded.
 */
public class TransactionManager {
    static {
        org.tiian.lixa.xta.Xta.init();
    }
    /**
     * This is the opaque wrapper of a xta_transaction_manager_t object used
     * by the native library
     */
    private ByteBuffer NativeObject;
    /**
     * Verifies that the current object is not corrupted
     */
    private void nullCheck() throws XtaException {
        if (null == NativeObject)
            throw new XtaException(ErrorCodes.LIXA_RC_OBJ_CORRUPTED);
    }
    /*
     * Create a new native xta_transaction_manager_t object and set
     * NativeObject
     * Called by class constructor
     */
    private native void newJNI() throws XtaException;
    /**
     * Create a new object calling the native interface
     * @throws XtaException if the underlying native C function returns
     * an error condition
     */
    public TransactionManager() throws XtaException {
        newJNI();
    }
    /*
     * Delete the native xta_transaction_manager_t object.
     * Called by finalize method
     */
    private native void deleteJNI();
    /**
     * Release the C native object when finalization is executed
     */
    protected void finalize() {
        if (null != NativeObject) {
            deleteJNI();
            NativeObject = null;
        }
    }
    /**
     * Create a new Transaction object associated with the current Transaction
     * Manager. It calls the native C interface and it's the factory for
     * {@link Transaction Transacton} objects.
     * @throws XtaException if the underlying native C function returns
     * an error condition
     */
    public native Transaction createTransaction() throws XtaException;
}
