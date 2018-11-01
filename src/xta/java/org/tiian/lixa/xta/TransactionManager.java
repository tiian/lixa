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



/*
 * XTA Transaction Manager
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
    /**
     * Create a new native flom_handle_t object and set NativeHandler
     * Called by class constructor
     */
    private native int newJNI();
    /**
     * Create a new object calling the native interface
     * @throws XtaException if the underlying native C function returns
     * an error condition
     */
    public TransactionManager() throws XtaException {
        int ReturnCode = newJNI();
        if (ErrorCodes.LIXA_RC_OK != ReturnCode)
            throw new XtaException(ReturnCode);
    }
    /**
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
}
