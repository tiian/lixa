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
import javax.transaction.xa.Xid;



/*
 * XTA XID
 */
/*
  see here for returning bytearray[] from JNI
  https://community.oracle.com/thread/1552704
*/
public class XtaXid implements Xid {
    static {
        org.tiian.lixa.xta.Xta.init();
    }
    /**
     * This is the opaque wrapper of a xta_xid_t object used
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
     * Allocate a C native object xta_xid_t
     */
    private native void newJNI();
    /**
     * This class does not have a public constructor because it's factory is
     * Transaction.getXid() and there's no usage of a
     * Xid object that has not been created by a Transaction factory.
     * This constructor is necessary only to allocate the Java object,
     * but the content is populated by a JNI function called by the factory.
     */
    XtaXid() {
        return;
    }
    /*
     * Delete the native xta_xid_t object and the native C XA resources
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
    public native byte[] getBranchQualifier();
    public native int getFormatId();
    public native byte[] getGlobalTransactionId();
}
