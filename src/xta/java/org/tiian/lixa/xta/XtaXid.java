/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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



/**
 * The XtaXid class implements the Xid interface provided by the JTA standard.
 * As explained in JTA documentation, it's a Java mapping of the X/Open
 * transaction identifier XID structure and it specifies three accessor
 * methods to retrieve a global transaction's format ID, global transaction ID,
 * and branch qualifier.
 * This class does not have a public constructor because its factory is
 * {@link Transaction#getXid getXid} and there would be no usage of a Xid
 * object that has been created outside a Transaction context.
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
    /*
     * Allocate a C native object xta_xid_t
     */
    private native void newJNI() throws XtaException;
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
    /**
     * Obtain the transaction branch identifier part of XID as an array of
     * bytes.
     */
    public native byte[] getBranchQualifier();
    /**
     * Obtain the format identifier part of the XID.
     */
    public native int getFormatId();
    /**
     * Obtain the global transaction identifier part of XID as an array of
     * bytes.
     */
    public native byte[] getGlobalTransactionId();
    /**
     * Convert the transaction ID to a string
     */
    public native String toString();
}
