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
import javax.transaction.xa.Xid;



/**
 * The Config class contains the configuration associated to a Transaction
 * object (composition pattern).
 * This class does not have a public constructor because it can be created
 * only from a {@link Transaction} object
 */
public class Config {
    static {
        org.tiian.lixa.xta.Xta.init();
    }
    /**
     * This is the opaque wrapper of a xta_config_t pointer used
     * by the native library; this pointer does not represent an object, but
     * only a reference
     */
    private ByteBuffer NativePointer;
    /*
     * Allocate a C native object @@@
     */
    private native void newJNI() throws XtaException;
    /**
     * This class does not have a public constructor because it's factory is
     * This constructor is necessary only to allocate the Java object,
     * but the content is populated by a JNI function called by the factory.
     */
    Config() {
        return;
    }
    /**
     * Release the C native object when finalization is executed
     */
    protected void finalize() {
        NativePointer = null;
    }
    /**
     * Obtain the format identifier part of the XID.
     */
    public native int getConnectionTimeout();
    /**
     * Obtain the global transaction identifier part of XID as an array of
     * bytes.
     */
    public native void setConnectionTimeout(int value);
}
