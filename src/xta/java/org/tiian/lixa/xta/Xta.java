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



/**
 * This class is used only to initialize the native C library; it's not public
 * and it's statically called by all the XTA classes that requires
 * environment set-up
 */
class Xta {
    static {
        System.loadLibrary("lixta_java");
    }
    static private boolean hasInitialized = false;
    /**
     * Initialize XTA environment at the C level
     */
    static private native void initJNI();
    /**
     * Call C level XTA environment initialization
     */
    static void init() {
        if (!hasInitialized) {
            hasInitialized = true;
            initJNI();
        }
    }
}
