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



/*
 * XTA Exception
 */
public class XtaException extends Exception {
    static {
        org.tiian.lixa.xta.Xta.init();
    }
    /**
     * The return code of the native C function that returned an error
     * condition to the JNI wrapper method
     */
    private int ReturnCode;
    /**
     * Build a new XTA exception object
     * @param returnCode is the value returned by the native C function
     */
    public XtaException(int returnCode) {
        super(ErrorCodes.getText(returnCode));
        ReturnCode = returnCode;
    }
    /**
     * Get the return code of the native C function that returned an error
     * condition to the JNI wrapper method
     * @return the return code associated to the exception
     */
    public int getReturnCode() { return ReturnCode; }
}
