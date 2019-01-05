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
import javax.transaction.xa.Xid;



/**
 * This XA Resource is useful only for test purposes and has no useful
 * production usage
 */
public class LixaDummyXAResource implements XAResource {
    private int timeout;

    /**
     * @param fileName name of the file containing the monkey's behavior
     */
    public LixaDummyXAResource(String fileName) { timeout = 0; return; }
    
    public void commit(Xid xid, boolean onePhase) { return; }
    public void end(Xid xid, int flags) { return; }
    public void forget(Xid xid) { return; }
    public int getTransactionTimeout() { return timeout; }
    public boolean isSameRM(XAResource xares) { return false; }
    public int prepare(Xid xid) { return XA_OK; }
    public Xid[] recover(int flag) { return new Xid[0]; }
    public void rollback(Xid xid) { return; }
    public boolean setTransactionTimeout(int seconds) {
        timeout = seconds;
        return true;
    }
    public void start(Xid xid, int flags) { return; }
}
