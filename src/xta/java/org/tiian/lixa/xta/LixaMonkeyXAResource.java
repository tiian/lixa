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
import javax.transaction.xa.Xid;



/**
 * This XA Resource is useful only for test purposes and has no useful
 * production usage
 */
public class LixaMonkeyXAResource implements XAResource {
    private int timeout;

    /**
     * @param fileName name of the file containing the monkey's behavior
     */
    public LixaMonkeyXAResource(String fileName) throws Exception {
        BufferedReader br = new BufferedReader(new FileReader(fileName));
        for (String line = br.readLine(); line != null; line = br.readLine()) {
            System.out.println(line);
        }
        br.close();
    }
    
    public void commit(Xid xid, boolean onePhase) {
        ;
    }
    public void end(Xid xid, int flags) {
        ;
    }
    public void forget(Xid xid) {
        ;
    }
    public int getTransactionTimeour() {
        return timeout;
    }
    public boolean isSameRM(XAResource xares) {
        return false;
    }
    public void prepare(Xid xid) {
        ;
    }
    public Xid[] recover(int flag) {
        return new Xid[0];
    }
    public void rollback(Xid xid) {
        ;
    }
    boolean setTransactionTimeout(int seconds) {
        timeout = seconds;
        return true;
    }
    public void start(Xid xid, int flags) {
        ;
    }
}
