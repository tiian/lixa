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



import java.io.*;
import java.util.*;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;
import javax.transaction.xa.Xid;



/**
 * This XA Resource is useful only for test purposes and has no useful
 * production usage
 */
public class LixaMonkeyXAResource implements XAResource {
    private int timeout;

    private enum StatusVerb { XA_OPEN, XA_CLOSE, XA_START, XA_END, XA_PREPARE,
                              XA_COMMIT, XA_ROLLBACK, XA_RECOVER, XA_FORGET,
                              XA_COMPLETE, DUMMY
    };
    private class Record {
        StatusVerb  verb;
        int         rc;
        public Record(StatusVerb verb, int rc) {
            this.verb = verb;
            this.rc = rc;
        }
    };
    // parsed values from config file
    private Vector records;

    /**
     * @param fileName name of the file containing the monkey's behavior
     */
    public LixaMonkeyXAResource(String fileName) throws Exception {
        records = new Vector();
        BufferedReader br = new BufferedReader(new FileReader(fileName));
        for (String line = br.readLine(); line != null; line = br.readLine()) {
            // ignore comments
            if (line.startsWith("#"))
                continue;
            // index of slash
            int index = line.indexOf("/");
            // extract value after slash
            int value = Integer.parseInt(line.substring(index+1));
            // extract verb before slash
            String verbString = line.substring(0, index);
            StatusVerb verb = StatusVerb.DUMMY;
            if (verbString.startsWith("xa_open"))
                verb = StatusVerb.XA_OPEN;
            else if (verbString.startsWith("xa_close"))
                verb = StatusVerb.XA_CLOSE;
            else if (verbString.startsWith("xa_start"))
                verb = StatusVerb.XA_START;
            else if (verbString.startsWith("xa_end"))
                verb = StatusVerb.XA_END;
            else if (verbString.startsWith("xa_rollback"))
                verb = StatusVerb.XA_ROLLBACK;
            else if (verbString.startsWith("xa_prepare"))
                verb = StatusVerb.XA_PREPARE;
            else if (verbString.startsWith("xa_commit"))
                verb = StatusVerb.XA_COMMIT;
            else if (verbString.startsWith("xa_recover"))
                verb = StatusVerb.XA_RECOVER;
            else if (verbString.startsWith("xa_forget"))
                verb = StatusVerb.XA_FORGET;
            else if (verbString.startsWith("xa_complete"))
                verb = StatusVerb.XA_COMPLETE;
            else throw new Exception("Invalid verb found in Monkey config");
            // create a new record            
            Record record = new Record(verb, value);
            // add to array
            records.addElement(record);
            System.out.println(verb);
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
    public int getTransactionTimeout() {
        return timeout;
    }
    public boolean isSameRM(XAResource xares) {
        return false;
    }
    public int prepare(Xid xid) {
        return XA_OK;
    }
    public Xid[] recover(int flag) {
        return new Xid[0];
    }
    public void rollback(Xid xid) {
        ;
    }
    public boolean setTransactionTimeout(int seconds) {
        timeout = seconds;
        return true;
    }
    public void start(Xid xid, int flags) {
        ;
    }
}
