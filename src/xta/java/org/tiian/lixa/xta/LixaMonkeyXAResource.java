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
    private int recordIndex;
    private enum StatusVerb { XA_START, XA_END, XA_PREPARE,
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
        recordIndex = 0;
        BufferedReader br = new BufferedReader(new FileReader(fileName));
        for (String line = br.readLine(); line != null; line = br.readLine()) {
            // ignore comments
            if (line.startsWith("#"))
                continue;
            // index of slash
            int index = line.indexOf("/");
            // extract value after slash
            int rc = Integer.parseInt(line.substring(index+1));
            // extract verb before slash
            String verbString = line.substring(0, index);
            StatusVerb verb = StatusVerb.DUMMY;
            if (verbString.startsWith("xa_open"))
                // ignore it, not supported in Java
                continue;
            else if (verbString.startsWith("xa_close"))
                // ignore it, not supported in Java
                continue;
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
            Record record = new Record(verb, rc);
            // add to array
            records.addElement(record);
            System.err.println(record.verb + "/" + record.rc);
        }
        br.close();
    }
    
    public void commit(Xid xid, boolean onePhase) throws XAException {
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_COMMIT != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK, throw an exception
        if (ErrorCodes.LIXA_RC_OK != record.rc)
            throw new XAException(record.rc);
    }
    
    public void end(Xid xid, int flags) throws XAException {
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_END != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK, throw an exception
        if (XA_OK != record.rc)
            throw new XAException(record.rc);
    }
    
    public void forget(Xid xid) throws XAException {  
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_FORGET != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK, throw an exception
        if (XA_OK != record.rc)
            throw new XAException(record.rc);
    }
    
    public int getTransactionTimeout() {
        return timeout;
    }
    
    public boolean isSameRM(XAResource xares) {
        return false;
    }
    
    public int prepare(Xid xid) throws XAException {
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_PREPARE != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK or XA_RDONLY, throw an exception
        if (XA_OK != record.rc && XA_RDONLY != record.rc)
            throw new XAException(record.rc);
        return record.rc;
    }
    
    public Xid[] recover(int flag) throws XAException {
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_RECOVER != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK, throw an exception
        if (XA_OK != record.rc)
            throw new XAException(record.rc);
        // return an empty array
        return new Xid[0];
    }
    
    public void rollback(Xid xid) throws XAException {
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_ROLLBACK != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK, throw an exception
        if (XA_OK != record.rc)
            throw new XAException(record.rc);
    }
    
    public boolean setTransactionTimeout(int seconds) {
        timeout = seconds;
        return true;
    }
    public void start(Xid xid, int flags) throws XAException {
        Record record = (Record)records.get(recordIndex++);
        // check the proper order of the XA verb
        if (StatusVerb.XA_START != record.verb)
            throw new XAException(XAException.XAER_PROTO);
        // if not XA_OK, throw an exception
        if (XA_OK != record.rc)
            throw new XAException(record.rc);
    }
}
