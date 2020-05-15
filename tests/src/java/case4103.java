/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */



// import XTA package from LIXA project
import org.tiian.lixa.xta.*;
// import Java XA
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
// import Java io
import java.io.*;



// XTA: suspend/resume test cases with LIXA native resource manager (monkey)



public class case4103 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 4) {
            System.err.println("This program requires at least 4 options");
            System.exit(1);
        }
        // 0: INITIAL
        // 1: INTERMEDIATE
        // 2: FINAL
        // 3: NO_PHASE
        int phase = Integer.parseInt(args[0]);
        boolean commit = Integer.parseInt(args[1]) > 0 ? true : false;
        int testRc = Integer.parseInt(args[2]);
        String filename = args[3];
        // Lixa XA Resources
        LixaMonkeyXAResource xar1 = null;
        LixaMonkeyXAResource xar2 = null;
        // XTA Transaction Manager
        TransactionManager tm = null;
        // XTA Transaction
        Transaction tx = null;
        
        try {
            BufferedWriter output = null;
            BufferedReader input = null;
            // check phase
            switch (phase) {
                case 0: // INITIAL
                    System.err.println("phase=" + phase + " (INITIAL)");
                    output = new BufferedWriter(new FileWriter(filename));
                    break;
                case 1: // INTERMEDIATE
                    System.err.println("phase=" + phase + " (INTERMEDIATE)");
                    input = new BufferedReader(new FileReader(filename));
                    break;
                case 2: // FINAL
                    System.err.println("phase=" + phase + " (FINAL)");
                    input = new BufferedReader(new FileReader(filename));
                    break;
                case 3: // NO_PHASE
                    System.err.println("phase=" + phase + " (NO_PHASE)");
                    break;
                default:
                    System.err.println("phase=" + phase + " UNKNOWN!");
            } // switch (phase)

            // Create two LixaMonkey RMs
            xar1 = new LixaMonkeyXAResource("monkey1s.conf");
            xar2 = new LixaMonkeyXAResource("monkey2s.conf");
            
            // create a Transaction Manager
            tm = new TransactionManager();
            // create a new Transation
            tx = tm.createTransaction();
            // enlist LixaMonkey resources to Transaction
            tx.enlistResource(xar1, "LixaMonkey", "First monkey RM");
            tx.enlistResource(xar2, "LixaMonkey", "Second monkey RM");
            
            if (phase == 0 || phase == 3) { // INITIAL || NO_PHASE
                // start transaction
                tx.start();
                // Retrieve the Transaction ID (XID) associated to the
                // transaction that has been created in the previous step
                String xidString = tx.getXid().toString();
                System.err.println("xta XID is '" + xidString + "'");
                if (phase == 0) { // INITIAL
                    output.write(xidString);
                    output.close();
                } // INITIAL
            } else { // INTERMEDIATE || FINAL
                String xidString = input.readLine();
                System.err.println("xta XID is '" + xidString + "'");
                input.close();
                try {
                    tx.resume(xidString);
                } catch (XtaException e) {
                    e.printStackTrace();
                    System.exit(1);
                }
            }
            
            if (phase == 0 || phase == 1) { // INITIAL || INTERMEDIATE
                try {
                    tx.suspend();
                } catch (XtaException e) {
                    if (e.getReturnCode() != testRc)
                        throw e;
                }
            } // INITIAL || INTERMEDIATE
            
            if (phase == 2 || phase == 3) { // FINAL || NO_PHASE
                try {
                    if (commit)
                        tx.commit();
                    else
                        tx.rollback();
                } catch (XtaException e) {
                    if (e.getReturnCode() != testRc)
                        throw e;
                    System.err.println("commit/rollback returned " +
                                       e.getReturnCode() + " as expected");
                }
            } // FINAL || NO_PHASE
        } catch (XtaException e) {
            if (e.getReturnCode() != testRc) {
                System.err.println("XtaException: LIXA ReturnCode=" +
                                   e.getReturnCode() + " ('" +
                                   e.getMessage() + "')");
                e.printStackTrace();
                System.exit(1);
            }
        } catch (XAException e) {
            e.printStackTrace();
            System.exit(1);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        } finally {
            // Destroy TransactionManager object and close the connection
            // with the LIXA state server
            tm.delete();
        }
    }
}
