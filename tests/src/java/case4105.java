/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
// import Java useful stuff
import java.io.*;
import java.util.concurrent.TimeUnit;



// XTA: suspend/resume test cases with LIXA native resource manager (monkey)



public class case4105 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 5) {
            System.err.println("This program requires at least 5 options");
            System.exit(1);
        }
        // 0: SUPERIOR
        // 1: SUBORDINATE
        // 2: NO_PHASE
        int phase = Integer.parseInt(args[0]);
        boolean commit = Integer.parseInt(args[1]) > 0 ? true : false;
        int testRc = Integer.parseInt(args[2]);
        String filename = args[3];
        boolean multipleBranches = Integer.parseInt(args[4]) > 0 ?
            true : false;
        // check if a second filename is provided
        String filename2 = null;
        if (args.length == 6)
            filename2 = args[5];
        // Lixa XA Resources
        LixaMonkeyXAResource xar1 = null;
        // XTA Transaction Manager
        TransactionManager tm = null;
        // XTA Transaction
        Transaction tx = null;
        
        try {
            BufferedWriter output = null;
            BufferedReader input = null;
            // check phase
            switch (phase) {
                case 0: // SUPERIOR
                    System.err.println("phase=" + phase + " (SUPERIOR)");
                    output = new BufferedWriter(new FileWriter(filename));
                    break;
                case 1: // SUBORDINATE
                    System.err.println("phase=" + phase + " (SUBORDINATE)");
                    input = new BufferedReader(new FileReader(filename));
                    if (filename2 != null)
                        output = new BufferedWriter(new FileWriter(filename2));
                    break;
                case 2: // NO_PHASE
                    System.err.println("phase=" + phase + " (NO_PHASE)");
                    break;
                default:
                    System.err.println("phase=" + phase + " UNKNOWN!");
                    System.exit(1);
            } // switch (phase)

            // Create a LixaMonkey RMs
            xar1 = new LixaMonkeyXAResource("monkey1s.conf");
            
            // create a Transaction Manager
            tm = new TransactionManager();
            // create a new Transation
            tx = tm.createTransaction();
            // enlist LixaMonkey resource to Transaction
            tx.enlistResource(xar1, "LixaMonkey", "First monkey RM");
            
            if (phase == 0 || phase == 2) { // SUPERIOR || NO_PHASE
                // start transaction
                tx.start(multipleBranches);
                // Retrieve the Transaction ID (XID) associated to the
                // transaction that has been created in the previous step
                String xidString = tx.getXid().toString();
                System.err.println("passing XID '" + xidString +
                                   "' to subordinate");
                if (phase == 0) { // SUPERIOR
                    // write to output the transaction that will be branched
                    output.write(xidString);
                    output.close();
                } // INITIAL
            } else { // SUBORDINATE
                String xidString = input.readLine();
                System.err.println("retrieved XID is '" + xidString + "'");
                input.close();
                try {
                    tx.branch(xidString);
                } catch (XtaException e) {
                    if (e.getReturnCode() != testRc) {
                        System.err.println("XtaException: LIXA ReturnCode=" +
                                           e.getReturnCode() + " ('" +
                                           e.getMessage() + "')");
                        e.printStackTrace();
                        System.exit(2);
                    } else
                        System.exit(0);
                }
                // write to xid_file2 the transaction that will be branched
                // again
                if (null != output) {
                    System.err.println("passing XID '" + xidString +
                                       "' to subordinate");
                    output.write(xidString);
                    output.close();
                }
            }

            // put a simple delay to allow the progress of the other branches
            TimeUnit.SECONDS.sleep(1);

            // commit or rollback the distributed transaction
            if (commit)
                tx.commit();
            else
                tx.rollback();
                    
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
