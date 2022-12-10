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
// import Java XA stuff
import javax.transaction.xa.XAException;



public class case4100 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 3) {
            System.err.println("This program requires at least 3 options");
            System.exit(1);
        }
        boolean commit = Integer.parseInt(args[0]) > 0 ? true : false;
        int testRc = Integer.parseInt(args[1]);
        int numberOfResources = Integer.parseInt(args[2]);

        if (numberOfResources < 1 || numberOfResources > 3) {
            System.err.println("Number of resources must be 1, 2 or 3");
            System.exit(1);
        }

        // XTA Transaction Manager
        TransactionManager tm = null;
        // XTA Transaction
        Transaction tx = null;
        // XA Resources
        LixaMonkeyXAResource xares1 = null;
        LixaMonkeyXAResource xares2 = null;
        LixaMonkeyXAResource xares3 = null;

        try {
            // Create a first LixaMonkery RM
            xares1 = new LixaMonkeyXAResource("monkey1s.conf");
            // create a second RM
            if (numberOfResources > 1)
                xares2 = new LixaMonkeyXAResource("monkey1s.conf");
            // create a third RM
            if (numberOfResources > 2)
                xares3 = new LixaMonkeyXAResource("monkey2s.conf");
            // create a Transaction Manager
            tm = new TransactionManager();
            // create a new Transation
            tx = tm.createTransaction();
            // enlist first resource
            tx.enlistResource(xares1, "LixaMonkey", "First monkey RM");
            // enlist second resource
            if (numberOfResources > 1)
                tx.enlistResource(xares2, "LixaMonkey", "Second monkey RM");
            // enlist third resource
            if (numberOfResources > 2)
                tx.enlistResource(xares3, "LixaMonkey", "Third monkey RM");
            try {
                // start transaction
                tx.start();
            } catch (XtaException e) {
                System.err.println("XtaException: LIXA ReturnCode=" +
                                   e.getReturnCode() + " ('" +
                                   e.getMessage() + "')");
                e.printStackTrace();
                System.exit(3);
            } 
            // commit or rollback
            if (commit) {
                try {
                    tx.commit();
                } catch (XtaException e) {
                    if (e.getReturnCode() != testRc) {
                        System.err.println("XtaException: LIXA ReturnCode=" +
                                           e.getReturnCode() + " ('" +
                                           e.getMessage() + "')");
                        e.printStackTrace();
                        System.exit(4);
                    }
                } 
                System.out.println("XTA commit performed");
            } else {
                tx.rollback();
                System.out.println("XTA rollback performed");
            }
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
