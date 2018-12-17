/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
//import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;



public class case4101 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 3) {
            System.err.println("This program requires at least 3 options");
            System.exit(1);
        }
        boolean commit = Integer.parseInt(args[0]) > 0 ? true : false;
        int testRc = Integer.parseInt(args[1]);
        int numberOfResources = Integer.parseInt(args[2]);

        if (numberOfResources < 1 || numberOfResources > 2) {
            System.err.println("Number of resources must be 1 or 2");
            System.exit(1);
        }

        // XTA Transaction Manager
        TransactionManager tm = null;
        // XTA Transaction
        Transaction tx = null;
        // XA Resources
        LixaMonkeyXAResource xares1 = null;
        LixaMonkeyXAResource xares2 = null;

        try {
            // Create a first LixaMonkery RM
            xares1 = new LixaMonkeyXAResource("monkey1s.conf");
            // create a second RM
            if (numberOfResources == 2)
                xares2 = new LixaMonkeyXAResource("monkey1s.conf");
            // create a Transaction Manager
            tm = new TransactionManager();
            // create a new Transation
            tx = tm.createTransaction();
            // enlist first resource
            tx.enlistResource(xares1, "LixaMonkey", "First monkey RM");
            // enlist second resource
            if (numberOfResources == 2)
                tx.enlistResource(xares2, "LixaMonkey", "Second monkey RM");
            // start transaction
            tx.start();
            // commit or rollback
            if (commit) {
                tx.commit();
                System.out.println("XTA commit performed");
            } else {
                tx.rollback();
                System.out.println("XTA rollback performed");
            }
        } catch (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
            e.printStackTrace();
            System.exit(1);
        } catch (XAException e) {
            e.printStackTrace();
            System.exit(1);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}