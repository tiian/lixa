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



/*
 * Test Config and its methods
 */



// import XTA package from LIXA project
import org.tiian.lixa.xta.*;
// import Java XA stuff
import javax.transaction.xa.XAException;



public class case4110 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 1) {
            System.err.println("This program requires at least 1 option");
            System.exit(1);
        }
        int connectionTimeout = Integer.parseInt(args[0]);

        // XTA Transaction Manager
        TransactionManager tm = null;
        // XTA Transaction
        Transaction tx = null;
        // XTA Config
        Config config = null;

        try {
            // create a Transaction Manager
            tm = new TransactionManager();
            // create a new Transation
            tx = tm.createTransaction();
            // get transaction's config
            config = tx.getConfig();
            System.out.println("Config.getConnectionTimeout() = " +
                               config.getConnectionTimeout());
            // check the value from config file or environment variable
            if (connectionTimeout != config.getConnectionTimeout()) {
                System.err.println("expected value = " + connectionTimeout +
                                   ", real value = " +
                                   config.getConnectionTimeout());
                System.exit(1);
            }
            // set a new value and check it
            config.setConnectionTimeout(1234);
            if (1234 != config.getConnectionTimeout()) {
                System.err.println("expected value = 1234, real value = " +
                                   config.getConnectionTimeout());
                System.exit(1);
            }
        } catch (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
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
