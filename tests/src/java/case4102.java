/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
// import SQL
import java.sql.Connection;
import java.sql.Statement;
// import Java XA
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.sql.XAConnection;
// import Oracle package for XA Data Source
import oracle.jdbc.xa.OracleXid;
import oracle.jdbc.xa.OracleXAException;
//import oracle.jdbc.*;
import oracle.jdbc.pool.*;
import oracle.jdbc.xa.client.*;
// import Java io
import java.io.*;



/*
 * EXIT CODES:
 *  0: OK
 *  1: generic error
 *  2: Transaction.resume() error
 *  3: Transaction.commit() error
 */



public class case4102 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 5) {
            System.err.println("This program requires at least 6 options");
            System.exit(1);
        }
        // 0: INITIAL
        // 1: INTERMEDIATE
        // 2: FINAL
        // 3: NO_PHASE
        int phase = Integer.parseInt(args[0]);
        boolean insert = Integer.parseInt(args[1]) > 0 ? true : false;
        boolean commit = Integer.parseInt(args[2]) > 0 ? true : false;
        int stmtNum = Integer.parseInt(args[3]);
        int testRc = Integer.parseInt(args[4]);
        String filename = args[5];
        // variable for SQL statement to execute
        String sqlStmtInsert = null;
        String sqlStmtDelete = null;
        String sqlStmt;
        // XA Resource for Oracle
        OracleXADataSource xads1 = null;
        XAConnection xac1 = null;
        XAResource xar1 = null;
        Connection conn1 = null;
        Statement stmt1 = null;
        // Lixa XA Resource
        LixaMonkeyXAResource xar2 = null;

        switch (stmtNum) {
            case 0:
                sqlStmtInsert = "INSERT INTO authors (ID, LAST_NAME, " +
                    "FIRST_NAME) VALUES (1886, 'Mallory', 'George')";
                sqlStmtDelete = "DELETE FROM authors WHERE id=1886";
                break;
            case 1:
                sqlStmtInsert = "INSERT INTO authors VALUES(1921, " +
                    "'Rigoni Stern', 'Mario')";
                sqlStmtDelete = "DELETE FROM authors WHERE id=1921";
                break;
            case 2:
                sqlStmtInsert = "INSERT INTO authors VALUES(1919, 'Levi', " +
                    "'Primo')";
                sqlStmtDelete = "DELETE FROM authors WHERE id=1919";
                break;
            default:
                System.err.println("Statenemt number " + stmtNum + " is not " +
                                   "valid!");
                System.exit(1);
                break;
        } // switch (stmtNum)
        
        // Prepare SQL statements in accordance with "insert" command line
        // parameter
        if (insert) // SQL INSERT
            sqlStmt = sqlStmtInsert;
        else        // SQL DELETE
            sqlStmt = sqlStmtDelete;
        
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

            //
            // A bit of boilerplate for Oracle JDBC & XA
            //
            // 1. create an XA Data Source
            xads1 = new OracleXADataSource();
            // 2. set connection URL (JDBC thin driver), user and password
            xads1.setURL("jdbc:oracle:thin:@" +
                         "(DESCRIPTION=" +
                         "(ADDRESS=(PROTOCOL=tcp)" +
                         "(HOST=centos7-oracle12.brenta.org)(PORT=1521))" +
                         "(CONNECT_DATA=" +
                         "(SERVICE_NAME=orcl.brenta.org)))");
            xads1.setUser("hr");
            xads1.setPassword("hr");
            // 3. get an XA Connection from the XA Data Source
            xac1 = xads1.getXAConnection();
            // 4. get an XA Resource from the XA Connection
            xar1 = xac1.getXAResource();
            // 5. get an SQL Connection from the XA Connection
            conn1 = xac1.getConnection();
            
            // XTA Transaction Manager
            TransactionManager tm = null;
            // XTA Transaction
            Transaction tx = null;

            // Create a LixaMonkey RM
            xar2 = new LixaMonkeyXAResource("monkey1s.conf");
            
            // create a Transaction Manager
            tm = new TransactionManager();
            // create a new Transation
            tx = tm.createTransaction();
            // enlist Oracle resource to transaction
            tx.enlistResource(xar1, "Oracle DB",
                              "orcl.brenta.org/hr/hr");
            // enlist LixaMonkey resource to Transaction
            tx.enlistResource(xar2, "LixaMonkey", "First monkey RM");
            
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
                    System.exit(2);
                }
            }
            // create a Statement object
            stmt1 = conn1.createStatement();
            // Execute the statement
            stmt1.executeUpdate(sqlStmt);
            // close the statement
            stmt1.close();
            System.out.println("JDBC statement >" + sqlStmt + "< completed");
            
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
        }
    }
}
