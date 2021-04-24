/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
 * This program is an example implementation of the
 * "Mulitple Applications, Consecutive Calls" Pattern
 * as documented in LIXA manual:
 * http://www.tiian.org/lixa/manuals/html/index.html
 *
 * This program accepts exactly four parameters on the command line:
 * first parameter:  "commit", boolean value (if FALSE, "rollback")
 * second parameter: "insert", boolean value (if FALSE, "delete")
 * third parameter:  "superior", boolean value (if FALSE, "subordinate")
 * fourth parameter: "XIDfilename", a string for a filename
 *
 * Programming Style note:
 * the purpose of this small program is not to explain Java development
 * techniques or good style, but simply to show XTA for Java using the easiest
 * approach.
 */



// import XTA package from LIXA project
import org.tiian.lixa.xta.*;
// import SQL
import java.sql.Connection;
import java.sql.Statement;
// import Java XA
import javax.sql.XAConnection;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;
// import Oracle package for XA Data Source
import oracle.jdbc.xa.OracleXid;
import oracle.jdbc.xa.OracleXAException;
import oracle.jdbc.pool.*;
import oracle.jdbc.xa.client.*;
// import Java io
import java.io.*;



public class ExampleXtaMACC31 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 4) {
            System.err.println("This program requires three boolean " +
                               "parameters: 'commit', 'insert', 'superior' " +
                               "and one string: 'XIDfilename'");
            System.exit(1);
        }
        // First parameter: commit transaction
        boolean commit = Integer.parseInt(args[0]) > 0 ? true : false;
        // Second parameter: insert data in databases
        boolean insert = Integer.parseInt(args[1]) > 0 ? true : false;
        // Third parameter: superior application program
        boolean superior = Integer.parseInt(args[2]) > 0 ? true : false;
        // Fourth parameter: XID filename
        String xidFileName = args[3];
        // variable for SQL statement to execute
        String sqlStmt;

        // XTA Transaction Manager
        TransactionManager tm = null;
        // XTA Transaction
        Transaction tx;
        
        // XAResource for Oracle DBMS and some boilerplate objects related to
        // JDBC
        OracleXADataSource xads = null;
        XAConnection xac = null;
        XAResource xar = null;
        Connection conn = null; 
        Statement stmt = null;
        
        // Prepare SQL statements in accordance with "insert" command line
        // parameter
        if (insert) {
            // SQL INSERT
            if (superior)
                sqlStmt = "INSERT INTO authors VALUES(1930, " +
                    "'Bonatti', 'Walter')";
            else
                sqlStmt = "INSERT INTO authors VALUES(1948, " +
                    "'Casarotto', 'Renato')";
        } else {
            // SQL DELETE
            if (superior)
                sqlStmt = "DELETE FROM authors WHERE id=1930";
            else
                sqlStmt = "DELETE FROM authors WHERE id=1948";
        }
        
        try {
            //
            // A bit of boilerplate for Oracle JDBC & XA
            //
            // 1. create an XA Data Source
            xads = new OracleXADataSource();
            // 2. set connection URL (JDBC thin driver), user and password
            xads.setURL("jdbc:oracle:thin:@" +
                         "(DESCRIPTION=" +
                         "(ADDRESS=(PROTOCOL=tcp)" +
                         "(HOST=centos7-oracle12.brenta.org)(PORT=1521))" +
                         "(CONNECT_DATA=" +
                         "(SERVICE_NAME=orcl.brenta.org)))");
            xads.setUser("hr");
            xads.setPassword("hr");
            // 3. get an XA Connection from the XA Data Source
            xac = xads.getXAConnection();
            // 4. get an XA Resource from the XA Connection
            xar = xac.getXAResource();
            // 5. get an SQL Connection from the XA Connection
            conn = xac.getConnection();
            
            //
            // Create the XTA objects that are necessary to manage the
            // distributed transaction
            //
            // Create a mew XTA Transaction Manager
            tm = new TransactionManager();
            // Create a new XA global transaction using the Transaction
            // Manager as a factory
            tx = tm.createTransaction();
            // Enlist Oracle DBMS resource to transaction
            tx.enlistResource(xar, "Oracle", "hr/hr");
            /*
             * *NOTE:*
             * The following block of code contains the first key concept
             * of the "Multiple Applications, Consecutive Calls" Pattern:
             *
             * if the program is running with the role of "superior", it
             * starts a new global transaction
             *
             * if the program is running with the role of "subordinate",
             * it reads from file the XID (Transaction ID) saved by
             * another program executed with the role of "superior" and
             * then it resumes the same global transaction.
             */
            if (superior) {
                // Start a new XA global transaction with a single branch
                tx.start();
            } else {
                // read XID from file
                BufferedReader input = new BufferedReader(
                    new FileReader(xidFileName));
                String xidString = input.readLine();
                System.out.println("XID='" + xidString + "' has been " +
                                   "read from file '" + xidFileName +
                                   "'");
                input.close();
                // Resume the global transaction started by a superior
                // program
                tx.resume(xidString);
            }
            //
            // At this point, it's time to do something useful with the
            // Resource Manager
            //
            // Create and Execute a JDBC statement
            //
            System.out.println("Oracle, executing >" + sqlStmt + "<");
            // create a Statement object
            stmt = conn.createStatement();
            // Execute the statement
            stmt.executeUpdate(sqlStmt);
            // close the statement
            stmt.close();
            /*
             * *NOTE:*
             * The following block of code contains the second key
             * concept of the "Multiple Applications, Consecutive Calls"
             * Pattern:
             *
             * if the program is running with the role of "superior", it
             * suspends the global transaction and it passes the XID
             * (Transaction ID) in a file (fifo) for future reading
             * (subordinate application program will read it)
             *
             * if the program is running with the role of "subordinate",
             * it commits or rollback the global transaction.
             */
            if (superior) {
                // Suspend the XA global transaction
                tx.suspend();
                // Retrieve the Transaction ID (XID) associated to the
                // transaction that has been created in the previous step
                String xidString = tx.getXid().toString();
                // Write XID to the file and pass it from Superior
                // to Subordinate
                BufferedWriter output = new BufferedWriter(
                    new FileWriter(xidFileName));
                output.write(xidString);
                System.out.println("XID='" + xidString + "' has been " +
                                   "written to file '" + xidFileName +
                                   "'");
                output.close();
            } else {
                // commit or rollback
                if (commit)
                    tx.commit();
                else
                    tx.rollback();
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
        } finally {
            try {
                // Close Statement, SQL Connection and XA Connection for
                // PostgreSQL
                stmt.close();
                conn.close();
                xac.close();
                // Destroy TransactionManager object and close the connection
                // with the LIXA state server
                tm.delete();
            } catch (Exception e) {
                e.printStackTrace();
                System.exit(1);
            }
         }
    }
}
