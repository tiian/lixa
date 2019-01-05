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



/*
 * This program is an example implementation of the
 * "Multiple Applications, Concurrent Branches/Pseudo Asynchronous" Pattern
 * as documented in LIXA manual:
 * http://www.tiian.org/lixa/manuals/html/index.html
 *
 * This program accepts exactly four parameters on the command line:
 * first parameter:  "commit", boolean value (if FALSE, "rollback")
 * second parameter: "insert", boolean value (if FALSE, "delete")
 * third parameter:  "sup2subFilename", a string with the name of the FIFO
 *                   (named pipe) that must be used for superior->subordinate
 *                   communication
 * fourth parameter: "sub2supFilename", a string with the name of the FIFO
 *                   (named pipe) that must be used for subordinate->superior
 *                   communication
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
// import MySQL package for XA Data Source
import com.mysql.jdbc.jdbc2.optional.MysqlXADataSource;
// import Java io
import java.io.*;



public class ExampleXtaMACBPA31 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 4) {
            System.err.println("This program requires two boolean " +
                               "parameters: 'commit', 'insert', and two " +
                               "strings: " +
                               "'superior2SubordinateFIFOname', " +
                               "'Subordinate2SuperiorFIFOname'");
            System.exit(1);
        }
        // First parameter: commit transaction
        boolean commit = Integer.parseInt(args[0]) > 0 ? true : false;
        // Second parameter: insert data in databases
        boolean insert = Integer.parseInt(args[1]) > 0 ? true : false;
        // Third parameter: name of the FIFO used to communicate from Superior
        //                  to Subordinate
        String sup2subFifoname = args[2];
        // Fourth parameter: name of the FIFO used to communicate from
        //                   Subordinate to Superior
        String sub2supFifoname = args[3];
        // variable for SQL statement to execute
        String sqlStmt;

        // XTA Transaction Manager
        TransactionManager tm;
        // XTA Transaction
        Transaction tx;
        
        // XA Resource for MySQL
        MysqlXADataSource xads = null;
        XAConnection xac = null;
        XAResource xar = null;
        Connection conn = null;
        Statement stmt = null;
             
        // Prepare SQL statements in accordance with "insert" command line
        // parameter
         if (insert)
             // SQL INSERT
             sqlStmt = "INSERT INTO authors VALUES(1919, 'Levi', 'Primo')";
         else
             // SQL DELETE
             sqlStmt = "DELETE FROM authors WHERE id=1919";
         
         try {
             //
             // A bit of scaffolding for MySQL/MariaDB
             //
             // 1. create an XA Data Source             
             xads = new MysqlXADataSource();
             // 2. set connection parameters using a connection URL
             xads.setUrl("jdbc:mysql://localhost/lixa?user=lixa&password=");
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
             // Enlist MySQL/MariaDB resource to transaction
             tx.enlistResource(
                 xar, "MySQL",
                 "jdbc:mysql://localhost/lixa?user=lixa/password=");
             /*
              * Start a new XA global transaction with multiple branches
              * Note: argument ("MultipleBranch") has true value because
              *       the transaction will be branched by the subordinate
              *       Application Program
              */
             tx.start(true);
             // Retrieve the Transaction ID (XID) associated to the
             // transaction that has been created in the previous step
             String xidString = tx.getXid().toString();
             /*
              * *** NOTE: ***
              * a Remote Procedure Call (RPC) or a Web Service (WS) or a
              * REST API is emulated by a synchronous message passing
              * using a named pipe (FIFO)
              */ 
             // Write XID to the FIFO, pass it from Superior to Subordinate
             BufferedWriter output = new BufferedWriter(
                 new FileWriter(sup2subFifoname));
             output.write(xidString);
             System.out.println("Superior AP has sent XID '" + xidString +
                                "' to subordinate AP");
             output.close();
             // read answer from FIFO
             BufferedReader input = new BufferedReader(
                 new FileReader(sub2supFifoname));
             String reply = input.readLine();
             System.out.println("Superior AP has received '" + reply +
                                "' reply from subordinate AP");
             input.close();
             /*
              * *** NOTE: ***
              * at this point the subordinate Application Program has branched
              * the transaction and this (superior) Application Program can go
              * on with the main branch created by Transaction.start()
              * indipendently from the subordinate AP
              */ 
             //
             // Create and Execute a JDBC statement
             //
             System.out.println("MySQL, executing >" + sqlStmt + "<");
             // create a Statement object
             stmt = conn.createStatement();
             // Execute the statement
             stmt.executeUpdate(sqlStmt);
             // close the statement
             stmt.close();
             // commit or rollback
             if (commit) {
                 tx.commit();
                 System.out.println("Superior AP has committed its branch");
             } else {
                 tx.rollback();
                 System.out.println("Superior AP has rolled back its branch");
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
             } catch (Exception e) {
                 e.printStackTrace();
                 System.exit(1);
             }
         }
    }
}
