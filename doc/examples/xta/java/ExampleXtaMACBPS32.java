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
// import PostgreSQL package for XA Data Source
import org.postgresql.xa.PGXADataSource;
// import Java io
import java.io.*;



public class ExampleXtaMACBPS32 {
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
        
        // XA Resource for PostgreSQL
        PGXADataSource xads = null;
        XAConnection xac = null;
        XAResource xar = null;
        Connection conn = null; 
        Statement stmt = null;
        
        // Prepare SQL statements in accordance with "insert" command line
        // parameter
         if (insert)
             // SQL INSERT
             sqlStmt = "INSERT INTO authors " +
                 "VALUES(1921, 'Rigoni Stern', 'Mario')";
         else
             // SQL DELETE
             sqlStmt = "DELETE FROM authors WHERE id=1921";
         
         try {
             //
             // A bit of scaffolding for PostgreSQL:
             //
             // 1. create an XA Data Source
             xads = new PGXADataSource();
             // 2. set connection parameters (one property at a time)
             xads.setServerName("localhost");
             xads.setDatabaseName("testdb");
             xads.setUser("tiian");
             xads.setPassword("passw0rd");
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
             // Enlist PostgreSQL resource to transaction
             tx.enlistResource(xar, "PostgreSQL",
                               "localhost/testdb/tiian/passw0rd");
             /*
              * *** NOTE: ***
              * at this point, subordinate Application Program must wait a
              * Remote Procedure Call (RPC) or a Web Service (WS) or a REST
              * API invocation from superior Application Program. Here the
              * incoming call is emulated with a synchronous message
              * passing using a named pipe (FIFO)
              */
             // read message from FIFO to get XID
             BufferedReader input = new BufferedReader(
                 new FileReader(sup2subFifoname));
             String xidString = input.readLine();
             System.out.println("Subordinate AP has received XID '" +
                                xidString + "' from superior AP");
             input.close();
             /*
              * *** NOTE: ***
              * at this point the subordinate Application Program (this
              * one) has been called by the superior A.P. that's waiting a
              * reply. Now this program can branch the global transaction
              * previously started by the superior A.P.
              */
             // create a new branch in the same global transaction
             tx.branch(xidString);
             /*
              * the branch has the same global identifier, but a different
              * branch id; the following statement is for the sake of
              * debugging only
              */
             String branchXidString = tx.getXid().toString();
             System.out.println("Subordinate AP has created a branch " +
                                "with XID '" + branchXidString + "'");
             //
             // Create and Execute a JDBC statement for PostgreSQL
             //
             System.out.println("PostgreSQL, executing >" + sqlStmt + "<");
             // create a Statement object
             stmt = conn.createStatement();
             // Execute the statement
             stmt.executeUpdate(sqlStmt);
             System.out.println("ZZZZZ");
             // commit or rollback the transaction
             if (commit) {
                 /*
                  * *** NOTE: ***
                  * commit MUST be performed in two step:
                  * 1. in first step, the branch is only "prepared" (as in
                  *    XA specification) and control can be returned to the
                  *    superior AP that has to start its commit
                  * 2. in the second step, the branch is definitely
                  *    "committed", but the operation will block the
                  *    caller because the subordinate AP must wait the
                  *    "prepared" state of the superior AP before
                  *    committing
                  */
                 // commit is performed with "NonBlocking" flag set to
                 // true: this is necessary to allow the superior branch
                 // to start commit */
                 tx.commit(true);
             } else {
                 tx.rollback();
             }
             /*
              * *** NOTE: ***
              * at this point the subordinate Application Program (this
              * one) has to reply to the superior A.P. that's waiting.
              * Here the reply is emulated with a synchronous message
              * passing using a named pipe (FIFO)
              */
             // open the pipe for write operation
             BufferedWriter output = new BufferedWriter(
                 new FileWriter(sub2supFifoname));
             // prepare the reply message
             String reply;
             if (commit)
                 reply = "PREPARED_for_COMMIT";
             else
                 reply = "ROLLBACK";
             // write the message
             output.write(xidString);
             System.out.println("Subordinate AP has returned '" + reply +
                                "' to superior AP");
             output.close();
             if (commit) {
                 /*
                  * Complete the second phase of the commit with
                  * "NonBlocking" flag set to false: this is necessary to
                  * wait the superior AP prepare phase
                  */
                 tx.commit(false);
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
