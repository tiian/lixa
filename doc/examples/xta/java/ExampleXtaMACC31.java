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
// import Oracle package for XA Data Source
/*
import oracle.ucp.jdbc.PoolDataSourceFactory;
import oracle.ucp.jdbc.PoolXADataSource;
*/
import oracle.jdbc.xa.OracleXid;
import oracle.jdbc.xa.OracleXAException;
import oracle.jdbc.pool.*;
import oracle.jdbc.xa.client.*;



public class ExampleXtaMACC31 {
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 3) {
            System.err.println("This program requires three boolean " +
                               "parameters: 'commit', 'insert', 'superior'");
            System.exit(1);
        }
        // First parameter: commit transaction
        boolean commit = Integer.parseInt(args[0]) > 0 ? true : false;
        // Second parameter: insert data in databases
        boolean insert = Integer.parseInt(args[1]) > 0 ? true : false;
        // Third parameter: superior application program
        boolean superior = Integer.parseInt(args[2]) > 0 ? true : false;
        // variable for SQL statement to execute
        String sqlStmt;

        // XTA Transaction Manager
        TransactionManager tm;
        // XTA Transaction
        Transaction tx;
        
        // XA Resource for Oracle DBMS
        // PoolXADataSource pds = null;
        OracleXADataSource xads1 = null;
        XAConnection xac1 = null;
        XAResource xar1 = null;
        Connection conn1 = null; 
        Statement stmt1 = null;
        
        /*
         * Prepare SQL statements in accordance with "insert" command line
         * parameter
         */
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
             // A bit of scaffolding for PostgreSQL:
             //
             // 0. create a Pool (UCP)
             /*
             pds = PoolDataSourceFactory.getPoolXADataSource();
             pds.setConnectionFactoryClassName("oracle.jdbc.xa.client.OracleXADataSource");
             pds.setURL("jdbc:oracle:thin:@" +
                        "(DESCRIPTION=" +
                        "(ADDRESS=(PROTOCOL=tcp)" +
                        "(HOST=centos7-oracle12.brenta.org)(PORT=1521))" +
                        "(CONNECT_DATA=" +
                        "(SERVICE_NAME=orcl.brenta.org)))");
             pds.setUser("hr");
             pds.setPassword("hr");
             */
             // 1. create an XA Data Source
             xads1 = new OracleXADataSource();
             // 2. set connection parameters (one property at a time)
             xads1.setURL("jdbc:oracle:thin:@" +
                          "(DESCRIPTION=" +
                          "(ADDRESS=(PROTOCOL=tcp)" +
                          "(HOST=centos7-oracle12.brenta.org)(PORT=1521))" +
                          "(CONNECT_DATA=" +
                          "(SERVICE_NAME=orcl.brenta.org)))");
             //xads1.setURL("jdbc:oracle:thin:@centos7-oracle12.brenta.org:1521:orcl");
             xads1.setUser("HR");
             xads1.setPassword("hr");
             // 3. get an XA Connection from the XA Data Source
             xac1 = xads1.getXAConnection();
             System.out.println("###");
             // 4. get an XA Resource from the XA Connection
             xar1 = xac1.getXAResource();
             // 5. get an SQL Connection from the XA Connection
             conn1 = xac1.getConnection();
             
             try {
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
                 tx.enlistResource(xar1, "Oracle", "hr/hr");
                 // Start a new XA global transaction with a single branch
                 tx.start();

                 //
                 // At this point, it's time to do something with the
                 // Resource Manager
                 //
                 // Create and Execute Oracle DBMS statement
                 //
                 System.out.println("Oracle, executing >" +
                                    sqlStmt + "<");
                 // create a Statement object
                 stmt1 = conn1.createStatement();
                 // Execute the statement
                 stmt1.executeUpdate(sqlStmt);
                 // commit or rollback
                 if (commit)
                     tx.commit();
                 else
                     tx.rollback();
                 
             } catch (XtaException e) {
                 System.err.println("XtaException: LIXA ReturnCode=" +
                                    e.getReturnCode() + " ('" +
                                    e.getMessage() + "')");
                 e.printStackTrace();
                 System.exit(1);
             } catch (XAException e) {
                 e.printStackTrace();
                 System.exit(1);
             }

         } catch (Exception e) {
             e.printStackTrace();
             System.exit(1);
         } finally {
             try {
                 // Close Statement, SQL Connection and XA Connection for
                 // PostgreSQL
                 stmt1.close();
                 conn1.close();
                 xac1.close();
             } catch (Exception e) {
                 e.printStackTrace();
                 System.exit(1);
             }
         }
    }
}
