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
import javax.sql.XAConnection;
// import Java useful stuff
import java.io.*;
import java.util.concurrent.TimeUnit;
// import SQL
import java.sql.Connection;
import java.sql.Statement;
// import MySQL package for XA Data Source
import com.mysql.jdbc.jdbc2.optional.MysqlXADataSource;
// import Oracle package for XA Data Source
import oracle.jdbc.xa.OracleXid;
import oracle.jdbc.xa.OracleXAException;
//import oracle.jdbc.*;
import oracle.jdbc.pool.*;
import oracle.jdbc.xa.client.*;
// import PostgreSQL package for XA Data Source
import org.postgresql.xa.PGXADataSource;



/*
 * Pseudo asynchronous branch case test for XTA
 *
 * NOTE: this is not a good example to learn the Java programming language
 *       because the usage of static variables has been abused and you should
 *       not write software as this one. This piece of Java code mimics as much
 *       as possible case0104.c to guarantee the same behavior.
 *       A huge amount of global variables has been used to avoid parameters
 *       for boilerplate functions: the meaning of the flow should be more
 *       evident without useless details.
 */



/*
 * EXIT CODES:
 *  0:   OK
 *  1:   generic error
 *  2:   superior branch / xta_transaction_commit() -> LIXA_RC_TX_ROLLBACK
 *  2:   intermediate branch / xta_transaction_commit() -> LIXA_RC_TX_ROLLBACK
 *  4:   subordinate branch / xta_transaction_commit() -> LIXA_RC_TX_ROLLBACK
 */



public class case4104 {
    // parsed command line arguments
    private static int branchType;
    private static boolean insert;
    private static int statement;
    private static boolean commit;
    private static int testRc;
    private static String fifoTo;
    private static String fifoFrom;
    // strings for SQL statements
    private static String mysqlStmtInsert;
    private static String mysqlStmtDelete;
    private static String oracleStmtInsert;
    private static String oracleStmtDelete;
    private static String postgresStmtInsert;
    private static String postgresStmtDelete;
    // XTA Transaction Manager
    private static TransactionManager tm = null;
    // XTA Transaction
    private static Transaction tx = null;
    // XA LIXA dummy resource
    private static LixaDummyXAResource xar0 = null;
    // XA Resource for MySQL
    private static MysqlXADataSource xads1 = null;
    private static XAConnection xac1 = null;
    private static XAResource xar1 = null;
    private static Connection conn1 = null;
    private static Statement stmt1 = null;
    // XA Resource for Oracle
    private static OracleXADataSource xads2 = null;
    private static XAConnection xac2 = null;
    private static XAResource xar2 = null;
    private static Connection conn2 = null;
    private static Statement stmt2 = null;
    // XA Resource for PostgreSQL
    private static PGXADataSource xads3 = null;
    private static XAConnection xac3 = null;
    private static XAResource xar3 = null;
    private static Connection conn3 = null;
    private static Statement stmt3 = null;
    
    public static void main(String[] args) {
        // Check command line parameters
        if (args.length < 7) {
            System.err.println("This program requires at least 7 options");
            System.exit(1);
        }
        // 0: SUPERIOR
        // 1: INTERMEDIATE
        // 2: SUBORDINATE
        // 10: RECOVERY
        branchType = Integer.parseInt(args[0]);
        insert = Integer.parseInt(args[1]) > 0 ? true : false;
        statement = Integer.parseInt(args[2]);
        commit = Integer.parseInt(args[3]) > 0 ? true : false;
        testRc = Integer.parseInt(args[4]);
        fifoTo = args[5];
        fifoFrom = args[6];

        // choose the SQL statements that must be executed by this branch
        statementSetup();

        // check branchType
        if (branchType >= 10) {
            branchType -= 10;
            recovery();
        } else switch (branchType) {
                case 0: // SUPERIOR
                    /* this is the superior task: it passes the transactional
                     * context to intermediate */
                    superior();
                    break;
                case 1: // INTERMEDIATE
                    /* this is the intermediate task: it receives the
                     * transactional
                     * context by superior and it passes it to subordinate */
                    intermediate();
                    break;
                case 2: // SUBORDINATE
                    /* this is the subordinate task: it receives the
                     * transactional context by intermediate */
                    subordinate();
                    break;
                default:
                    System.err.println("branchType=" + branchType +
                                       "UNKNOWN");
                    System.exit(1);
            }
    }
    private static void statementSetup() {
        switch (statement) {
            case 1:
                mysqlStmtInsert    = "INSERT INTO authors VALUES(101, 'Ernest', 'Hemingway')";
                mysqlStmtDelete    = "DELETE FROM authors WHERE id=101";
                oracleStmtInsert   = "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) VALUES (101, 'Bulgakov', 'Michail')";
                oracleStmtDelete   = "DELETE FROM authors WHERE ID = 101";
                postgresStmtInsert = "INSERT INTO authors VALUES(101, 'Milan', 'Kundera');";
                postgresStmtDelete = "DELETE FROM authors WHERE id=101;";
                break;
            case 2:
                mysqlStmtInsert    = "INSERT INTO authors VALUES(102, 'Giorgio', 'Saviane')";
                mysqlStmtDelete    = "DELETE FROM authors WHERE id=102";
                oracleStmtInsert   = "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) VALUES (102, 'Solzenicyn', 'Aleksandr')";
                oracleStmtDelete   = "DELETE FROM authors WHERE ID = 102";
                postgresStmtInsert = "INSERT INTO authors VALUES(102, 'Jostein', 'Gaarder');";
                postgresStmtDelete = "DELETE FROM authors WHERE id=102;";
                break;
            case 3:
                mysqlStmtInsert    = "INSERT INTO authors VALUES(103, 'Philip', 'Roth')";
                mysqlStmtDelete    = "DELETE FROM authors WHERE id=103";
                oracleStmtInsert   = "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) VALUES (103, 'Dostoevskij', 'Fedor')";
                oracleStmtDelete   = "DELETE FROM authors WHERE ID = 103";
                postgresStmtInsert = "INSERT INTO authors VALUES(103, 'Patrick', 'Suskind');";
                postgresStmtDelete = "DELETE FROM authors WHERE id=103;";
                break;
            default:
                System.err.println("Statement=" + statement +
                                   " is not valid!");
                System.exit(1);
        } // switch (statement)
    }
    private static void recovery() {
        System.err.println("branchType=" + branchType + " (RECOVERY");
        // initial boilerplate code
        createXaResources();
        createTransactionManager();
        createTransaction();
        enlistResourcesToTransaction();
        // force transaction recovery
        try {
            tx.recover();
        } catch  (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
            e.printStackTrace();
            System.exit(1);
        } catch (XAException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
    private static void createXaResources() {
        try {
            // Create a dummy resource
            xar0 = new LixaDummyXAResource();
            if (branchType == 0) { // SUPERIOR
                //
                // A bit of scaffolding for MySQL/MariaDB
                //
                // 1. create an XA Data Source
                xads1 = new MysqlXADataSource();
                // 2. set connection parameters using a connection URL
                xads1.setUrl("jdbc:mysql://localhost/lixa?user=lixa&password=");
                // 3. get an XA Connection from the XA Data Source
                xac1 = xads1.getXAConnection();
                // 4. get an XA Resource from the XA Connection
                xar1 = xac1.getXAResource();
                // 5. get an SQL Connection from the XA Connection
                conn1 = xac1.getConnection();
            }
            if (branchType == 1) { // INTERMEDIATE
                //
                // A bit of boilerplate for Oracle JDBC & XA
                //
                // 1. create an XA Data Source
                xads2 = new OracleXADataSource();
                // 2. set connection URL (JDBC thin driver), user and password
                xads2.setURL("jdbc:oracle:thin:@" +
                             "(DESCRIPTION=" +
                             "(ADDRESS=(PROTOCOL=tcp)" +
                             "(HOST=centos7-oracle12.brenta.org)(PORT=1521))" +
                             "(CONNECT_DATA=" +
                             "(SERVICE_NAME=orcl.brenta.org)))");
                xads2.setUser("hr");
                xads2.setPassword("hr");
                // 3. get an XA Connection from the XA Data Source
                xac2 = xads2.getXAConnection();
                // 4. get an XA Resource from the XA Connection
                xar2 = xac2.getXAResource();
                // 5. get an SQL Connection from the XA Connection
                conn2 = xac2.getConnection();
            }
            if (branchType == 2) { // SUBORDINATE
                //
                // A bit of scaffolding for PostgreSQL:
                //
                // 1. create an XA Data Source
                xads3 = new PGXADataSource();
                // 2. set connection parameters (one property at a time)
                xads3.setServerName("localhost");
                xads3.setDatabaseName("testdb");
                xads3.setUser("tiian");
                xads3.setPassword("passw0rd");
                // 3. get an XA Connection from the XA Data Source
                xac3 = xads3.getXAConnection();
                // 4. get an XA Resource from the XA Connection
                xar3 = xac3.getXAResource();
                // 5. get an SQL Connection from the XA Connection
                conn3 = xac3.getConnection();
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
    private static void createTransactionManager() {
        try {
            // create a Transaction Manager
            tm = new TransactionManager();
        } catch  (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
            e.printStackTrace();
            System.exit(1);
        }
    }
    private static void createTransaction() {
        try {
            // create a new Transation
            tx = tm.createTransaction();
        } catch  (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
            e.printStackTrace();
            System.exit(1);
        }
    }
    private static void enlistResourcesToTransaction() {
        try {
            // enlist Lixa Dummy resource
            tx.enlistResource(xar0, "LixaDummy", "LixaDummy");
            if (branchType == 0) { // SUPERIOR
                // enlist MySQL resource to Transaction
                tx.enlistResource(
                    xar1, "MySQL",
                    "jdbc:mysql://localhost/lixa?user=lixa/password=");
            }
            if (branchType == 1) { // INTERMEDIATE
                // enlist Oracle resource to transaction
                tx.enlistResource(xar2, "Oracle DB", "orcl.brenta.org/hr/hr");
            }
            if (branchType == 2) { // SUBORDINATE
                // enlist PostgreSQL resource to transaction
                tx.enlistResource(xar3, "PostgreSQL",
                                  "localhost/testdb/tiian/passw0rd");
            }
        } catch  (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
            e.printStackTrace();
            System.exit(1);
        }
    }
    private static void useXaResources() {
        switch (branchType) {
            case 0: // SUPERIOR
                String mysqlStmt = null;
                if (insert)
                    mysqlStmt = mysqlStmtInsert;
                else
                    mysqlStmt = mysqlStmtDelete;
                try {
                    // create a Statement object for MySQL/MariaDB
                    stmt1 = conn1.createStatement();
                    // Execute MySQL statements
                    stmt1.executeUpdate(mysqlStmt);
                    // close the statement
                    stmt1.close();
                } catch (Exception e) {
                    e.printStackTrace();
                    System.exit(1);
                }
                System.out.println("MySQL statement >" + mysqlStmt +
                                   "< completed");
                break;
            case 1: // INTERMEDIATE
                String oracleStmt = null;
                if (insert)
                    oracleStmt = oracleStmtInsert;
                else
                    oracleStmt = oracleStmtDelete;
                try {
                    // create a Statement object for Oracle
                    stmt2 = conn2.createStatement();
                    // Execute Oracle statement
                    stmt2.executeUpdate(oracleStmt);
                    // close the statement
                    stmt2.close();
                } catch (Exception e) {
                    e.printStackTrace();
                    System.exit(1);
                }
                System.out.println("Oracle statement >" + oracleStmt +
                                   "< completed");
                break;
            case 2: // SUBORDINATE
                String postgresStmt = null;
                if (insert)
                    postgresStmt = postgresStmtInsert;
                else
                    postgresStmt = postgresStmtDelete;
                try {
                    // create a Statement object for PostgreSQL
                    stmt3 = conn3.createStatement();
                    // Execute PostgreSQL statements
                    stmt3.executeUpdate(postgresStmt);
                    // close the statement
                    stmt3.close();
                } catch (Exception e) {
                    e.printStackTrace();
                    System.exit(1);
                }
                System.out.println("PostgreSQL statement >" + postgresStmt +
                                   "< completed");
                break;
            default:
                System.err.println("branchType=" + branchType +
                                   "UNKNOWN");
                System.exit(1);
        } // switch (branchType)
    }
    private static void superior() {
        String xidString = null;
        System.err.println("branchType=" + branchType + " (SUPERIOR)");
        // initial boilerplate code
        createXaResources();
        createTransactionManager();
        createTransaction();
        enlistResourcesToTransaction();
        // interesting code for XTA branching
        try {
            // start a new Distributed Transaction
            tx.start(true);
            // retrieve the XID associated to the started transaction
            xidString = tx.getXid().toString();
            System.err.println("passing XID '" + xidString +
                               "' to intermediate branch");
            // passing string to intermediate application program
            msgSend(fifoTo, xidString);
            // update XA resources under the control of XTA
            useXaResources();
            // put a simple delay to allow the progress of the other branches
            TimeUnit.SECONDS.sleep(1);
            try {
                if (commit) {
                    tx.commit();
                    System.err.println("XTA commit completed");
                } else {
                    tx.rollback();
                    System.err.println("XTA rollback completed");
                }
            } catch (XtaException e) {
                if (e.getReturnCode() != testRc)
                    throw e;
                System.err.println("commit/rollback returned " +
                                   e.getReturnCode() + " as expected");
            }
        } catch (XtaException e) {
            System.err.println("XtaException: LIXA ReturnCode=" +
                               e.getReturnCode() + " ('" +
                               e.getMessage() + "')");
            e.printStackTrace();
            System.exit(1);
        }  catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        } finally {
            // Destroy TransactionManager object and close the connection
            // with the LIXA state server
            tm.delete();
        }
    }
    private static void intermediate() {
        System.err.println("branchType=" + branchType + " (INTERMEDIATE)");
        // initial boilerplate code
        createXaResources();
        createTransactionManager();
        createTransaction();
        enlistResourcesToTransaction();
        // receiving XID (transactional context) from superior branch
        String xidString = msgReceive(fifoFrom);
        // interesting code for XTA branching
        try {
            // start a new Distributed Transaction
            tx.branch(xidString);
            System.err.println("passing XID '" + xidString +
                               "' to subordinate branch");
            // passing string to intermediate application program
            msgSend(fifoTo, xidString);
            // update XA resources under the control of XTA
            useXaResources();
            // put a simple delay to allow the progress of the other branches
            TimeUnit.SECONDS.sleep(1);
            try {
                if (commit) {
                    tx.commit();
                    System.err.println("XTA commit completed");
                } else {
                    tx.rollback();
                    System.err.println("XTA rollback completed");
                }
            } catch (XtaException e) {
                if (e.getReturnCode() != testRc)
                    throw e;
                System.err.println("commit/rollback returned " +
                                   e.getReturnCode() + " as expected");
            }
        } catch  (XtaException e) {
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
    private static void subordinate() {
        System.err.println("branchType=" + branchType + " (SUBORDINATE)");
        // initial boilerplate code
        createXaResources();
        createTransactionManager();
        createTransaction();
        enlistResourcesToTransaction();
        // receiving XID (transactional context) from superior branch
        String xidString = msgReceive(fifoFrom);
        // interesting code for XTA branching
        try {
            // start a new Distributed Transaction
            tx.branch(xidString);
            // update XA resources under the control of XTA
            useXaResources();
            // put a simple delay to allow the progress of the other branches
            TimeUnit.SECONDS.sleep(1);
            try {
                if (commit) {
                    tx.commit();
                    System.err.println("XTA commit completed");
                } else {
                    tx.rollback();
                    System.err.println("XTA rollback completed");
                }
            } catch (XtaException e) {
                if (e.getReturnCode() != testRc)
                    throw e;
                System.err.println("commit/rollback returned " +
                                   e.getReturnCode() + " as expected");
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
    private static String msgReceive(String fifoName) {
        String buffer = null;
        try {
            BufferedReader input =
                new BufferedReader(new FileReader(fifoName));
            buffer = input.readLine();
            input.close();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        return buffer;
    }
    private static void msgSend(String fifoName, String buffer) {
        try {
            BufferedWriter output =
                new BufferedWriter(new FileWriter(fifoName));
            output.write(buffer);
            output.close();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
