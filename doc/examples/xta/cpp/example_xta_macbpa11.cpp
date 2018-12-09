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
 * third parameter:  "sup2sub_filename", a string with the name of the FIFO
 *                   (named pipe) that must be used for superior->subordinate
 *                   communication
 * fourth parameter: "sub2sup_filename", a string with the name of the FIFO
 *                   (named pipe) that must be used for subordinate->superior
 *                   communication
 *
 * Programming Style note:
 * the purpose of this small program is not to explain C++ development
 * techniques or good style, but simply to show XTA for C++ using the easiest
 * approach.
 * "xta" namespace is explicitly put in every statement (see "xta::") but it
 * can be avoided with "using namespace xta;"
 */



// Standard headers
#include <iostream>
#include <fstream>
// This header is necessary for all the stuff related to XTA
#include <xta/cpp/Xta.hpp>



int main(int argc, char *argv[])
{
    /* First parameter: commit transaction? */
    int                           commit;
    /* Second parameter: insert data in databases? */
    int                           insert;
    /* Third parameter: name of the named pipe (FIFO) that must be used to
       send from superior AP to subordinate AP */
    const char                   *sup2sub_fifoname = NULL;
    /* Fourth parameter: name of the named pipe (FIFO) that must be used to
       send from subordinate AP to superior AP */
    const char                   *sub2sup_fifoname = NULL;
    /* native MySQL connection handler */
    MYSQL                        *rm = NULL;
    /* variable for MySQL statement to execute */
    const char                   *mysql_stmt;
    /* XTA Resource for MySQL */
    xta::MysqlXaResource         *xar = NULL;
    /* XTA Transaction Manager object reference */
    xta::TransactionManager      *tm = NULL;

    /*
     * Check command line parameters
     */
    if (argc < 5) {
        fprintf(stderr, "This program requires two boolean parameters: "
                "'commit', 'insert' and two strings: "
                "'Superior2SubordinateFIFOname', "
                "'Subordinate2SuperiorFIFOname'\n");
        return 1;
    }
    commit = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    sup2sub_fifoname = argv[3];
    sub2sup_fifoname = argv[4];
    /*
     * Prepare SQL statements in accordance with "insert" command line
     * parameter
     */
    if (insert)
        mysql_stmt = "INSERT INTO authors VALUES(1919, 'Levi', 'Primo')";
    else
        mysql_stmt = "DELETE FROM authors WHERE id=1919";
    // initialize XTA environment
    xta::Xta::init();
    // create a new MySQL connection (if subordinate Application Program)
    rm = mysql_init(NULL);
    if (rm == NULL) {
        fprintf(stderr, "mysql_init: returned NULL\n");
        return 1;
    }
    if (mysql_real_connect(rm, "localhost", "lixa", "",
                           "lixa", 0, NULL, 0) == NULL) {
        fprintf(stderr, "mysql_real_connect: returned error: %u, %s\n",
                mysql_errno(rm), mysql_error(rm));
        return 1;
    }

    try {
        // create a new XTA Transaction Manager object
        tm = new xta::TransactionManager();
        // create an XA resource for MySQL
        xar = new xta::MysqlXaResource(rm, "MySQL", "localhost,0,lixa,,lixa");
        // Create a new XA global transaction and retrieve a reference from
        // the TransactionManager object
        xta::Transaction tx = tm->createTransaction();
        // Enlist MySQL resource to Transaction
        tx.enlistResource(xar);
        /*
         * Start a new XA global transaction with multiple branches
         * Note: argument ("MultipleBranch") has true value because the
         *       transaction will be branched by the subordinate Application
         *       Program
         */
        tx.start(true);
        /*
         * Retrieve the Transaction ID (XID) associated to the transaction
         * that has been started in the previous step
         */
        string xidString = tx.getXid().toString();
        /*
         * *** NOTE: ***
         * a synchronization message must be sent to the subordinate
         * Application Program: the global transaction has been started and
         * the subordinate AP can branch it. The synchronization is implemented
         * with a synchronous message passing using a named pipe (FIFO)
         */    
        // open the pipe for write operation
        ofstream sup2subFifo(sup2sub_fifoname);
        if (!sup2subFifo) {
            cerr << "Unable to open file '" << sup2sub_fifoname << "'" << endl;
            return 1;
        }
        // write the message
        sup2subFifo << xidString;
        sup2subFifo.close();
        cout << "Superior AP has sent XID '" << xidString <<
            "' to subordinate AP" << endl;
        // open the pipe for read operation
        ifstream sub2supFifo(sub2sup_fifoname);
        if (!sub2supFifo) {
            cerr << "Unable to open file '" << sub2sup_fifoname << "'" << endl;
            return 1;
        }
        // read the reply message
        string reply;
        sub2supFifo >> reply;
        sub2supFifo.close();
        cout << "Superior AP has received '" << reply << "' reply from "
            "subordinate AP" << endl;
        /*
         * *** NOTE: ***
         * at this point the subordinate Application Program has branched the
         * transaction and this (superior) Application Program can go on with
         * the main branch created by xta::Transaction.Start() indipendently
         * from the subordinate AP
         */    
        // Execute MySQL statement
        printf("MySQL, executing >%s<\n", mysql_stmt);
        if (mysql_query(rm, mysql_stmt)) {
            fprintf(stderr, "MySQL, error while executing >%s<: %u/%s\n",
                    mysql_stmt, mysql_errno(rm), mysql_error(rm));
            mysql_close(rm);
            return 1;
        }
        // commit or rollback the transaction
        if (commit) {
            tx.commit();
            cout << "Superior AP has committed its branch" << endl;
        } else {
            tx.rollback();
            cout << "Superior AP has rolled back its branch" << endl;
        }
        // Delete MySQL native and XA resource
        delete xar;
        // Close the MySQL connection
        mysql_close(rm);
        //Delete Transaction Manager object
        delete tm;

    } catch (xta::Exception e) {
        /*
         * what() is a standard method that describes the exception
         * where() is a method provided by XTA to describe the XTA C function
         *         that raised the exception
         * getReturnCode() is a method provided by XTA to retrieve the
         *                 integer reason code returned by XTA C function
         *                 (see file lixa_errors.h)
         */
        cerr << "Exception in function '" << e.where() <<
            "', return code description: '" << e.what() << "', " <<
            "return code: " << e.getReturnCode() << endl;
        return 1;
    }
    
    return 0;
}
