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
    /* native PostgreSQL connection handler */
    PGconn                       *rm = NULL;
    /* PostgreSQL result */
    PGresult                     *pg_res;
    /* variable for PostgreSQL statement to execute */
    const char                   *postgresql_stmt;
    /* XTA Resource for PostgreSQL */
    xta::PostgresqlXaResource    *xar = NULL;
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
        postgresql_stmt = "INSERT INTO authors "
                "VALUES(1921, 'Rigoni Stern', 'Mario')";
    else
        postgresql_stmt = "DELETE FROM authors WHERE id=1921";
    // initialize XTA environment
    xta::Xta::Init();
    // create a new PostgreSQL connection
    rm = PQconnectdb("dbname=testdb");
    if (PQstatus(rm) != CONNECTION_OK) {
        fprintf(stderr, "PQconnectdb: returned error %s\n",
                PQerrorMessage(rm));
        PQfinish(rm);
        return 1;
    }
    try {
        // create a new XTA Transaction Manager object
        tm = new xta::TransactionManager();
        /*
         * create an XA resource for PostgreSQL
         * second parameter "PostgreSQL" is descriptive
         * third parameter "dbname=testdb" identifies the specific database
         */
        xar = new xta::PostgresqlXaResource(rm, "PostgreSQL", "dbname=testdb");
        // Create a new XA global transaction and retrieve a reference from
        // the TransactionManager object
        xta::Transaction tx = tm->CreateTransaction();
        // Enlist PostgreSQL resource to transaction
        tx.EnlistResource(xar);
        // Open all resources enlisted by the Transaction
        tx.Open();
        /*
         * *** NOTE: ***
         * at this point, subordinate Application Program must wait until
         * superior Application Program has started the transaction.
         * Here the synchronization is implemented with
         * a synchronous message passing using a named pipe (FIFO)
         */
        // open the pipe for read operation
        ifstream sup2subFifo(sup2sub_fifoname);
        if (!sup2subFifo) {
            cerr << "Unable to open file '" << sup2sub_fifoname << "'" << endl;
            return 1;
        }
        // read the message
        string xidString;
        sup2subFifo >> xidString;
        cout << "Subordinate AP has received XID '" << xidString <<
            "' from superior AP" << endl;
        // close the pipe
        sup2subFifo.close();
        // create a new branch in the same global transaction
        tx.Branch(xidString);
        // the branch has the same global identifier, but a different branch id
        string branchXidString = tx.getXid().toString();
        cout << "Subordinate AP has created a branch with XID '" <<
            branchXidString << "'" << endl;
        /*
         * *** NOTE: ***
         * subordinate Application Program (this one) has branched the
         * transaction and must send a message to the superior Application
         * Program that can proceed with it's own operations
         */
        // open the pipe for write operation
        ofstream sub2supFifo(sub2sup_fifoname);
        if (!sub2supFifo) {
            cerr << "Unable to open file '" << sub2sup_fifoname << "'" << endl;
            return 1;
        }
        // write the message
        sub2supFifo << branchXidString;
        cout << "Subordinate AP has returned '" << branchXidString <<
            "' to superior AP" << endl;
        // close the pipe
        sub2supFifo.close();
        /*
         * *** NOTE: ***
         * at this point the subordinate Application Program (this one) can
         * go on with its own operations indipendently from the superior AP
         */    
        // Execute PostgreSQL statement
        printf("PostgreSQL, executing >%s<\n", postgresql_stmt);
        pg_res = PQexec(rm, postgresql_stmt);
        if (PQresultStatus(pg_res) != PGRES_COMMAND_OK) {
            fprintf(stderr, "PostgreSQL, error while executing >%s<: %s\n",
                    postgresql_stmt, PQerrorMessage(rm));
            PQclear(pg_res);
            PQfinish(rm);
            return 1;
        }
        PQclear(pg_res);
        // commit or rollback the transaction
        if (commit) {
            tx.Commit();
            cout << "Subordinate AP has committed its branch" << endl;
        } else {
            tx.Rollback();
            cout << "Subordinate AP has rolled back its branch" <<endl;
        }
        // Close all resources enlisted by the Transaction
        tx.Close();
        // Delete PostgreSQL native and XA resource
        delete xar;
        // Close the PostgreSQL connection
        PQfinish(rm);
        // Delete Transaction Manager object
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
