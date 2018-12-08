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
 * "Multiple Applications, Concurrent Branches/Pseudo Synchronous" Pattern
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
#include "cpp/Xta.hpp"



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
    xta::Xta::init();
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
        xta::Transaction tx = tm->createTransaction();
        // enlist PostgreSQL resource to transaction
        tx.enlistResource(xar);
        /*
         * *** NOTE: ***
         * at this point, subordinate Application Program must wait a Remote
         * Procedure Call (RPC) or a Web Service (WS) or a REST API invocation
         * from superior Application Program. Here the incoming call is
         * emulated with a synchronous message passing using a named pipe
         * (FIFO)
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
        /*
         * *** NOTE: ***
         * at this point the subordinate Application Program (this one) has
         * been called by the superior A.P. that's waiting a reply. Now this
         * program can branch the global transaction previously started by the
         * superior A.P.
         */
        
        // create a new branch in the same global transaction
        tx.branch(xidString);
        /*
         * the branch has the same global identifier, but a different branch
         * id; the following statement is for the sake of debugging only
         */
        string branchXidString = tx.getXid().toString();
        cout << "Subordinate AP has created a branch with XID '" <<
            branchXidString << "'" << endl;
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
            /*
             * *** NOTE: ***
             * commit MUST be performed in two step:
             * 1. in first step, the branch is only "prepared" (as in XA
             *    specification) and control can be returned to the superior AP
             *    that has to start its commit
             * 2. in the second step, the branch is definitely "committed", but
             *    the operation will block the caller because the subordinate
             *    AP must wait the "prepared" state of the superior AP before
             *    committing
             */
            /* commit is performed with "NonBlocking" flag set to true: this is
               necessary to allow the superior branch to start commit */
            tx.commit(true);
        } else {
            tx.rollback();
        }
        /*
         * *** NOTE: ***
         * at this point the subordinate Application Program (this one) has to
         * reply to the superior A.P. that's waiting. Here the reply is
         * emulated with a synchronous message passing using a named pipe
         * (FIFO)
         */
        // open the pipe for write operation
        ofstream sub2supFifo(sub2sup_fifoname);
        if (!sub2supFifo) {
            cerr << "Unable to open file '" << sub2sup_fifoname << "'" << endl;
            return 1;
        }
        // prepare the reply message
        string reply;
        if (commit)
            reply = "PREPARED_for_COMMIT";
        else
            reply = "ROLLBACK";
        // write the message
        sub2supFifo << reply;
        cout << "Subordinate AP has returned '" << reply <<
            "' to superior AP" << endl;
        // close the pipe
        sub2supFifo.close();
        if (commit) {
            /*
             * Complete the second phase of the commit with "NonBlocking" flag
             * set to false: this is necessary to wait the superior AP
             * prepare phase
             */
            tx.commit(false);
        }
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
