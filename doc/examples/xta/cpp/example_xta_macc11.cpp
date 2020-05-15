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
// This header is necessary for Oracle C Interface
#include <oci.h>



int main(int argc, char *argv[])
{
    /* LIXA / XTA return and reason code */
    int                           rc;
    /* First parameter: commit transaction? */
    int                           commit;
    /* Second parameter: insert data in databases? */
    int                           insert;
    /* Third parameter: superior application program? */
    int                           superior;
    /* XTA native XA resource to use for Oracle DBMS */
    xta::NativeXaResource        *xar = NULL;
    /* Variable for Oracle SQL statement that must be executed */
    text                         *oci_stmt;
    /* XTA Transaction Manager object reference */
    xta::TransactionManager      *tm = NULL;
    /* Name of the file that must be used to pass XID from superior to
     * subordinate program */
    const char                   *xid_filename = NULL;
    /* Oracle OCI environment variables and handles */
    OCIEnv                       *oci_env;
    OCISvcCtx                    *oci_svc_ctx;
    OCIStmt                      *oci_stmt_hndl;
    OCIError                     *oci_err_hndl;
    
    /*
     * Check command line parameters
     */
    if (argc < 5) {
        fprintf(stderr, "This program requires three boolean parameters: "
                "'commit', 'insert' and 'superior' and one string: "
                "'XIDfilename'\n");
        return 1;
    }
    commit = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    superior = strtol(argv[3], NULL, 0);
    xid_filename = argv[4];
    // Prepare SQL statement in accordance with "insert" and "superior" command
    // line parameter
    if (insert) {
        // SQL INSERT
        if (superior)
            oci_stmt = (text *)
                "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) "
                "VALUES(1930, 'Bonatti', 'Walter')";
        else
            oci_stmt = (text *)
                "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) "
                "VALUES(1948, 'Casarotto', 'Renato')";
    } else {
        // SQL DELETE
        if (superior)
            oci_stmt = (text *) "DELETE FROM authors WHERE ID=1930";
        else
            oci_stmt = (text *) "DELETE FROM authors WHERE ID=1948";
    }

    // initialize XTA environment
    xta::Xta::init();
    try {
        // dynamically create a native XA resource for Oracle DBMS
        xar = new xta::NativeXaResource(
            "OracleIC_stareg",
            "/opt/lixa/lib/switch_oracle_stareg.so",
            "ORACLE_XA+Acc=P/hr/hr+SesTm=30+LogDir=/tmp+"
            "threads=true+DbgFl=7+SqlNet=lixa_ora_db+"
            "Loose_Coupling=true", "");
        // create a new XTA Transaction Manager object
        tm = new xta::TransactionManager();
        // Create a new XA global transaction and retrieve a reference from
        // the TransactionManager object
        xta::Transaction tx = tm->createTransaction();
        // Enlist Oracle DMBS resource to transaction
        tx.enlistResource(xar);
        /*
         * *NOTE:*
         * The following block of code contains the first key concept of the
         * "Multiple Applications, Consecutive Calls" Pattern:
         *
         * if the program is running with the role of "superior", it starts a
         * new global transaction
         *
         * if the program is running with the role of "subordinate", it reads
         * from file the XID (Transaction ID) saved by another program executed
         * with the role of "superior" and then it resumes the same global
         * transaction.
         */
        if (superior) {
            // Start a new XA global transaction with a single branch
            tx.start();
        } else {
            string xidString;
            // Open, read and close the file to retrieve xidString from
            // superior
            ifstream xidFile(xid_filename);
            if (!xidFile) {
                cerr << "Unable to open file '" << xid_filename << "'" << endl;
                return 1;
            }
            xidFile >> xidString;
            xidFile.close();
            cout << "XID='" << xidString << "' has been read from file '" <<
                xid_filename << "'" << endl;
            // Resume the global transaction started by a superior program
            tx.resume(xidString);
        }
        /*
         * This part is boilerplate code related to Oracle OCI for XA: no
         * specific XTA logic here
         */
        /* retrieve environment and context */
        if (NULL == (oci_env = xaoEnv(NULL))) {
            fprintf(stderr, "xaoEnv returned a NULL pointer\n");
            return 1;
        }
        if (NULL == (oci_svc_ctx = xaoSvcCtx(NULL))) {
            fprintf(stderr, "xaoSvcCtx returned a NULL pointer\n");
            return 1;
        }
        /* allocate statement and error handles */
        if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_stmt_hndl,
                                 OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
            fprintf(stderr, "Unable to allocate OCI statement handle\n");
            return 1;
        }
        if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_err_hndl,
                                 OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
            fprintf(stderr, "Unable to allocate OCI error handle\n");
            return 1;
        }
        /*
         * Prepare and execute of a SQL statement with Oracle DBMS using OCI
         */
        if (OCI_SUCCESS != OCIStmtPrepare(
                oci_stmt_hndl, oci_err_hndl, oci_stmt,
                (ub4) strlen((char *)oci_stmt),
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
            fprintf(stderr, "Unable to prepare >%s< OCI statement for "
                    "execution\n", oci_stmt);
            return 1;
        }
        rc = OCIStmtExecute(
            oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
            (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
            (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != rc && OCI_SUCCESS_WITH_INFO != rc) {
            fprintf(stderr, "Error while executing >%s< statement; rc = %d\n",
                    oci_stmt, rc);
            return 1;
        }
        printf("OCI statement >%s< completed\n", (char *)oci_stmt);
        /*
         * Free allocated OCI handles to avoid memory leaks
         */
        OCIHandleFree((dvoid *)oci_stmt_hndl, (ub4)OCI_HTYPE_STMT);
        OCIHandleFree((dvoid *)oci_err_hndl, (ub4)OCI_HTYPE_ERROR);
        /*
         * *NOTE:*
         * The following block of code contains the second key concept of the
         * "Multiple Applications, Consecutive Calls" Pattern:
         *
         * if the program is running with the role of "superior", it suspends
         * the global transaction and it passes the XID (Transaction ID) in a
         * file (fifo) for future reading (subordinate application program
         * will read it)
         *
         * if the program is running with the role of "subordinate", it commits
         * or rollback the global transaction.
         */
        if (superior) {
            /*
             * Suspend the XA global transaction
             */
            tx.suspend();
            /*
             * Retrieve the Transaction ID (XID) associated to the transaction
             * that has been created in the previous step
             */
            string xidString = tx.getXid().toString();
            /*
             * Open, write and close the file to pass xidString from superior
             * to subordinate
             */
            ofstream xidFile(xid_filename);
            if (!xidFile) {
                cerr << "Unable to open file '" << xid_filename << "'" << endl;
                return 1;
            }
            xidFile << xidString;
            xidFile.close();
            cout << "XID='" << xidString << "' has been written to file '" <<
                xid_filename << "'" << endl;
        } else {
            if (commit) {
                // Commit the global transaction
                tx.commit();
            } else {
                // Rollback the global transaction
                tx.rollback();
            }
        }
        // Delete Transaction Manager object
        delete tm;
        // Delete MySQL native and XA resource
        delete xar;
                
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
