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
 * the purpose of this small program is not to explain C development
 * techniques or good style, but simply to show XTA for C using the easiest
 * approach.
 */ 



/*
 * This header is necessary for Oracle C Interface
 */
#include <oci.h>



/*
 * This header is necessary for all the XTA related definitions
 */
#include <xta/xta.h>



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
    xta_native_xa_resource_t     *xar = NULL;
    /* Variable for Oracle SQL statement that must be executed */
    text                         *oci_stmt;
    /* XTA Transaction Manager object reference */
    xta_transaction_manager_t    *tm = NULL;
    /* XTA Transaction object reference */
    xta_transaction_t            *tx = NULL;
    /* XID (Transaction ID) as a null terminated C string */
    char                         *xid_string = NULL;
    /* Name of the file that must be used to pass XID from superior to
     * subordinate program */
    const char                   *xid_filename = NULL;
    /* File that must be used to pass XID from superior to
     * subordinate program */
    FILE                         *xid_file = NULL;
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
    /*
     * Prepare SQL statement in accordance with "insert" and "superior" command
     * line parameter
     */
    if (insert) {
        /* SQL INSERT */
        if (superior)
            oci_stmt = (text *)
                "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) "
                "VALUES(1930, 'Bonatti', 'Walter')";
        else
            oci_stmt = (text *)
                "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) "
                "VALUES(1948, 'Casarotto', 'Renato')";
    } else {
        /* SQL DELETE */
        if (superior)
            oci_stmt = (text *) "DELETE FROM authors WHERE ID=1930";
        else
            oci_stmt = (text *) "DELETE FROM authors WHERE ID=1948";
    }

    /*
     * initialize XTA environment
     */
    xta_init();
    /*
     * dynamically create a native XA resource for Oracle DBMS
     */
    xar = xta_native_xa_resource_new(
        "OracleIC_stareg",
        "/opt/lixa/lib/switch_oracle_stareg.so",
        "ORACLE_XA+Acc=P/hr/hr+SesTm=30+LogDir=/tmp+"
        "threads=true+DbgFl=7+SqlNet=lixa_ora_db+"
        "Loose_Coupling=true", "");
    if (xar == NULL) {
        fprintf(stderr, "xta_native_xa_resource_new: returned "
                "NULL for dynamically created resource\n");
        return 1;
    }
    /*
     * create a new XTA Transaction Manager object
     */
    tm = xta_transaction_manager_new();
    if (tm == NULL) {
        fprintf(stderr, "xta_transaction_manager_new: returned NULL\n");
        return 1;
    }    
    /*
     * Create a new XA global transaction
     */
    tx = xta_transaction_manager_create_transaction(tm);
    if (tx == NULL) {
        fprintf(stderr, "xta_transaction_manager_get_transaction: "
                "returned NULL\n");
        return 1;
    }
    /*
     * Enlist Oracle DMBS resource to transaction
     */
    rc = xta_transaction_enlist_resource(tx, (xta_xa_resource_t *)xar);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_enlist_resource returned %d (%s) for "
               "PostgreSQL XA resource\n", rc, lixa_strerror(rc));
        return 1;
    }
    /* @@@
     * Open all resources enlisted by tx Transaction
    rc = xta_transaction_open(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_open: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
     */
    /*
     * *NOTE:*
     * The following block of code contains the first key concept of the
     * "Multiple Applications, Consecutive Calls" Pattern:
     *
     * if the program is running with the role of "superior", it starts a new
     * global transaction
     *
     * if the program is running with the role of "subordinate", it reads from
     * file the XID (Transaction ID) saved by another program executed with the
     * role of "superior" and then it resumes the same global transaction.
     */
    if (superior) {
        /*
         * Start a new XA global transaction with a single branch
         * Note: second argument ("multiple_branch") has FALSE value
         */
        rc = xta_transaction_start(tx, FALSE);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "xta_transaction_start: returned %d (%s)\n",
                    rc, lixa_strerror(rc));
            return 1;
        }
    } else {
        char buffer[200];
        /*
         * Open, read and close the file to retrieve xid_string from superior
         */
        xid_file = fopen(xid_filename, "r");
        if (xid_file == NULL) {
            fprintf(stderr, "fopen(\"%s\", \"r\") returned NULL\n",
                    xid_filename);
            return 1;
        }
        if (NULL == fgets(buffer, sizeof(buffer), xid_file)) {
            fprintf(stderr, "Error while retrieving XID from file '%s'\n",
                    xid_filename);
            return 1;
        }
        fclose(xid_file);
        xid_file = NULL;
        printf("XID='%s' has been read from file '%s'\n",
               buffer, xid_filename);
        /*
         * Resume the global transaction started by a superior program
         */
        rc = xta_transaction_resume(tx, buffer, TMRESUME);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "xta_transaction_resume: returned %d (%s)\n",
                    rc, lixa_strerror(rc));
            return 1;
        }
    }
    /*
     * This part is boilerplate code related to Oracle OCI for XA: no specific
     * XTA logic here
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
     * if the program is running with the role of "superior", it suspends the
     * global transaction and it passes the XID (Transaction ID) a file for
     * future reading (subordinate application program will read it)
     *
     * if the program is running with the role of "subordinate", it commits
     * or rollback the global transaction.
     */
    if (superior) {
        /*
         * Suspend the XA global transaction
         */
        rc = xta_transaction_suspend(tx, TMMIGRATE);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "xta_transaction_suspend: returned %d (%s)\n",
                    rc, lixa_strerror(rc));
            return 1;
        }
        /*
         * Retrieve the Transaction ID (XID) associated to the transaction that
         * has been created in the previous step
         */
        xid_string = xta_xid_to_string(xta_transaction_get_xid(tx));
        if (xid_string == NULL) {
            fprintf(stderr, "xta_transaction_get_xid returned NULL\n");
            return 1;
        }
        /*
         * Open, write and close the file to pass xid_string from superior to
         * subordinate
         */
        xid_file = fopen(xid_filename, "w");
        if (xid_file == NULL) {
            fprintf(stderr, "fopen(\"%s\", \"w\") returned NULL\n",
                    xid_filename);
            return 1;
        }
        fprintf(xid_file, "%s", xid_string);
        fclose(xid_file);
        xid_file = NULL;
        printf("XID='%s' has been written in file '%s'\n",
               xid_string, xid_filename);
        /*
         * Release xid_string to avoid a memory leak
         */
        free(xid_string);
        xid_string = NULL;
    } else {
        if (commit) {
            /*
             * Commit the global transaction
             * Note: second argument ("multiple_branch") has FALSE value
             */
            rc = xta_transaction_commit(tx, FALSE);
            if (rc != LIXA_RC_OK) {
                fprintf(stderr, "xta_transaction_commit: returned %d (%s)\n",
                        rc, lixa_strerror(rc));
                return 1;
            }
        } else {
            /*
             * Rollback the global transaction
             */
            rc = xta_transaction_rollback(tx);
            if (rc != LIXA_RC_OK) {
                fprintf(stderr, "xta_transaction_rollback: returned %d (%s)\n",
                        rc, lixa_strerror(rc));
                return 1;
            }
        }
    }
    /* @@@
     * Close all resources enlisted by tx Transaction
    rc = xta_transaction_close(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_close: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
     */
    /*
     * Delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    /*
     * delete dynamically created native XA Resource object for Oracle
     */
    xta_native_xa_resource_delete(xar);
    
    return 0;
}
