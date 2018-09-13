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
 */



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
    /* Third parameter: name of the named pipe (FIFO) that must be used to
       send from superior AP to subordinate AP */
    const char                   *sup2sub_fifoname = NULL;
    /* Fourth parameter: name of the named pipe (FIFO) that must be used to
       send from subordinate AP to superior AP */
    const char                   *sub2sup_fifoname = NULL;
    /* File (named pipe/FIFO) that must be used to pass XID from superior to
       subordinate application program */
    FILE                         *sup2sub_fifo = NULL;
    /* File (named pipe/FIFO) that must be used to return the result from
       subordinate to superior application program */
    FILE                         *sub2sup_fifo = NULL;
    /* native PostgreSQL connection handler */
    PGconn                       *rm = NULL;
    /* PostgreSQL result */
    PGresult                     *pg_res;
    /* variable for PostgreSQL statement to execute */
    char                         *postgresql_stmt;
    /* XTA Resource for PostgreSQL */
    xta_postgresql_xa_resource_t *xar = NULL;
    /* XTA Transaction Manager object reference */
    xta_transaction_manager_t    *tm = NULL;
    /* XTA Transaction object reference */
    xta_transaction_t            *tx = NULL;
    /* XID (Transaction ID) as a null terminated C string */
    char                         *xid_string = NULL;
    /* a buffer to read the reply from the subordinate Application Program */
    char                          fifo_buffer[100];

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
    /*
     * initialize XTA environment
     */
    xta_init();
    /*
     * create a new PostgreSQL connection (if superior Application Program)
     */
    rm = PQconnectdb("dbname=testdb");
    if (PQstatus(rm) != CONNECTION_OK) {
        fprintf(stderr, "PQconnectdb: returned error %s\n",
                PQerrorMessage(rm));
        PQfinish(rm);
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
     * create an XA resource for PostgreSQL
     * second parameter "PostgreSQL" is descriptive
     * third parameter "dbname=testdb" identifies the specific database
     */
    xar = xta_postgresql_xa_resource_new(rm, "PostgreSQL", "dbname=testdb");
    if (xar == NULL) {
        fprintf(stderr, "xta_postgresql_xa_resource_new: returned NULL\n");
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
     * Enlist PostgreSQL resource to transaction
     */
    rc = xta_transaction_enlist_resource(tx, (xta_xa_resource_t *)xar);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_enlist_resource returned %d (%s) for "
               "PostgreSQL XA resource\n", rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * Open all resources enlisted by tx Transaction
     */
    rc = xta_transaction_open(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_open: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * *** NOTE: ***
     * at this point, subordinate Application Program must wait a Remote
     * Procedure Call (RPC) or a Web Service (WS) or a REST API invocation from
     * superior Application Program. Here the incoming call is emulated with
     * a synchronous message passing using a named pipe (FIFO)
     */
    /* open the pipe for read operation */
    sup2sub_fifo = fopen(sup2sub_fifoname, "r");
    if (sup2sub_fifo == NULL) {
        fprintf(stderr, "fopen error for fifo '%s'\n", sup2sub_fifoname);
        return 1;
    }
    /* read the message */
    if (NULL == fgets(fifo_buffer, sizeof(fifo_buffer),
                      sup2sub_fifo)) {
        fprintf(stderr, "fgets error while retrieving message from "
                "fifo '%s'\n", sup2sub_fifoname);
        return 1;
    }
    printf("Subordinate AP has received XID '%s' from superior AP\n",
           fifo_buffer);
    /* close the pipe */
    fclose(sup2sub_fifo);
    /*
     * *** NOTE: ***
     * at this point the subordinate Application Program (this one) has been
     * called by the superior A.P. that's waiting a reply. Now this
     * program can branch the global transaction previously started by the
     * superior A.P.
     */
    /*
     * create a new branch in the same global transaction
     */
    rc = xta_transaction_branch(tx, fifo_buffer);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_branch returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * the branch has the same global identifier, but a different branch id;
     * the following statement is for the sake of debugging only
     */
    xid_string = xta_xid_to_string(xta_transaction_get_xid(tx));
    if (xid_string == NULL) {
        fprintf(stderr, "xta_transaction_get_xid returned NULL\n");
        return 1;
    } else {
        printf("Subordinate AP has created a branch with XID '%s'\n",
               xid_string);
        /* release the memory allocated for xid_string */
        free(xid_string);
        xid_string = NULL;
    }
    /*
     * Execute PostgreSQL statement
     */
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
    /*
     * commit or rollback the transaction
     */
    if (commit) {
        /*
         * *** NOTE: ***
         * commit MUST be performed in two step:
         * 1. in first step, the branch is only "prepared" (as in XA
         *    specification) and control can be returned to the superior AP
         *    that has to start its commit
         * 2. in the second step, the branch is definitely "committed", but
         *    the operation will block the caller because the subordinate AP
         *    must wait the "prepared" state of the superior AP before
         *    committing
         */
        /* commit is performed with "non_blocking" flag set to TRUE: this is
           necessary to allow the superior branch to commit */
        rc = xta_transaction_commit(tx, TRUE);
        if (rc != LIXA_RC_WOULD_BLOCK) {
            fprintf(stderr, "xta_transaction_rollback: returned %d (%s) "
                    "instead of %d (%s)\n",
                    rc, lixa_strerror(rc),
                    LIXA_RC_WOULD_BLOCK, lixa_strerror(LIXA_RC_WOULD_BLOCK));
            return 1;
        }
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "xta_transaction_rollback: returned %d (%s)\n",
                    rc, lixa_strerror(rc));
            return 1;
        }
    }
    /*
     * *** NOTE: ***
     * at this point the subordinate Application Program (this one) has to
     * reply to the superior A.P. that's waiting. Here the reply is emulated
     * with a synchronous message passing using a named pipe (FIFO)
     */
    /* open the pipe for write operation */
    sub2sup_fifo = fopen(sub2sup_fifoname, "w");
    if (sub2sup_fifo == NULL) {
        fprintf(stderr, "fopen error for fifo '%s'\n", sub2sup_fifoname);
        return 1;
    }
    /* prepare the reply message */
    if (commit)
        sprintf(fifo_buffer, "PREPARED_for_COMMIT");
    else
        sprintf(fifo_buffer, "ROLLBACK");
    /* write the message */
    if (0 > fprintf(sub2sup_fifo, "%s", fifo_buffer)) {
        fprintf(stderr, "fprintf error while writing '%s' to fifo '%s'\n",
                fifo_buffer, sub2sup_fifoname);
        return 1;
    }
    printf("Subordinate AP has returned '%s' to superior AP\n", fifo_buffer);
    /* close the pipe */
    fclose(sub2sup_fifo);
    
    if (commit) {
        /*
         * Complete the second phase of the commit with "non_blocking" flag set
         * to FALSE: this is necessary to wait the superior AP prepare phase
         */
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "xta_transaction_rollback: returned %d (%s)\n",
                    rc, lixa_strerror(rc));
            return 1;
        }
    }
    /*
     * Close all resources enlisted by tx Transaction
     */
    rc = xta_transaction_close(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_close: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * Delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    /*
     * Delete PostgreSQL native and XA resource
     */
    xta_postgresql_xa_resource_delete(xar);
    PQfinish(rm);
    
    return 0;
}
