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
 * "Single Application" Pattern
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
    /* native MySQL connection handler */
    MYSQL                        *rm = NULL;
    /* variable for MySQL statement to execute */
    char                         *mysql_stmt;
    /* variable for a second MySQL statement to execute */
    char                         *mysql_stmt2;
    /* XTA Resource for MySQL */
    xta_mysql_xa_resource_t      *xar = NULL;
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
    if (insert) {
        mysql_stmt = "INSERT INTO authors VALUES(1919, 'Levi', 'Primo')";
        mysql_stmt2 = "INSERT INTO authors VALUES(1898, 'Remarque', "
            "'Erich Maria')";
    } else {
        mysql_stmt = "DELETE FROM authors WHERE id=1919";
        mysql_stmt2 = "DELETE FROM authors WHERE id=1898";
    }
    /*
     * initialize XTA environment
     */
    xta_init();
    /*
     * create a new MySQL connection (if subordinate Application Program)
     */
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
    /*
     * create a new XTA Transaction Manager object
     */
    tm = xta_transaction_manager_new();
    if (tm == NULL) {
        fprintf(stderr, "xta_transaction_manager_new: returned NULL\n");
        return 1;
    }
    /*
     * create an XA resource for MySQL
     */
    xar = xta_mysql_xa_resource_new(rm, "MySQL", "localhost,0,lixa,,lixa");
    if (xar == NULL) {
        fprintf(stderr, "xta_mysql_xa_resource_new: returned NULL\n");
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
     * Enlist MySQL resource to Transaction
     */
    rc = xta_transaction_enlist_resource(tx, (xta_xa_resource_t *)xar);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_enlist_resource returned %d (%s) for "
               "MySQL XA resource\n", rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * Open all resources enlisted by the Transaction
     */
    rc = xta_transaction_open(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_open: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * Start a new XA global transaction with a single branch
     * Note: second argument ("multiple_branch") has TRUE value because the
     *       transaction will be branched by the subordinate Application
     *       Program
     */
    rc = xta_transaction_start(tx, TRUE);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_start: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
    /*
     * Execute MySQL statement
     */
    printf("MySQL, executing >%s<\n", mysql_stmt);
    if (mysql_query(rm, mysql_stmt)) {
        fprintf(stderr, "MySQL, error while executing >%s<: %u/%s\n",
                mysql_stmt, mysql_errno(rm), mysql_error(rm));
        mysql_close(rm);
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
     * *** NOTE: ***
     * a Remote Procedure Call (RPC) or a Web Service (WS) or a REST API is
     * emulated by a synchronous message passing using a named pipe (FIFO)
     */    
    /* open the pipe for write operation */
    sup2sub_fifo = fopen(sup2sub_fifoname, "w");
    if (sup2sub_fifo == NULL) {
        fprintf(stderr, "fopen error for fifo '%s'\n", sup2sub_fifoname);
        return 1;
    }
    /* write the message */
    if (0 > fprintf(sup2sub_fifo, "%s", xid_string)) {
        fprintf(stderr, "fprintf error while writing '%s' to fifo '%s'\n",
                xid_string, sup2sub_fifoname);
        return 1;
    }
    printf("Superior AP has sent XID '%s' to subordinate AP\n", xid_string);
    /* close the pipe */
    fclose(sup2sub_fifo);
    /* release the memory allocated for xid_string */
    free(xid_string);
    xid_string = NULL;

    /* open the pipe for read operation */
    sub2sup_fifo = fopen(sub2sup_fifoname, "r");
    if (sub2sup_fifo == NULL) {
        fprintf(stderr, "fopen error for fifo '%s'\n", sub2sup_fifoname);
        return 1;
    }
    /* read the message */
    if (NULL == fgets(fifo_buffer, sizeof(fifo_buffer),
                      sub2sup_fifo)) {
        fprintf(stderr, "fgets error while retrieving message from "
                "fifo '%s'\n", sub2sup_fifoname);
        return 1;
    }
    printf("Superior AP has received '%s' reply from subordinate AP\n",
           fifo_buffer);
    /* close the pipe */
    fclose(sub2sup_fifo);
    /*
     * Execute another MySQL statement
     */
    printf("MySQL, executing >%s<\n", mysql_stmt2);
    if (mysql_query(rm, mysql_stmt2)) {
        fprintf(stderr, "MySQL, error while executing >%s<: %u/%s\n",
                mysql_stmt2, mysql_errno(rm), mysql_error(rm));
        mysql_close(rm);
        return 1;
    }
    /*
     * *** NOTE: ***
     * at this point the RPC/WS/REST API emulation terminates: the subordinate
     * Application Program has been called and this (superior) Application
     * Program can go on with the branch created by xta_transaction_start
     */    
    /*
     * commit or rollback the transaction
     */
    if (commit) {
        /* Note: commit is performed with "non_block" flag set to FALSE: this
         * is necessary to synchronize with the subordinate branch */
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "xta_transaction_commit: returned %d (%s)\n",
                    rc, lixa_strerror(rc));
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
     * Close all resources enlisted by the Transaction
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
     * Delete MySQL native and XA resource
     */
    xta_mysql_xa_resource_delete(xar);
    mysql_close(rm);
    
    return 0;
}
