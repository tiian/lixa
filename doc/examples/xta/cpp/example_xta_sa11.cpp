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
 * This program accepts exactly two parameters on the command line:
 * first parameter:  "commit", boolean value (if FALSE, "rollback")
 * second parameter: "insert", boolean value (if FALSE, "delete")
 */



/*
 * This header is necessary for all the XTA related definitions
 */
#include <xta/cpp/Xta.hpp>



using namespace xta;



int main(int argc, char *argv[])
{
    /* LIXA / XTA return and reason code */
    int                           rc;
    /* First parameter: commit transaction? */
    int                           commit;
    /* Second parameter: insert data in databases? */
    int                           insert;
    /* native PostgreSQL connection handler */
    PGconn                       *rm1 = NULL;
    /* PostgreSQL result */
//    PGresult                     *pg_res;
    /* variable for PostgreSQL statement to execute */
    const char                   *postgresql_stmt;
    /* XTA Resource for PostgreSQL */
    PostgresqlXaResource         *xar1 = NULL;
    /* native MySQL connection handler */
    MYSQL                        *rm2 = NULL;
    /* variable for MySQL statement to execute */
    const char                   *mysql_stmt;
    /* XTA Resource for MySQL */
    MysqlXaResource              *xar2 = NULL;
    /* XTA Transaction Manager object reference */
    TransactionManager           *tm = NULL;
    /* XTA Transaction object reference */
    Transaction                  *tx = NULL;

    /*
     * Check command line parameters
     */
    if (argc < 3) {
        fprintf(stderr, "This program requires two boolean parameters: "
                "'commit' and 'insert'\n");
        return 1;
    }
    commit = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    /*
     * Prepare SQL statements in accordance with "insert" command line
     * parameter
     */
    if (insert) {
        postgresql_stmt = "INSERT INTO authors VALUES(1921, 'Rigoni Stern', "
            "'Mario')";
        mysql_stmt = "INSERT INTO authors VALUES(1919, 'Levi', 'Primo')";
    } else {
        postgresql_stmt = "DELETE FROM authors WHERE id=1921";
        mysql_stmt = "DELETE FROM authors WHERE id=1919";
    }

    /*
     * initialize XTA environment
     */
    Xta::Init();
    /*
     * create a new PostgreSQL connection
     */
    rm1 = PQconnectdb("dbname=testdb");
    if (PQstatus(rm1) != CONNECTION_OK) {
        fprintf(stderr, "PQconnectdb: returned error %s\n",
                PQerrorMessage(rm1));
        PQfinish(rm1);
        return 1;
    }
    /*
     * create a new MySQL connection
     */
    rm2 = mysql_init(NULL);
    if (rm2 == NULL) {
        fprintf(stderr, "mysql_init: returned NULL\n");
        return 1;
    }
    if (mysql_real_connect(rm2, "localhost", "lixa", "",
                           "lixa", 0, NULL, 0) == NULL) {
        fprintf(stderr, "mysql_real_connect: returned error: %u, %s\n",
                mysql_errno(rm2), mysql_error(rm2));
        return 1;
    }
    /*
     * create a new XTA Transaction Manager object
     */
    tm = new TransactionManager();
    if (tm == NULL) {
        fprintf(stderr, "xta::TransactionManager: returned NULL\n");
        return 1;
    }
    /*
     * create an XA resource for PostgreSQL
     * second parameter "PostgreSQL" is descriptive
     * third parameter "dbname=testdb" identifies the specific database
     */
    xar1 = new PostgresqlXaResource(rm1, "PostgreSQL", "dbname=testdb");
    if (xar1 == NULL) {
        fprintf(stderr, "xta_postgresql_xa_resource_new: returned NULL\n");
        return 1;
    }
    /*
     * create an XA resource for MySQL
     */
    xar2 = new MysqlXaResource(rm2, "MySQL", "localhost,0,lixa,,lixa");
    if (xar2 == NULL) {
        fprintf(stderr, "xta_mysql_xa_resource_new: returned NULL\n");
        return 1;
    }
    /*
     * Create a new XA global transaction
     */
    tx = tm->CreateTransaction();
    if (tx == NULL) {
        fprintf(stderr, "xta_transaction_manager_get_transaction: "
                "returned NULL\n");
        return 1;
    }
    /*
     * Enlist PostgreSQL resource to transaction
     */
/*
    rc = xta_transaction_enlist_resource(tx, (xta_xa_resource_t *)xar1);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_enlist_resource returned %d (%s) for "
               "PostgreSQL XA resource\n", rc, lixa_strerror(rc));
        return 1;
    }
*/
    /*
     * Enlist MySQL resource to Transaction
     */
/*
    rc = xta_transaction_enlist_resource(tx, (xta_xa_resource_t *)xar2);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_enlist_resource returned %d (%s) for "
               "MySQL XA resource\n", rc, lixa_strerror(rc));
        return 1;
    }
*/
    /*
     * Open all resources enlisted by the Transaction
     */
/*
    rc = xta_transaction_open(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_open: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
*/
    /*
     * Start a new XA global transaction with a single branch
     * Note: second argument ("multiple_branch") has FALSE value
     */
/*
    rc = xta_transaction_start(tx, FALSE);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_start: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
*/
    /*
     * At this point, it's time to do something with the Resource Managers
     * (PostgreSQL and MySQL)
     *
     * Execute PostgreSQL statement
     */
/*
    printf("PostgreSQL, executing >%s<\n", postgresql_stmt);
    pg_res = PQexec(rm1, postgresql_stmt);
    if (PQresultStatus(pg_res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "PostgreSQL, error while executing >%s<: %s\n",
                postgresql_stmt, PQerrorMessage(rm1));
        PQclear(pg_res);
        PQfinish(rm1);
        return 1;
    }
*/
    /*
     * Execute MySQL statement
     */
/*
    printf("MySQL, executing >%s<\n", mysql_stmt);
    if (mysql_query(rm2, mysql_stmt)) {
        fprintf(stderr, "MySQL, error while executing >%s<: %u/%s\n",
                mysql_stmt, mysql_errno(rm2), mysql_error(rm2));
        mysql_close(rm2);
        return 1;
    }
*/
    /*
     * commit or rollback the transaction
     */
/*
    if (commit) {
*/
        /* Note: second argument ("multiple_branch") has FALSE value */
/*
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
*/
    /*
     * Close all resources enlisted by the Transaction
     */
/*
    rc = xta_transaction_close(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "xta_transaction_close: returned %d (%s)\n",
                rc, lixa_strerror(rc));
        return 1;
    }
*/
    /*
     * Delete Transaction object
     */
    delete tx;
    
    /*
     * Delete Transaction Manager object
     */
    delete tm;
    /*
     * Delete PostgreSQL native and XA resource
     */
/*
    xta_postgresql_xa_resource_delete(xar1);
*/
    PQfinish(rm1);
    /*
     * Delete MySQL native and XA resource
     */
/*
    xta_mysql_xa_resource_delete(xar2);
*/
    mysql_close(rm2);
    
    return 0;
}
