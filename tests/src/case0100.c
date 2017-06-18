/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <config.h>



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include "xta/xta.h"



/*
 * Trivial case test for XTA: just assuring it can be compiled and executed!
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    xta_transaction_manager_t *tm;
    xta_transaction_t *tx;
    xta_native_xa_resource_t *native_xa_res;
    xta_native_xa_resource_t *dynamic_native_xa_res;
#ifdef HAVE_MYSQL
    MYSQL *mysql_conn = NULL;
    xta_mysql_xa_resource_t *mysql_xa_res;
#endif
#ifdef HAVE_POSTGRESQL
    PGconn *postgres_conn = NULL;
    xta_postgresql_xa_resource_t *postgresql_xa_res;
#endif

    /* turn ON trace for debugging purpose */
    xta_init();
    
    printf("%s| starting...\n", pgm);
    /*
     * dynamically create an XA native resource object
     */
    if (NULL == (dynamic_native_xa_res = xta_native_xa_resource_new(
                     "OracleIC_stareg",
                     "/opt/lixa/lib/switch_oracle_stareg.so",
                     "ORACLE_XA+Acc=P/hr/hr+SesTm=30+LogDir=/tmp+"
                     "threads=true+DbgFl=7+SqlNet=lixa_ora_db+"
                     "Loose_Coupling=true", ""))) {
        printf("%s| xta_native_xa_resource_new: returned NULL for "
               "dynamically creted resource\n", pgm);
        return 1;
    }
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        printf("%s| xta_transaction_manager_new: returned NULL\n", pgm);
        return 1;
    }
    /*
     * create an XA native resource object linked to the first Resouce
     * Manager configured in LIXA profile
     */
    if (NULL == (native_xa_res = xta_native_xa_resource_new_by_rmid(
                     0, xta_transaction_manager_get_config()))) {
        printf("%s| xta_native_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#ifdef HAVE_MYSQL
    /*
     * create a MySQL native connection
     */
    if (NULL == (mysql_conn = mysql_init(NULL))) {
        printf("%s| mysql_init: returned NULL\n", pgm);
        return 1;
    }
    if (NULL == mysql_real_connect(mysql_conn, "localhost", "lixa", "",
                                   "lixa", 0, NULL, 0)) {
        printf("%s| mysql_real_connect: returned error: %u, %s\n",
               pgm, mysql_errno(mysql_conn), mysql_error(mysql_conn));
        return 1;
    }
    /*
     * create a MySQL XA resource object
     */
    if (NULL == (mysql_xa_res = xta_mysql_xa_resource_new(
                     mysql_conn, "MySQL", "localhost,0,lixa,,lixa"))) {
        printf("%s| xta_mysql_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#endif
#ifdef HAVE_POSTGRESQL
    /*
     * create a PostgreSQL native connection
     */
    postgres_conn = PQconnectdb("dbname=testdb");
    if (CONNECTION_OK != PQstatus(postgres_conn)) {
        printf("%s| PQconnectdb: returned error %s\n",
               pgm, PQerrorMessage(postgres_conn));
        PQfinish(postgres_conn);
        return 1;
    }
    /*
     * create a PostgreSQL XA resource object
     */
    if (NULL == (postgresql_xa_res = xta_postgresql_xa_resource_new(
                     postgres_conn, "PostgreSQL",
                     "dbname=testdb"))) {
        printf("%s| xta_postgresql_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#endif
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_begin: returned NULL\n", pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }
    /* (try to) create again a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_begin: returned NULL\n", pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }
    /* register the native XA Resource to the transaction manager: this step
     * is useless but it's not dangerous */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)native_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/native_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
    /* register the dynamic native XA Resource to the transaction manager */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)dynamic_native_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/dynamic_native_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
#ifdef HAVE_MYSQL
    /* register the MySQL XA Resource to the transaction manager */
    /*
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)mysql_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/mysql_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
    */
#endif
#ifdef HAVE_POSTGRESQL
    /* register the PostgreSQL XA Resource to the transaction manager */
    /*
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)postgresql_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/postgresql_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
    */
#endif

    /* open all the resources for Distributed Transactions */
    if (LIXA_RC_OK != (rc = xta_transaction_open(tx))) {
        printf("%s| xta_transaction_open: returned %d\n", pgm, rc);
        return 1;
    }

    /* start a Distributed Transaction */
    if (LIXA_RC_OK != (rc = xta_transaction_start(tx))) {
        printf("%s| xta_transaction_start: returned %d\n", pgm, rc);
        return 1;
    }

    
    /*
     * some code here ...
     */

    /* commit the Distributed Transaction */
    if (LIXA_RC_OK != (rc = xta_transaction_commit(tx))) {
        printf("%s| xta_transaction_commit: returned %d\n", pgm, rc);
        return 1;
    }
    
    /* close all the resources for Distributed Transactions */
    if (LIXA_RC_OK != (rc = xta_transaction_close(tx))) {
        printf("%s| xta_transaction_close: returned %d\n", pgm, rc);
        return 1;
    }
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
#ifdef HAVE_POSTGRESQL
    /*
     * delete the PostgreSQL XA resource object
     */
    xta_postgresql_xa_resource_delete(postgresql_xa_res);
    /*
     * close PostgreSQL database connection
     */
    PQfinish(postgres_conn);
#endif
#ifdef HAVE_MYSQL
    /*
     * delete the MySQL XA resource object
     */
    xta_mysql_xa_resource_delete(mysql_xa_res);
    /*
     * close MySQL database connection
     */
    mysql_close(mysql_conn);
#endif    
    /*
     * delete dynamically created native XA Resource object
     */
    xta_native_xa_resource_delete(dynamic_native_xa_res);
    /*
     * delete native XA Resource object
     */
    xta_native_xa_resource_delete(native_xa_res);
    /*
     * end of XTA API calls
     */
    printf("%s| ...finished\n", pgm);
    return 0;
}
