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
    xta_native_xa_resource_t *native_xa_res;
#ifdef HAVE_MYSQL
    MYSQL *mysql_conn = NULL;
    xta_mysql_xa_resource_t *mysql_xa_res;
#endif
#ifdef HAVE_POSTGRESQL
    PGconn *postgres_conn = NULL;
    xta_postgresql_xa_resource_t *postgresql_xa_res;
#endif

    printf("%s| starting...\n", pgm);
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
    if (NULL == (native_xa_res = xta_native_xa_resource_new(0, NULL, NULL))) {
        printf("%s| xta_native_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#ifdef HAVE_MYSQL
    /*
     * create a MySQL XA resource object
     */
    if (NULL == (mysql_xa_res = xta_mysql_xa_resource_new(
                     mysql_conn, "MySQL", "ip,port,user,password"))) {
        printf("%s| xta_mysql_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#endif
#ifdef HAVE_POSTGRESQL
    /*
     * create a PostgreSQL XA resource object
     */
    if (NULL == (postgresql_xa_res = xta_postgresql_xa_resource_new(
                     postgres_conn, "PostgreSQL",
                     "ip,port,user,password"))) {
        printf("%s| xta_postgresql_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#endif
    /*
     * some code here ... @@@
     */
#ifdef HAVE_POSTGRESQL
    /*
     * delete the PostgreSQL XA resource object
     */
    xta_postgresql_xa_resource_delete(postgresql_xa_res);
#endif
#ifdef HAVE_MYSQL
    /*
     * delete the MySQL XA resource object
     */
    xta_mysql_xa_resource_delete(mysql_xa_res);
#endif    
    /*
     * delete native XA Resource object
     */
    xta_native_xa_resource_delete(native_xa_res);
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    
    /*
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    if (TX_ERROR == rc) {
        printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
        lixa_monkeyrm_call_cleanup();
    }
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(&info));
    assert(1 == rc);
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    lixa_monkeyrm_call_cleanup();
    */
    printf("%s| ...finished\n", pgm);
    return 0;
}
