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
 * This program is an example implementation of the "Single Application Program
 * Pattern" as documented in LIXA manual:
 * http://www.tiian.org/lixa/manuals/html/index.html
 */



/*
 * This header is necessary for all the XTA related definitions
 */
#include <xta/xta.h>



int main(int argc, char *argv[])
{
    /* native PostgreSQL connection handler */
    PGconn                       *rm1 = NULL;
    /* native MySQL connection handler */
    MYSQL                        *rm2 = NULL;
    /* XTA Transaction Manager object reference */
    xta_transaction_manager_t    *tm = NULL;
    /* XTA Resource for PostgreSQL */
    xta_postgresql_xa_resource_t *xar1 = NULL;
    /* XTA Resource for MySQL */
    xta_mysql_xa_resource_t      *xar2 = NULL;
    /* XTA Transaction object reference */
    xta_transaction_t            *tx = NULL;
    /*
     * initialize XTA environment
     */
    xta_init();
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
    xar1 = xta_postgresql_xa_resource_new(rm1, "PostgreSQL", "dbname=testdb");
    if (xar1 == NULL) {
        fprintf(stderr, "xta_postgresql_xa_resource_new: returned NULL\n");
        return 1;
    }
    /*
     * create an XA resource for MySQL
     */
    xar2 = xta_mysql_xa_resource_new(rm2, "MySQL", "localhost,0,lixa,,lixa");
    if (xar2 == NULL) {
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

    
    return 0;
}
