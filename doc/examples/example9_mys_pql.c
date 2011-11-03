/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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
 * This is an example that shows as you can use LIXA TX (Transaction
 * Demarcation) API, MySQL and PostgreSQL together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* UNIX standard headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* MySQL */
#include <mysql.h>

/* PostgreSQL */
#include <libpq-fe.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>

/* LIXA help library for MySQL */
#include <lixamy.h>

/* LIXA help library for PostgreSQL */
#include <lixapq.h>



static void exit_nicely(MYSQL *conn_my, PGconn *conn_pq)
{
    mysql_close(conn_my);
    PQfinish(conn_pq);
    exit(1);
}



int main(int argc, char *argv[])
{
    /* generic variables */
    int         txrc, delete;
    /* MySQL variables */
    MYSQL      *conn_my;
    /* PostgreSQL variables */
    PGconn     *conn_pq;
    PGresult   *res;
    
    if (argc > 1 && (!strcmp(argv[1], "delete") || !strcmp(argv[1], "DELETE")))
        delete = 1;
    else
        delete = 0;
 
    /* open the resource managers */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* retrieve MySQL connection */
    conn_my = lixa_my_get_conn();
    /*
     * These functions can be used when there are more than one PostgreSQL
     * configured as a resource manager
     * conn_my = lixa_my_get_conn_by_rmid(0);
     */

    /* retrieve PostgreSQL connection */
    conn_pq = lixa_pq_get_conn();
    /*
     * These functions can be used when there are more than one PostgreSQL
     * configured as a resource manager
     * conn_pq = lixa_pq_get_conn_by_rmid(0);
     */

    /* start a new transaction */
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }

    if (delete) {
        printf("Deleting a row from MySQL  table...\n");
        if (mysql_query(conn_my, "DELETE FROM authors")) {
            fprintf(stderr, "DELETE FROM  authors: %u/%s\n",
                    mysql_errno(conn_my), mysql_error(conn_my));
            exit_nicely(conn_my, conn_pq);
        }
        printf("Deleting a row from PostgreSQL table...\n");
        res = PQexec(conn_pq,
                     "DELETE FROM authors WHERE id=1;");
        if (PGRES_COMMAND_OK != PQresultStatus(res))
            {
                fprintf(stderr, "DELETE FROM authors: %s",
                        PQerrorMessage(conn_pq));
                PQclear(res);
                exit_nicely(conn_my, conn_pq);
            }
        PQclear(res);
    } else {
        printf("Inserting a row in MySQL table...\n");
        if (mysql_query(conn_my,
                        "INSERT INTO authors VALUES(1, 'Foo', 'Bar')")) {
            fprintf(stderr, "INSERT INTO authors: %u/%s\n",
                    mysql_errno(conn_my), mysql_error(conn_my));
            exit_nicely(conn_my, conn_pq);
        }
        printf("Inserting a row in PostgreSQL table...\n");
        res = PQexec(conn_pq,
                     "INSERT INTO authors VALUES(1, 'Foo', 'Bar');");
        if (PGRES_COMMAND_OK != PQresultStatus(res))
            {
                fprintf(stderr, "INSERT INTO authors: %s",
                        PQerrorMessage(conn_pq));
                PQclear(res);
                exit_nicely(conn_my, conn_pq);
            }
        PQclear(res);
    }
    
    if (TX_OK != (txrc = tx_commit())) {
        fprintf(stderr, "tx_commit error: %d\n", txrc);
        exit(txrc);
    }
    /*
     * This function can be used if you want to rollback instead of commit the
     * transaction
    if (TX_OK != (txrc = tx_rollback())) {
        fprintf(stderr, "tx_rollback error: %d\n", txrc);
        exit(txrc);
    }
    */

    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }

    return 0;
}


