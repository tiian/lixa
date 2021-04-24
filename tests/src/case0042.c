/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <stdio.h>
#include <stdlib.h>

/* PostgreSQL */
#include <libpq-fe.h>

/* MySQL */
#include <mysql.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>

/* LIXA help library for PostgreSQL */
#include <lixapq.h>

/* LIXA help library for MySQL */
#include <lixamy.h>



void exit_nicely(PGconn *pq_conn, MYSQL *my_conn)
{
    PQfinish(pq_conn);
    mysql_close(my_conn);
    exit(1);
}



int main(int argc, char *argv[])
{
    /* generic variables */
    int         txrc;
    /* PostgreSQL variables */
    PGconn     *pq_conn;
    PGresult   *res;
    /* MySQL variables */
    MYSQL      *my_conn;
    /* control variables */
    int         commit;
    int         insert;
    int         test_rc;
    
    if (argc < 4) {
        fprintf(stderr, "%s: at least two options must be specified\n",
                argv[0]);
        exit (1);
    }
    commit = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    test_rc = strtol(argv[3], NULL, 0);

    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* retrieve the connection handler */
    pq_conn = lixa_pq_get_conn();
    if (NULL == pq_conn) {
        fprintf(stderr, "lixa_pq_get_conn: conn is NULL\n");
        exit(1);
    }
    
    /* trivial check for lixa_pq_is_managed_conn() function */
    if (!lixa_pq_is_managed_conn(pq_conn)) {
        fprintf(stderr, "lixa_pq_is_managed_conn: returned FALSE, this "
                "should be impossible!\n");
        exit(1);
    }
    
    /* retrieve the connection handler */
    my_conn = lixa_my_get_conn();
    if (NULL == my_conn) {
        fprintf(stderr, "lixa_my_get_conn: conn is NULL\n");
        exit(1);
    }
    
    /* start a new transaction */
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }

    /* insert data */
    if (insert) {
        res = PQexec(
            pq_conn, "INSERT INTO authors VALUES(9, 'Name', 'Surname');");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            fprintf(stderr,
                    "INSERT INTO authors: %s", PQerrorMessage(pq_conn));
            PQclear(res);
            exit_nicely(pq_conn, my_conn);
        }
        PQclear(res);
        if (mysql_query(my_conn,
                        "INSERT INTO authors VALUES(9, 'Name', 'Surname')")) {
            fprintf(stderr, "INSERT INTO authors: %u/%s",
                    mysql_errno(my_conn), mysql_error(my_conn));
            exit_nicely(pq_conn, my_conn);
        }
    } else {
        res = PQexec(
            pq_conn, "DELETE FROM authors;");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            fprintf(stderr,
                    "DELETE FROM authors: %s", PQerrorMessage(pq_conn));
            PQclear(res);
            exit_nicely(pq_conn, my_conn);
        }
        PQclear(res);
        if (mysql_query(my_conn, "DELETE FROM authors")) {
            fprintf(stderr, "DELETE FROM  authors: %u/%s",
                    mysql_errno(my_conn), mysql_error(my_conn));
            exit_nicely(pq_conn, my_conn);
        }
    }
    
    /* commit transaction */
    if (commit) {
        if (test_rc != (txrc = tx_commit())) {
            fprintf(stderr, "tx_commit error: %d instead of %d\n",
                    txrc, test_rc);
            exit(txrc);
        }
    } else {
        if (test_rc != (txrc = tx_rollback())) {
            fprintf(stderr, "tx_rollback error: %d instead of %d\n",
                    txrc, test_rc);
            exit(txrc);
        }
    }

    /* close the resource manager */
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }

    return 0;
}


