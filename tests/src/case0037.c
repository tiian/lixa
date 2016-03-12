/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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

/* TX (Transaction Demarcation) header */
#include <tx.h>

/* LIXA help library for PostgreSQL */
#include <lixapq.h>



static void exit_nicely(PGconn *conn)
{
    PQfinish(conn);
    exit(1);
}



int main(int argc, char *argv[])
{
    /* generic variables */
    int         txrc;
    /* PostgreSQL variables */
    PGconn     *conn;
    PGresult   *res;
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
    conn = lixa_pq_get_conn();
    if (NULL == conn) {
        fprintf(stderr, "lixa_pq_get_conn: conn is NULL\n");
        exit(1);
    }

    /* trivial check for lixa_pq_is_managed_conn() function */
    if (!lixa_pq_is_managed_conn(conn)) {
        fprintf(stderr, "lixa_pq_is_managed_conn: returned FALSE, this "
                "should be impossible!\n");
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
            conn, "INSERT INTO authors VALUES(9, 'Name', 'Surname');");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            fprintf(stderr,
                    "INSERT INTO authors: %s", PQerrorMessage(conn));
            PQclear(res);
            exit_nicely(conn);
        }
        PQclear(res);
    } else {
        res = PQexec(
            conn, "DELETE FROM authors;");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            fprintf(stderr,
                    "DELETE FROM authors: %s", PQerrorMessage(conn));
            PQclear(res);
            exit_nicely(conn);
        }
        PQclear(res);
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
