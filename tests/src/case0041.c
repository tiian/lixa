/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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

/* MySQL */
#include <mysql.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>

/* LIXA help library for MySQL */
#include <lixamy.h>



static void exit_nicely(MYSQL *conn)
{
    mysql_close(conn);
    exit(1);
}



int main(int argc, char *argv[])
{
    /* generic variables */
    int         txrc;
    /* MySQL variables */
    MYSQL      *conn;
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
    conn = lixa_my_get_conn();
    if (NULL == conn) {
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
        if (mysql_query(conn,
                        "INSERT INTO authors VALUES(9, 'Name', 'Surname')")) {
            fprintf(stderr, "INSERT INTO authors: %u/%s",
                    mysql_errno(conn), mysql_error(conn));
            exit_nicely(conn);
        }
    } else {
        if (mysql_query(conn, "DELETE FROM authors")) {
            fprintf(stderr, "DELETE FROM  authors: %u/%s",
                    mysql_errno(conn), mysql_error(conn));
            exit_nicely(conn);
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
