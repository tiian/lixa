/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
 * Demarcation) API and MySQL together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* UNIX standard headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    int         txrc, delete;
    /* MySQL variables */
    MYSQL      *conn;
    
    if (argc > 1 && (!strcmp(argv[1], "delete") || !strcmp(argv[1], "DELETE")))
        delete = 1;
    else
        delete = 0;
 
    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* 
     * Retrieve MySQL connection: don't use mysql_real_connect() function
     * because it would retrieve a connection to the DB that's NOT managed
     * by LIXA transaction manager
     */
    conn = lixa_my_get_conn();
    /*
     * These functions can be used when there are more than one MySQL
     * configured as a resource manager
     * conn = lixa_my_get_conn_by_rmid(0);
     * conn = lixa_my_get_conn_by_pos(0);
     */

    /* start a new transaction */
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }

    if (delete) {
        printf("Deleting a row from the table...\n");
        if (mysql_query(conn, "DELETE FROM authors")) {
            fprintf(stderr, "DELETE FROM  authors: %u/%s\n",
                    mysql_errno(conn), mysql_error(conn));
            exit_nicely(conn);
        }
    } else {
        printf("Inserting a row in the table...\n");
        if (mysql_query(conn,
                        "INSERT INTO authors VALUES(1, 'Foo', 'Bar')")) {
            fprintf(stderr, "INSERT INTO authors: %u/%s\n",
                    mysql_errno(conn), mysql_error(conn));
            exit_nicely(conn);
        }
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
