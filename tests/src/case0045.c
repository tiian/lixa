/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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



/*
 * This case test is specifically designed to exploit bug # 3512953
 * http://sourceforge.net/tracker/?func=detail&aid=3512953&group_id=257602&atid=1233231
 * It must be executed after
 * case0041 1 0 0 (commit + delete)
 * because the table must be empty
 */
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
    MYSQL_RES  *result;
    MYSQL_ROW   row;
    long        count;
    
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
    fprintf(stderr,"Starting a new transaction...\n");
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }
    /* insert data */
    fprintf(stderr,"Inserting one row...\n");
    if (mysql_query(conn,
                    "INSERT INTO authors VALUES(9, 'Name', 'Surname')")) {
        fprintf(stderr, "INSERT INTO authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    /* check there's exactly one row */
    fprintf(stderr,"Checking there's exactly one row...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (1 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 1\n",
                count);
        exit_nicely(conn);
    }
    /* commit transaction */
    fprintf(stderr,"Committing...\n");
    if (TX_OK != (txrc = tx_commit())) {
        fprintf(stderr, "tx_commit error: %d\n", txrc);
        exit(txrc);
    }
    /* check there's exactly one row */
    fprintf(stderr,"Checking there's exactly one row...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (1 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 1\n",
                count);
        exit_nicely(conn);
    }
    /* start a new transaction */
    fprintf(stderr,"Starting a new transaction...\n");
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }
    /* removing row */
    if (mysql_query(conn, "DELETE FROM authors")) {
        fprintf(stderr, "DELETE FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    /* check there are no rows */
    fprintf(stderr,"Checking there are no rows...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (0 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 0\n",
                count);
        exit_nicely(conn);
    }
    /* backing out, the row must appear again... */
    fprintf(stderr,"Backing out...\n");
    if (TX_OK != (txrc = tx_rollback())) {
        fprintf(stderr, "tx_rollback error: %d\n", txrc);
        exit(txrc);
    }
    /* check there's exactly one row */
    fprintf(stderr,"Checking there's exactly one row...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (1 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 1\n",
                count);
        exit_nicely(conn);
    }
    /* start a new transaction */
    fprintf(stderr,"Starting a new transaction...\n");
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }
    /* removing row */
    if (mysql_query(conn, "DELETE FROM authors")) {
        fprintf(stderr, "DELETE FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    /* check there are no rows */
    fprintf(stderr,"Checking there are no rows...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (0 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 0\n",
                count);
        exit_nicely(conn);
    }
    /* commit transaction */
    fprintf(stderr,"Committing...\n");
    if (TX_OK != (txrc = tx_commit())) {
        fprintf(stderr, "tx_commit error: %d\n", txrc);
        exit(txrc);
    }
    /* check there are no rows */
    fprintf(stderr,"Checking there are no rows...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (0 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 0\n",
                count);
        exit_nicely(conn);
    }
    /* start a new transaction */
    fprintf(stderr,"Starting a new transaction...\n");
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }
    /* insert data */
    fprintf(stderr,"Inserting one row...\n");
    if (mysql_query(conn,
                    "INSERT INTO authors VALUES(9, 'Name', 'Surname')")) {
        fprintf(stderr, "INSERT INTO authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    /* check there's exactly one row */
    fprintf(stderr,"Checking there's exactly one row...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (1 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 1\n",
                count);
        exit_nicely(conn);
    }
    /* backing out, the row must appear again... */
    fprintf(stderr,"Backing out...\n");
    if (TX_OK != (txrc = tx_rollback())) {
        fprintf(stderr, "tx_rollback error: %d\n", txrc);
        exit(txrc);
    }
    /* check there are no rows */
    fprintf(stderr,"Checking there are no rows...\n");
    if (mysql_query(conn, "SELECT COUNT(*) FROM authors;")) {
        fprintf(stderr, "SELECT COUNT(*) FROM authors: %u/%s",
                mysql_errno(conn), mysql_error(conn));
        exit_nicely(conn);
    }
    result = mysql_store_result(conn);
    row = mysql_fetch_row(result);
    count = strtol(row[0], NULL, 10);
    if (0 != count) {
        fprintf(stderr, "authors table contains %ld rows instead of 0\n",
                count);
        exit_nicely(conn);
    }
    /* close the resource manager */
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }

    return 0;
}
