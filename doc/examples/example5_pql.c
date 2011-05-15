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
    const char *conninfo;
    PGconn     *conn;
    PGresult   *res;

    conninfo = "dbname = testdb";
    
    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    conn = lixa_pq_get_conn();
    /*
    conn = PQconnectdb(conninfo);
    */
    if (CONNECTION_OK != PQstatus(conn)) {
        fprintf(stderr, "Connection to database failed: %s",
                PQerrorMessage(conn));
        exit_nicely(conn);
    } else
        printf("conn=%p\n", conn);
    
    res = PQexec(conn, "BEGIN");
    if (PGRES_COMMAND_OK != PQresultStatus(res))
    {
        fprintf(stderr, "BEGIN command failed: %s", PQerrorMessage(conn));
        PQclear(res);
        exit_nicely(conn);
    }
    PQclear(res);

    res = PQexec(conn,
                 "INSERT INTO authors VALUES(1, 'Ferrari', 'Christian');");
    if (PGRES_COMMAND_OK != PQresultStatus(res))
    {
        fprintf(stderr, "INSERT INTO authors: %s", PQerrorMessage(conn));
        PQclear(res);
        exit_nicely(conn);
    }
    PQclear(res);

    res = PQexec(conn, "PREPARE TRANSACTION 'foo-bar';");
    if (PGRES_COMMAND_OK != PQresultStatus(res))
    {
        fprintf(stderr, "PREPARE TRANSACTION command failed: %s", PQerrorMessage(conn));
        PQclear(res);
        exit_nicely(conn);
    }
    PQclear(res);
/*
    res = PQexec(conn, "ROLLBACK PREPARED 'foo-bar';");
    if (PGRES_COMMAND_OK != PQresultStatus(res))
    {
        fprintf(stderr, "ROLLBACK PREPARED command failed: %s", PQerrorMessage(conn));
        PQclear(res);
        exit_nicely(conn);
    }
    PQclear(res);
*/

    res = PQexec(conn, "COMMIT PREPARED 'foo-bar';");
    if (PGRES_COMMAND_OK != PQresultStatus(res))
    {
        fprintf(stderr, "COMMIT PREPARED command failed: %s", PQerrorMessage(conn));
        PQclear(res);
        exit_nicely(conn);
    }
    PQclear(res);

    PQfinish(conn);

    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }

    return 0;
}
