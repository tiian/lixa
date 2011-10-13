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
 * Demarcation) API, PostgreSQL and CLI (IBM DB2 C) API together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* standard C & UNIX headers */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* PostgreSQL */
#include <libpq-fe.h>

/* IBM DB2 CLI API */
#include <sqlcli1.h>
#include <sqlca.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>

/* LIXA help library for PostgreSQL */
#include <lixapq.h>



static void exit_nicely(PGconn *conn, int exitrc)
{
    PQfinish(conn);
    exit(exitrc);
}



int main(int argc, char *argv[])
{
    /* generic variables */
    int         txrc, delete = 0, dept = 0;
    /* PostgreSQL variables */
    const char *conninfo;
    PGconn     *pq_conn;
    PGresult   *res;
    /* IBM DB2 variables */
    SQLRETURN rc_cli = SQL_SUCCESS;
    SQLHANDLE env, db2_conn, stat;
    char db_name[] = "sample";
    char user[] = "";
    char passwd[] = "";
    SQLCHAR sql_stat_i[] = "INSERT INTO DB2INST1.ORG"
        "(DEPTNUMB, DEPTNAME, MANAGER, DIVISION, LOCATION) "
        "VALUES(150, 'Europe', 231, 'R&D', 'Mojan')";
    SQLCHAR sql_stat_d[] = "DELETE FROM DB2INST1.ORG WHERE LOCATION='Mojan'";
    SQLCHAR sql_stat_id[] = "INSERT INTO DB2INST1.DEPT "
        "(DEPTNO, DEPTNAME, ADMRDEPT) "
        "VALUES('Z99', 'RESEARCH & DEVELOPMENT', 'E01')";
    SQLCHAR sql_stat_dd[] = "DELETE DB2INST1.DEPT WHERE DEPTNO='Z99'";
    SQLCHAR *sql_stat = NULL;
    char psql_stat_i[] = "INSERT INTO authors VALUES(1, 'Foo', 'Bar');";
    char psql_stat_d[] = "DELETE FROM authors WHERE id=1;";
    char *psql_stat = NULL;

    conninfo = "dbname = testdb";
    
    if (argc > 1 && (!strcmp(argv[1], "delete") || !strcmp(argv[1], "DELETE")))
        delete = 1;
    if (argc > 2 && (!strcmp(argv[2], "dept") ||
                     !strcmp(argv[2], "DEPT")))
        dept = 1;
    if (delete) {
        psql_stat = psql_stat_d;
        if (dept)
            sql_stat = sql_stat_dd;
        else
            sql_stat = sql_stat_d;
    } else {
        psql_stat = psql_stat_i;
        if (dept)
            sql_stat = sql_stat_id;
        else
            sql_stat = sql_stat_i;
    }
    
    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* retrieve PostgreSQL connection */
    pq_conn = lixa_pq_get_conn();
    /*
     * These functions can be used when there are more than one PostgreSQL
     * configured as a resource manager
     * pq_conn = lixa_pq_get_conn_by_rmid(0);
     */

    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env))) {
        fprintf(stderr, "Unable to allocate the environment handle: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLSetEnvAttr(
                           env, SQL_ATTR_ODBC_VERSION, (void *)SQL_OV_ODBC3,
                           0))) {
        fprintf(stderr, "Unable to set ODBC version 3.0: %d\n", rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_DBC, env, &db2_conn))) {
        fprintf(stderr, "Unable to allocate the connection handle: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLSetConnectAttr(
                            db2_conn, SQL_ATTR_AUTOCOMMIT,
                            (SQLPOINTER)SQL_AUTOCOMMIT_OFF,
                            SQL_NTS))) {
        fprintf(stderr, "Unable to set autocommit (OFF) attribute: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLConnect(
                           db2_conn, (SQLCHAR *)db_name, SQL_NTS,
                           (SQLCHAR *)user, SQL_NTS,
                           (SQLCHAR *)passwd, SQL_NTS))) {
        fprintf(stderr, "Unable to connect to database '%s': %d\n",
                db_name, rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_STMT, db2_conn, &stat))) {
        fprintf(stderr, "Unable to allocate the statement handle: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }
    
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit_nicely(pq_conn, txrc);
    }

    printf("Executing DB2 statement '%s'...\n", sql_stat);
    if (SQL_SUCCESS != (rc_cli = SQLExecDirect(stat, sql_stat, SQL_NTS))) {
        fprintf(stderr, "Unable to execute the SQL statement ('%s'): %d\n",
                sql_stat, rc_cli);
        exit_nicely(pq_conn, 1);
    }
    printf("Executing PostgreSQL statement '%s'...\n", psql_stat);
    res = PQexec(pq_conn, psql_stat);
    if (PGRES_COMMAND_OK != PQresultStatus(res)) {
        fprintf(stderr, "PostgreSQL error: %s", PQerrorMessage(pq_conn));
        PQclear(res);
        exit_nicely(pq_conn, 1);
    }
    PQclear(res);

    /* this is to test tx_commit */
    if (TX_OK != (txrc = tx_commit())) {
        fprintf(stderr, "tx_commit error: %d\n", txrc);
        exit_nicely(pq_conn, txrc);
    }

    /* this is to test tx_rollback */
    /*
    if (TX_OK != (txrc = tx_rollback())) {
        fprintf(stderr, "tx_rollback error: %d\n", txrc);
        exit(txrc);
    }
    */
    
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_STMT, stat))) {
        fprintf(stderr, "Unable to free the statement handle: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }    
    if (SQL_SUCCESS != (rc_cli = SQLDisconnect(db2_conn))) {
        fprintf(stderr, "Unable to disconnect from database '%s': %d\n",
                db_name, rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_DBC, db2_conn))) {
        fprintf(stderr, "Unable to free the connection handle: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_ENV, env))) {
        fprintf(stderr, "Unable to free the environment handle: %d\n",
                rc_cli);
        exit_nicely(pq_conn, 1);
    }
    
    /* close the resource manager(s) */
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit_nicely(pq_conn, txrc);
    }
    
    return 0;
}
