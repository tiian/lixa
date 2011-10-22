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
 * Demarcation) API and CLI (IBM DB2 C) API together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* standard C & UNIX headers */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* IBM DB2 CLI API */
#include <sqlcli1.h>
#include <sqlca.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>



int main(int argc, char *argv[])
{
    /* generic variables */
    int txrc;
    /* IBM DB2 variables */
    SQLRETURN rc_cli = SQL_SUCCESS;
    SQLHANDLE env, conn, stat;
    char db_name[] = "sample";
    char user[] = "";
    char passwd[] = "";
    SQLCHAR sql_stat_i[] = "INSERT INTO DB2INST1.DEPT "
        "(DEPTNO, DEPTNAME, ADMRDEPT) "
        "VALUES('Z99', 'RESEARCH & DEVELOPMENT', 'E01')";
    SQLCHAR sql_stat_d[] = "DELETE DB2INST1.DEPT WHERE DEPTNO='Z99'";
    SQLCHAR *sql_stat = NULL;
    /* control variables */
    int         commit;
    int         insert;
    int         test_rc;

    if (argc < 4) {
        fprintf(stderr, "%s: at least three options must be specified\n",
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

    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env))) {
        fprintf(stderr, "Unable to allocate the environment handle: %d\n",
                rc_cli);
        exit(1);
    }

    if (SQL_SUCCESS != (rc_cli = SQLSetEnvAttr(
                           env, SQL_ATTR_ODBC_VERSION, (void *)SQL_OV_ODBC3,
                           0))) {
        fprintf(stderr, "Unable to set ODBC version 3.0: %d\n", rc_cli);
        exit(1);
    }
    
    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_DBC, env, &conn))) {
        fprintf(stderr, "Unable to allocate the connection handle: %d\n",
                rc_cli);
        exit(1);
    }

    if (SQL_SUCCESS != (rc_cli = SQLSetConnectAttr(
                            conn, SQL_ATTR_AUTOCOMMIT,
                            (SQLPOINTER)SQL_AUTOCOMMIT_OFF,
                            SQL_NTS))) {
        fprintf(stderr, "Unable to set autocommit (OFF) attribute: %d\n",
                rc_cli);
        exit(1);
    }

    if (SQL_SUCCESS != (rc_cli = SQLConnect(
                           conn, (SQLCHAR *)db_name, SQL_NTS,
                           (SQLCHAR *)user, SQL_NTS,
                           (SQLCHAR *)passwd, SQL_NTS))) {
        fprintf(stderr, "Unable to connect to database '%s': %d\n",
                db_name, rc_cli);
        exit(1);
    }

    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_STMT, conn, &stat))) {
        fprintf(stderr, "Unable to allocate the statement handle: %d\n",
                rc_cli);
        exit(1);
    }
    
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }

    /* insert data */
    if (insert) {
        if (SQL_SUCCESS != (rc_cli = SQLExecDirect(
                                stat, sql_stat_i, SQL_NTS))) {
            fprintf(stderr, "Unable to execute the SQL statement ('%s'): %d\n",
                    sql_stat_i, rc_cli);
            exit(1);
        }
    } else {
        if (SQL_SUCCESS != (rc_cli = SQLExecDirect(
                                stat, sql_stat_d, SQL_NTS))) {
            fprintf(stderr, "Unable to execute the SQL statement ('%s'): %d\n",
                    sql_stat_d, rc_cli);
            exit(1);
        }
    }

    /* commit transaction */
    if (commit) {
        if (test_rc != (txrc = tx_commit())) {
            fprintf(stderr, "tx_commit error: %d\n", txrc);
            exit(txrc);
        }
    } else {
        if (test_rc != (txrc = tx_rollback())) {
            fprintf(stderr, "tx_rollback error: %d\n", txrc);
            exit(txrc);
        }
    }
    
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_STMT, stat))) {
        fprintf(stderr, "Unable to free the statement handle: %d\n",
                rc_cli);
        exit(1);
    }
    
    if (SQL_SUCCESS != (rc_cli = SQLDisconnect(conn))) {
        fprintf(stderr, "Unable to disconnect from database '%s': %d\n",
                db_name, rc_cli);
        exit(1);
    }
    
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_DBC, conn))) {
        fprintf(stderr, "Unable to free the connection handle: %d\n",
                rc_cli);
        exit(1);
    }
    
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_ENV, env))) {
        fprintf(stderr, "Unable to free the environment handle: %d\n",
                rc_cli);
        exit(1);
    }
    
    /* close the resource manager(s) */
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }
    
    return 0;
}
