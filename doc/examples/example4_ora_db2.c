/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
 * Demarcation) API, OCI (Oracle C Interface) and CLI (IBM DB2 C) API
 * together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* standard UNIX headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Oracle C Interface API header */
#include <oci.h>

/* IBM DB2 CLI API */
#include <sqlcli1.h>
#include <sqlca.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>




int main(int argc, char *argv[])
{
    /* generic variables */
    int            txrc;
    int            rollback = 0;
    int            error = 0;
    /* ORACLE related variables */
    int            ocirc;
    OCIEnv        *oci_env;
    OCISvcCtx     *oci_svc_ctx;
    OCIStmt       *stmt_hndl;
    OCIError      *err_hndl;
    text          *sql_icountries = (text *) "INSERT INTO COUNTRIES "
        "(COUNTRY_ID, COUNTRY_NAME, REGION_ID) "
        "VALUES ('RS', 'Repubblica San Marino', 1)";
    text          *sql_dcountries = (text *) "DELETE FROM COUNTRIES WHERE "
        "COUNTRY_ID = 'RS'";
    /* IBM DB2 related variables */
    SQLRETURN      rc_cli = SQL_SUCCESS;
    SQLHANDLE      env, conn, stat;
    char           db_name[] = "sample";
    char           user[] = "";
    char           passwd[] = "";
    SQLCHAR        sql_iorg[] = "INSERT INTO DB2INST1.ORG"
        "(DEPTNUMB, DEPTNAME, MANAGER, DIVISION, LOCATION) "
        "VALUES(150, 'Europe', 231, 'R&D', 'Mojan')";
    SQLCHAR        sql_dorg[] = "DELETE FROM DB2INST1.ORG WHERE "
        "LOCATION='Mojan'";
    SQLCHAR        sql_idept[] = "INSERT INTO DB2INST1.DEPT "
        "(DEPTNO, DEPTNAME, ADMRDEPT) "
        "VALUES('Z99', 'RESEARCH & DEVELOPMENT', 'E01')";
    SQLCHAR        sql_ddept[] = "DELETE DB2INST1.DEPT WHERE DEPTNO='Z99'";

    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* retrieve Oracle environment and context */
    if (NULL == (oci_env = xaoEnv(NULL))) {
        fprintf(stderr, "xaoEnv/returned a NULL pointer\n");
        exit(1);
    }
    if (NULL == (oci_svc_ctx = xaoSvcCtx(NULL))) {
        fprintf(stderr, "xaoSvcCtx/returned a NULL pointer\n");
        exit(1);
    }

    /* allocate ORACLE statement and error handles */
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&stmt_hndl,
                             OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
        fprintf(stderr,
                "OCIHandleAlloc/Unable to allocate statement handle\n");
        exit(1);
    }
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&err_hndl,
                             OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "OCIHandleAlloc/Unable to allocate error handle\n");
        exit(1);
    }

    /* allocate IBM DB2 statement and error handles */
    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env))) {
        fprintf(stderr,
                "SQLAllocHandle/Unable to allocate the environment "
                "handle: %d\n", rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLSetEnvAttr(
                           env, SQL_ATTR_ODBC_VERSION, (void *)SQL_OV_ODBC3,
                           0))) {
        fprintf(stderr, "SQLSetEnvAttr/Unable to set ODBC version 3.0: %d\n",
                rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_DBC, env, &conn))) {
        fprintf(stderr,
                "SQLAllocHandle/Unable to allocate the connection "
                "handle: %d\n", rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLSetConnectAttr(
                            conn, SQL_ATTR_AUTOCOMMIT,
                            (SQLPOINTER)SQL_AUTOCOMMIT_OFF,
                            SQL_NTS))) {
        fprintf(stderr, "SQLSetConnectAttr/Unable to set autocommit "
                "(OFF) attribute: %d\n", rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLConnect(
                           conn, (SQLCHAR *)db_name, SQL_NTS,
                           (SQLCHAR *)user, SQL_NTS,
                           (SQLCHAR *)passwd, SQL_NTS))) {
        fprintf(stderr, "SQLConnect/Unable to connect to database '%s': %d\n",
                db_name, rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLAllocHandle(
                            SQL_HANDLE_STMT, conn, &stat))) {
        fprintf(stderr,
                "SQLAllocHandle/Unable to allocate the statement handle: %d\n",
                rc_cli);
        exit(1);
    }

    /* start a new transaction */
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }

    if (argc > 1) {
        int i;
        for (i = 1; i < argc; ++i) {
            error = 0;
            /* "parse" the action must be performed */
            if (!strcmp(argv[i], "iorg")) {
                /* INSERT INTO DB2INST1.ORG (DB2) table */
                if (SQL_SUCCESS != (rc_cli = SQLExecDirect(
                                        stat, sql_iorg, SQL_NTS))) {
                    fprintf(stderr, "SQLExecDirect/Unable to execute the "
                            "SQL statement ('%s'): %d\n", sql_iorg, rc_cli);
                    error = 1;
                    break;
                } else
                    fprintf(stderr, "INSERT INTO DB2INST1.ORG executed!\n");
            } else if (!strcmp(argv[i], "dorg")) {
                /* DELETE FROM DB2INST1.ORG (DB2) table */
                if (SQL_SUCCESS != (rc_cli = SQLExecDirect(
                                        stat, sql_dorg, SQL_NTS))) {
                    fprintf(stderr, "SQLExecDirect/Unable to execute the "
                            "SQL statement ('%s'): %d\n", sql_dorg, rc_cli);
                    error = 1;
                    break;
                } else
                    fprintf(stderr, "DELETE FROM DB2INST1.ORG executed!\n");
            } else if (!strcmp(argv[i], "idept")) {
                /* INSERT INTO DB2INST1.DEPT (DB2) table */
                if (SQL_SUCCESS != (rc_cli = SQLExecDirect(
                                        stat, sql_idept, SQL_NTS))) {
                    fprintf(stderr, "SQLExecDirect/Unable to execute the "
                            "SQL statement ('%s'): %d\n", sql_idept, rc_cli);
                    error = 1;
                    break;
                } else
                    fprintf(stderr, "INSERT INTO DB2INST1.DEPT executed!\n");
            } else if (!strcmp(argv[i], "ddept")) {
                /* DELETE FROM DB2INST1.DEPT (DB2) table */
                if (SQL_SUCCESS != (rc_cli = SQLExecDirect(
                                        stat, sql_ddept, SQL_NTS))) {
                    fprintf(stderr, "SQLExecDirect/Unable to execute the "
                            "SQL statement ('%s'): %d\n", sql_ddept, rc_cli);
                    error = 1;
                    break;
                } else
                    fprintf(stderr, "DELETE FROM DB2INST1.DEPT executed!\n");
            } else if (!strcmp(argv[i], "icountries")) {
                /* INSERT INTO COUNTRIES (ORACLE) table */
                if (OCI_SUCCESS != OCIStmtPrepare(
                        stmt_hndl, err_hndl,
                        sql_icountries,
                        (ub4) strlen((char *)sql_icountries),
                        (ub4) OCI_NTV_SYNTAX,
                        (ub4) OCI_DEFAULT)) {
                    fprintf(stderr,
                            "OCIStmtPrepare/Unable to prepare INSERT "
                            "statement for execution\n");
                    error = 1;
                    break;
                }
                ocirc = OCIStmtExecute(oci_svc_ctx, stmt_hndl, err_hndl,
                                       (ub4)1, (ub4)0,
                                       (CONST OCISnapshot *)NULL,
                                       (OCISnapshot *)NULL, OCI_DEFAULT);
                if (OCI_SUCCESS != ocirc && OCI_SUCCESS_WITH_INFO != ocirc) {
                    fprintf(stderr,
                            "OCIStmtExecute/Error while executing INSERT "
                            "statement; ocirc = %d\n", ocirc);
                    error = 1;
                    break;
                } else
                    fprintf(stderr, "INSERT INTO COUNTRIES executed!\n");
            } else if (!strcmp(argv[i], "dcountries")) {
                /* INSERT INTO COUNTRIES (ORACLE) table */
                if (OCI_SUCCESS != OCIStmtPrepare(
                        stmt_hndl, err_hndl, sql_dcountries,
                        (ub4) strlen((char *)sql_dcountries),
                        (ub4) OCI_NTV_SYNTAX,
                        (ub4) OCI_DEFAULT)) {
                    fprintf(stderr, "OCIStmtPrepare/Unable to prepare DELETE "
                            "statement for execution\n");
                    error = 1;
                    break;
                }
                ocirc = OCIStmtExecute(oci_svc_ctx, stmt_hndl, err_hndl,
                                       (ub4)1, (ub4)0,
                                       (CONST OCISnapshot *)NULL,
                                       (OCISnapshot *)NULL, OCI_DEFAULT);
                if (OCI_SUCCESS != ocirc && OCI_SUCCESS_WITH_INFO != ocirc) {
                    fprintf(stderr, "OCIStmtExecute/Error while executing "
                            "DELETE statement; ocirc = %d\n", ocirc);
                    error = 1;
                    break;
                } else
                    fprintf(stderr, "DELETE FROM COUNTRIES executed!\n");
            } else if (!strcmp(argv[i], "rollback"))
                rollback = 1;
        }
    }

    if (error || rollback) {
        if (TX_OK != (txrc = tx_rollback())) {
            fprintf(stderr, "tx_rollback error: %d\n", txrc);
            exit(txrc);
        }
        fprintf(stderr, "ROLLBACK performed!\n");
    } else {
        if (TX_OK != (txrc = tx_commit())) {
            fprintf(stderr, "tx_commit error: %d\n", txrc);
            exit(txrc);
        }
        fprintf(stderr, "COMMIT performed!\n");
    }

    /* release ORACLE handles */
    OCIHandleFree((dvoid *)stmt_hndl, (ub4)OCI_HTYPE_STMT);
    OCIHandleFree((dvoid *)err_hndl, (ub4)OCI_HTYPE_ERROR);

    /* release IBM DB2 handles */
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_STMT, stat))) {
        fprintf(stderr, "SQLFreeHandle/Unable to free the statement "
                "handle: %d\n", rc_cli);
        exit(1);
    }    
    if (SQL_SUCCESS != (rc_cli = SQLDisconnect(conn))) {
        fprintf(stderr, "SQLDisconnect/Unable to disconnect from "
                "database '%s': %d\n", db_name, rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_DBC, conn))) {
        fprintf(stderr, "SQLFreeHandle/Unable to free the connection "
                "handle: %d\n", rc_cli);
        exit(1);
    }
    if (SQL_SUCCESS != (rc_cli = SQLFreeHandle(SQL_HANDLE_ENV, env))) {
        fprintf(stderr, "SQLFreeHandle/Unable to free the environment "
                "handle: %d\n", rc_cli);
        exit(1);
    }

    /* close the resource manager(s) */
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }
    
    return 0;
}
