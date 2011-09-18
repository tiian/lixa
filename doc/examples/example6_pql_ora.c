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
 * Demarcation) API, PostgreSQL and OCI (Oracle C Interface) API together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* standard UNIX headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Oracle C Interface API header */
#include <oci.h>

/* PostgreSQL */
#include <libpq-fe.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>

/* LIXA help library for PostgreSQL */
#include <lixapq.h>



static void exit_nicely(PGconn *conn, int ocirc)
{
    PQfinish(conn);
    exit(ocirc);
}



int main(int argc, char *argv[])
{
    /* generic variables */
    int         txrc, delete;
    /* PostgreSQL variables */
    const char *conninfo;
    PGconn     *conn;
    PGresult   *res;
    /* Oracle variables */
    int         ocirc = OCI_SUCCESS;
    OCIEnv     *oci_env;
    OCISvcCtx  *oci_svc_ctx;
    OCIStmt    *stmt_hndl;
    OCIError   *err_hndl;
    text       *stmt_i = (text *) "INSERT INTO COUNTRIES "
        "(COUNTRY_ID, COUNTRY_NAME, REGION_ID) "
        "VALUES ('RS', 'Repubblica San Marino', 1)";
    text       *stmt_d = (text *) "DELETE FROM COUNTRIES WHERE "
        "COUNTRY_ID = 'RS'";

    conninfo = "dbname = testdb";

    if (argc > 1 && (!strcmp(argv[1], "delete") || !strcmp(argv[1], "DELETE")))
        delete = 1;
    else
        delete = 0;
 
    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* retrieve PostgreSQL connection */
    conn = lixa_pq_get_conn();
    /*
     * These functions can be used when there are more than one PostgreSQL
     * configured as a resource manager
     * conn = lixa_pq_get_conn_by_pos(0);
     * conn = lixa_pq_get_conn_by_rmid(0);
     */

    /* retrieve Oracle environment and context */
    if (NULL == (oci_env = xaoEnv(NULL))) {
        fprintf(stderr, "xaoEnv returned a NULL pointer\n");
        exit_nicely(conn, 1);
    }
    if (NULL == (oci_svc_ctx = xaoSvcCtx(NULL))) {
        fprintf(stderr, "xaoSvcCtx returned a NULL pointer\n");
        exit_nicely(conn, 1);
    }

    /* allocate Oracle statement and error handles */
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&stmt_hndl,
                             OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "Unable to allocate statement handle\n");
        exit_nicely(conn, 1);
    }
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&err_hndl,
                             OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "Unable to allocate error handle\n");
        exit_nicely(conn, 1);
    }

    /* start a new transaction */
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit_nicely(conn, txrc);
    }

    if (delete) {
        printf("Deleting a row from the tables...\n");
        /* PostgreSQL stuff */
        res = PQexec(conn,
                     "DELETE FROM authors WHERE id=1;");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            fprintf(stderr, "DELETE FROM authors: %s",
                    PQerrorMessage(conn));
            PQclear(res);
            exit_nicely(conn, 1);
        }
        PQclear(res);
        /* Oracle stuff */
        if (OCI_SUCCESS != OCIStmtPrepare(stmt_hndl, err_hndl, stmt_d,
                                          (ub4) strlen((char *)stmt_d),
                                          (ub4) OCI_NTV_SYNTAX,
                                          (ub4) OCI_DEFAULT)) {
            fprintf(stderr, "Unable to prepare DELETE statement for "
                    "execution\n");
            exit_nicely(conn, 1);
        }
        ocirc = OCIStmtExecute(oci_svc_ctx, stmt_hndl, err_hndl,
                               (ub4)1, (ub4)0,
                               (CONST OCISnapshot *)NULL,
                               (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != ocirc && OCI_SUCCESS_WITH_INFO != ocirc) {
            fprintf(stderr, "Error while executing DELETE statement; "
                    "ocirc = %d\n",
                    ocirc);
            exit_nicely(conn, ocirc);
        } else
            fprintf(stderr, "Oracle DELETE statement executed!\n");
    } else {
        printf("Inserting a row in the tables...\n");
        /* PostgreSQL stuff */
        res = PQexec(conn,
                     "INSERT INTO authors VALUES(1, 'Foo', 'Bar');");
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            fprintf(stderr, "INSERT INTO authors: %s",
                    PQerrorMessage(conn));
            PQclear(res);
            exit_nicely(conn, 1);
        }
        PQclear(res);
        /* Oracle stuff */
        if (OCI_SUCCESS != OCIStmtPrepare(stmt_hndl, err_hndl, stmt_i,
                                          (ub4) strlen((char *)stmt_i),
                                          (ub4) OCI_NTV_SYNTAX,
                                          (ub4) OCI_DEFAULT)) {
            fprintf(stderr,
                    "Unable to prepare INSERT statement for execution\n");
            exit_nicely(conn, 1);
        }

        ocirc = OCIStmtExecute(oci_svc_ctx, stmt_hndl, err_hndl,
                               (ub4)1, (ub4)0,
                               (CONST OCISnapshot *)NULL,
                               (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != ocirc && OCI_SUCCESS_WITH_INFO != ocirc) {
            fprintf(stderr, "Error while executing INSERT statement; "
                    "ocirc = %d\n", ocirc);
            exit_nicely(conn, ocirc);
        } else
            fprintf(stderr, "Oracle INSERT statement executed!\n");
    }
    
    if (TX_OK != (txrc = tx_commit())) {
        fprintf(stderr, "tx_commit error: %d\n", txrc);
        exit_nicely(conn, txrc);
    }
/*
 * This function can be used if you want to rollback instead of commit the
 * transaction
    if (TX_OK != (txrc = tx_rollback())) {
        fprintf(stderr, "tx_rollback error: %d\n", txrc);
        exit(txrc);
    }
*/

    /* free the allocated handles (Oracle stuff) */
    OCIHandleFree((dvoid *)stmt_hndl, (ub4)OCI_HTYPE_STMT);
    OCIHandleFree((dvoid *)err_hndl, (ub4)OCI_HTYPE_ERROR);
    
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit_nicely(conn, txrc);
    }

    return 0;
}
