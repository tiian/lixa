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
#include "config.h"



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#ifdef HAVE_ORACLE
/* Oracle C Interface API header */
# include <oci.h>
#endif



#include "xta/xta.h"



/*
 * Forced xta_transaction_recover in a loop of multiple transactions
 * Related issue: https://github.com/tiian/lixa/issues/6
 */


/*
 * EXIT CODES:
 *  0: OK
 *  1: generic error
 *  2: xta_transaction_open error
 *  3: xta_transaction_start error
 *  4: xta_transaction_commit error
 *  5: xta_transaction_recover error
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    /* XTA variables (objects) */
    xta_transaction_manager_t *tm;
    xta_transaction_t *tx;
    xta_native_xa_resource_t *native_xa_res;
    xta_native_xa_resource_t *dynamic_native_xa_res;
    /* control variables */
    int        commit;
    int        insert;
    int        test_rc;
    int        i;
#ifdef HAVE_MYSQL
    /* MySQL variables */
    MYSQL *mysql_conn = NULL;
    char  *mysql_stmt_insert =
        "INSERT INTO authors VALUES(1930, 'Bonatti', 'Walter')";
    char  *mysql_stmt_delete = "DELETE FROM authors WHERE ID=1930";
    xta_mysql_xa_resource_t *mysql_xa_res;
#endif
#ifdef HAVE_ORACLE
    /* Oracle variables */
    int            oci_rc;
    OCIEnv        *oci_env;
    OCISvcCtx     *oci_svc_ctx;
    OCIStmt       *oci_stmt_hndl;
    OCIError      *oci_err_hndl;
    text          *oci_stmt_insert =
        (text *) "INSERT INTO authors (ID, LAST_NAME, FIRST_NAME) "
        "VALUES (1944, 'Messner', 'Reinhold')";
    text          *oci_stmt_delete =
        (text *) "DELETE FROM authors WHERE ID=1944";
#endif
#ifdef HAVE_POSTGRESQL
    /* PostgreSQL variables */
    PGconn *postgres_conn = NULL;
    PGresult *postgres_res;
    xta_postgresql_xa_resource_t *postgresql_xa_res;
    char *postgres_stmt_insert = "INSERT INTO authors VALUES("
        "1966, 'Kropp', 'Goran')";
    char *postgres_stmt_delete = "DELETE FROM authors WHERE ID=1966";
#endif

    /* turn ON trace for debugging purpose */
    xta_init();
    
    printf("%s| starting...\n", pgm);
    if (argc < 3) {
        fprintf(stderr, "%s: at least two options must be specified\n",
                argv[0]);
        return 1;
    }
    commit = strtol(argv[1], NULL, 0);
    test_rc = strtol(argv[2], NULL, 0);
    
    /*
     * dynamically create an XA native resource object
     */
#ifdef HAVE_ORACLE
    if (NULL == (dynamic_native_xa_res = xta_native_xa_resource_new(
                     "OracleIC_stareg",
                     "/opt/lixa/lib/switch_oracle_stareg.so",
                     "ORACLE_XA+Acc=P/hr/hr+SesTm=30+LogDir=/tmp+"
                     "threads=true+DbgFl=7+SqlNet=lixa_ora_db+"
                     "Loose_Coupling=true", ""))) {
        printf("%s| xta_native_xa_resource_new: returned NULL for "
               "dynamically creted resource\n", pgm);
        return 1;
    }
#endif
#ifdef HAVE_MYSQL
    /*
     * create a MySQL native connection
     */
    if (NULL == (mysql_conn = mysql_init(NULL))) {
        printf("%s| mysql_init: returned NULL\n", pgm);
        return 1;
    }
    if (NULL == mysql_real_connect(mysql_conn, "localhost", "lixa", "",
                                   "lixa", 0, NULL, 0)) {
        printf("%s| mysql_real_connect: returned error: %u, %s\n",
               pgm, mysql_errno(mysql_conn), mysql_error(mysql_conn));
        return 1;
    }
    /*
     * create a MySQL XA resource object
     * &<> are introduced to check a possible bug in XML serialization
     */
    if (NULL == (mysql_xa_res = xta_mysql_xa_resource_new(
                     mysql_conn, "MySQL&<>", "localhost,0,lixa,,lixa&<>"))) {
        printf("%s| xta_mysql_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#endif
#ifdef HAVE_POSTGRESQL
    /*
     * create a PostgreSQL native connection
     */
    postgres_conn = PQconnectdb("dbname=testdb");
    if (CONNECTION_OK != PQstatus(postgres_conn)) {
        printf("%s| PQconnectdb: returned error %s\n",
               pgm, PQerrorMessage(postgres_conn));
        PQfinish(postgres_conn);
        return 1;
    }
    /*
     * create a PostgreSQL XA resource object
     * &<> are introduced to check a possible bug in XML serialization
     */
    if (NULL == (postgresql_xa_res = xta_postgresql_xa_resource_new(
                     postgres_conn, "PostgreSQL&<>",
                     "dbname=testdb&<>"))) {
        printf("%s| xta_postgresql_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
#endif
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        printf("%s| xta_transaction_manager_new: returned NULL\n", pgm);
        return 1;
    }
    /*
     * create an XA native (static) resource object linked to the first Resouce
     * Manager configured in LIXA profile
     */
    if (NULL == (native_xa_res = xta_native_xa_resource_new_by_rmid(
                     0, xta_transaction_manager_get_config()))) {
        printf("%s| xta_native_xa_resource_new: returned NULL\n", pgm);
        return 1;
    }
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_get_transaction: returned NULL\n",
                pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }
    /* (try to) create again a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_get_transaction: returned NULL\n",
                pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }
    /* register the native XA Resource to the transaction manager: this step
     * is useless but it's not dangerous */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)native_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/native_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
    /* register the dynamic native XA Resource (Oracle) to the transaction
     * manager */
#ifdef HAVE_ORACLE
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)dynamic_native_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/dynamic_native_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
#endif
#ifdef HAVE_MYSQL
    /* register the MySQL XA Resource to the transaction manager */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)mysql_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/mysql_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
#endif
#ifdef HAVE_POSTGRESQL
    /* register the PostgreSQL XA Resource to the transaction manager */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)postgresql_xa_res))) {
        printf("%s| xta_transaction_enlist_resource/postgresql_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
#endif

    /* execute 10 inserts and 10 deletes */
    for (i=1; i<21; ++i) {
        insert = i%2;

        /* force a recovery of previous transactions, if any */
        rc = xta_transaction_recover(tx);
        if (LIXA_RC_OK != rc && LIXA_RC_BYPASSED_OPERATION != rc) {
            printf("%s| xta_transaction_recover: returned %d\n", pgm, rc);
            return 5;
        }        
    
        /* start a Distributed Transaction */
        if (LIXA_RC_OK != (rc = xta_transaction_start(tx, FALSE))) {
            printf("%s| xta_transaction_start: returned %d\n", pgm, rc);
            return 3;
        }

#ifdef HAVE_MYSQL
        /* insert data */
        if (insert) {
            if (mysql_query(mysql_conn, mysql_stmt_insert)) {
                fprintf(stderr, "%s| INSERT INTO authors: %u/%s",
                        pgm, mysql_errno(mysql_conn), mysql_error(mysql_conn));
                mysql_close(mysql_conn);
                return 1;
            }
            printf("%s| MySQL statement >%s< completed\n",
                   pgm, mysql_stmt_insert);
        } else {
            if (mysql_query(mysql_conn, mysql_stmt_delete)) {
                fprintf(stderr, "%s| DELETE FROM authors: %u/%s",
                        pgm, mysql_errno(mysql_conn), mysql_error(mysql_conn));
                mysql_close(mysql_conn);
                return 1;
            }
            printf("%s| MySQL statement >%s< completed\n",
                   pgm, mysql_stmt_delete);
        }
#endif /* HAVE_MYSQL */
    
#ifdef HAVE_ORACLE
        /* retrieve environment and context */
        if (NULL == (oci_env = xaoEnv(NULL))) {
            fprintf(stderr, "%s| xaoEnv returned a NULL pointer\n", pgm);
            return 1;
        }
        if (NULL == (oci_svc_ctx = xaoSvcCtx(NULL))) {
            fprintf(stderr, "%s| xaoSvcCtx returned a NULL pointer\n", pgm);
            return 1;
        }
        /* allocate statement and error handles */
        if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_stmt_hndl,
                                 OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
            fprintf(stderr, "%s| Unable to allocate OCI statement handle\n",
                    pgm);
            return 1;
        }
        if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_err_hndl,
                                 OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
            fprintf(stderr, "%s| Unable to allocate OCI error handle\n", pgm);
            return 1;
        }
        /* insert data */
        if (insert) {
            if (OCI_SUCCESS != OCIStmtPrepare(
                    oci_stmt_hndl, oci_err_hndl, oci_stmt_insert,
                    (ub4) strlen((char *)oci_stmt_insert),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
                fprintf(stderr, "%s| Unable to prepare INSERT OCI statement "
                        "for execution\n", pgm);
                return 1;
            }
            oci_rc = OCIStmtExecute(
                oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
                (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
                (OCISnapshot *)NULL, OCI_DEFAULT);
            if (OCI_SUCCESS != oci_rc && OCI_SUCCESS_WITH_INFO != oci_rc) {
                fprintf(stderr, "%s| Error while executing INSERT statement; "
                        "ocirc = %d\n", pgm, oci_rc);
                return oci_rc;
            }
            printf("%s| OCI statement >%s< completed\n",
                   pgm, (char *)oci_stmt_insert);
        } else {
            if (OCI_SUCCESS != OCIStmtPrepare(
                    oci_stmt_hndl, oci_err_hndl, oci_stmt_delete,
                    (ub4) strlen((char *)oci_stmt_delete),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
                fprintf(stderr, "%s| Unable to prepare DELETE statement for "
                        "execution\n", pgm);
                return 1;
            }
            oci_rc = OCIStmtExecute(
                oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
                (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
                (OCISnapshot *)NULL, OCI_DEFAULT);
            if (OCI_SUCCESS != oci_rc && OCI_SUCCESS_WITH_INFO != oci_rc) {
                fprintf(stderr, "%s| Error while executing DELETE statement; "
                        "ocirc = %d\n", pgm, oci_rc);
                return oci_rc;
            }
            printf("%s| OCI statement >%s< completed\n",
                   pgm, (char *)oci_stmt_delete);
        }
        /* free the allocated handles */
        OCIHandleFree((dvoid *)oci_stmt_hndl, (ub4)OCI_HTYPE_STMT);
        OCIHandleFree((dvoid *)oci_err_hndl, (ub4)OCI_HTYPE_ERROR);
#endif /* HAVE_ORACLE */

#ifdef HAVE_POSTGRESQL
        if (insert) {
            postgres_res = PQexec(
                postgres_conn, postgres_stmt_insert);
            if (PGRES_COMMAND_OK != PQresultStatus(postgres_res)) {
                fprintf(stderr, "%s| error while executing >%s< %s\n",
                        pgm, postgres_stmt_insert,
                        PQerrorMessage(postgres_conn));
                PQclear(postgres_res);
                PQfinish(postgres_conn);
                return 1;
            }
            PQclear(postgres_res);
            printf("%s| PostgreSQL statement >%s< completed\n",
                   pgm, postgres_stmt_insert);
        } else {
            postgres_res = PQexec(
                postgres_conn, postgres_stmt_delete);
            if (PGRES_COMMAND_OK != PQresultStatus(postgres_res)) {
                fprintf(stderr, "%s| error while executing >%s< %s\n",
                        pgm, postgres_stmt_delete,
                        PQerrorMessage(postgres_conn));
                PQclear(postgres_res);
                PQfinish(postgres_conn);
                return 1;
            }
            PQclear(postgres_res);
            printf("%s| PostgreSQL statement >%s< completed\n",
                   pgm, postgres_stmt_delete);
    }    
#endif /* HAVE_POSTGRESQL */
    
        /*
         * some code here ...
         */
        
        /* commit the Distributed Transaction */
        if (commit) {
            if (test_rc != (rc = xta_transaction_commit(tx, FALSE))) {
                fprintf(stderr, "%s| xta_transaction_commit: returned %d "
                        "instead of %d\n", pgm, rc, test_rc);
                return 4;
            }
            printf("%s| XTA commit returned %d as expected\n", pgm, rc);
        } else {
            if (test_rc != (rc = xta_transaction_rollback(tx))) {
                fprintf(stderr, "%s| xta_transaction_rollback: returned %d "
                        "instead of %d\n", pgm, rc, test_rc);
                return 1;
            }
            printf("%s| XTA rollback returned %d as expected\n", pgm, rc);
        } /* if (commit) */
    } /* for (i=1; i<21; ++i) */
    
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
#ifdef HAVE_POSTGRESQL
    /*
     * delete the PostgreSQL XA resource object
     */
    xta_postgresql_xa_resource_delete(postgresql_xa_res);
    /*
     * close PostgreSQL database connection
     */
    PQfinish(postgres_conn);
#endif
#ifdef HAVE_MYSQL
    /*
     * delete the MySQL XA resource object
     */
    xta_mysql_xa_resource_delete(mysql_xa_res);
    /*
     * close MySQL database connection
     */
    mysql_close(mysql_conn);
#endif
#ifdef HAVE_ORACLE
    /*
     * delete dynamically created native XA Resource object for Oracle
     */
    xta_native_xa_resource_delete(dynamic_native_xa_res);
#endif 
    /*
     * delete native XA Resource object
     */
    xta_native_xa_resource_delete(native_xa_res);
    /*
     * end of XTA API calls
     */
    printf("%s| ...finished\n", pgm);
    return 0;
}
