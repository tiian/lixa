/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
 * Branch case test for XTA
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    char *xid_string = NULL;
    FILE *xid_file = NULL;
    FILE *xid_file2 = NULL;
    unsigned pid = (unsigned)getpid();
    
    /* XTA variables (objects) */
    xta_transaction_manager_t *tm;
    xta_transaction_t *tx;
    xta_native_xa_resource_t *native_xa_res;
    xta_native_xa_resource_t *dynamic_native_xa_res;
    /* control variables */
    enum Phase { SUPERIOR, SUBORDINATE, NO_PHASE } phase;
    int        commit;
    int        insert;
    int        statement;
    int        test_rc;
    const char *filename = NULL;
    const char *filename2 = NULL;
#ifdef HAVE_ORACLE
    /* Oracle variables */
    int            oci_rc;
    OCIEnv        *oci_env;
    OCISvcCtx     *oci_svc_ctx;
    OCIStmt       *oci_stmt_hndl;
    OCIError      *oci_err_hndl;
    text          *oci_stmt_insert1 =
        (text *) "INSERT INTO COUNTRIES (COUNTRY_ID, COUNTRY_NAME, REGION_ID) "
        "VALUES ('IS', 'Iceland', 1)";
    text          *oci_stmt_delete1 =
        (text *) "DELETE FROM COUNTRIES WHERE COUNTRY_ID = 'IS'";
    text          *oci_stmt_insert2 =
        (text *) "INSERT INTO COUNTRIES (COUNTRY_ID, COUNTRY_NAME, REGION_ID) "
        "VALUES ('ZA', 'South Africa', 4)";
    text          *oci_stmt_delete2 =
        (text *) "DELETE FROM COUNTRIES WHERE COUNTRY_ID = 'ZA'";
    text          *oci_stmt_insert3 =
        (text *) "INSERT INTO COUNTRIES (COUNTRY_ID, COUNTRY_NAME, REGION_ID) "
        "VALUES ('MV', 'Republic of Maldives', 3)";
    text          *oci_stmt_delete3 =
        (text *) "DELETE FROM COUNTRIES WHERE COUNTRY_ID = 'MV'";
    text          *oci_stmt_insert = NULL;
    text          *oci_stmt_delete = NULL;
#endif

    /* turn ON trace for debugging purpose */
    xta_init();
    
    fprintf(stderr, "%s/%u| starting...\n", pgm, pid);
    if (argc < 7) {
        fprintf(stderr, "%s/%u: at least six options must be specified\n",
                argv[0], pid);
        return 1;
    }
    phase = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    statement = strtol(argv[3], NULL, 0);
    commit = strtol(argv[4], NULL, 0);
    test_rc = strtol(argv[5], NULL, 0);
    filename = argv[6];
    /* check if a second filename is provided */
    if (argc == 8)
        filename2 = argv[7];

    /* check phase */
    switch (phase) {
        case SUPERIOR:
            fprintf(stderr, "%s/%u| phase=%d (SUPERIOR)\n", pgm, pid, phase);
            /* open file for write */
            if (NULL == (xid_file = fopen(filename, "w"))) {
                fprintf(stderr, "%s/%u| error while opening file '%s'\n",
                        pgm, pid, filename);
                return 1;
            }
            break;
        case SUBORDINATE:
            fprintf(stderr, "%s/%u| phase=%d (SUBORDINATE)\n",
                    pgm, pid, phase);
            /* open file for read */
            if (NULL == (xid_file = fopen(filename, "r"))) {
                fprintf(stderr, "%s/%u| error while opening file '%s'\n",
                        pgm, pid, filename);
                return 1;
            }
            /* open file for write */
            if (NULL != filename2 &&
                NULL == (xid_file2 = fopen(filename2, "w"))) {
                fprintf(stderr, "%s/%u| error while opening file '%s'\n",
                        pgm, pid, filename2);
                return 1;
            }
            break;
        case NO_PHASE:
            fprintf(stderr, "%s/%u| phase=%d (NO_PHASE)\n", pgm, pid, phase);
            break;
        default:
            fprintf(stderr, "%s/%u| phase=%d UNKNOWN!\n", pgm, pid, phase);
            return 1;
    } /* switch(phase) */

    /* check statement */
    switch (statement) {
        case 1:
#ifdef HAVE_ORACLE
            oci_stmt_insert = oci_stmt_insert1;
            oci_stmt_delete = oci_stmt_delete1;
#endif
            break;
        case 2:
#ifdef HAVE_ORACLE
            oci_stmt_insert = oci_stmt_insert2;
            oci_stmt_delete = oci_stmt_delete2;
#endif
            break;
        case 3:
#ifdef HAVE_ORACLE
            oci_stmt_insert = oci_stmt_insert3;
            oci_stmt_delete = oci_stmt_delete3;
#endif
            break;
        default:
            fprintf(stderr, "%s/%u| statement=%d is not valid!\n",
                    pgm, pid, statement);
            return 1;
    } /* check statement */
    
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
        fprintf(stderr, "%s/%u| xta_native_xa_resource_new: returned NULL for "
                "dynamically creted resource\n", pgm, pid);
        return 1;
    }
#endif
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        fprintf(stderr, "%s/%u| xta_transaction_manager_new: returned NULL\n",
                pgm, pid);
        return 1;
    }
    /*
     * create an XA native (static) resource object linked to the first Resouce
     * Manager configured in LIXA profile
     */
    if (NULL == (native_xa_res = xta_native_xa_resource_new_by_rmid(
                     0, xta_transaction_manager_get_config()))) {
        fprintf(stderr, "%s/%u| xta_native_xa_resource_new: returned NULL\n",
                pgm, pid);
        return 1;
    }
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        fprintf(stderr, "%s/%u| xta_transaction_manager_begin: returned "
                "NULL\n", pgm, pid);
        return 1;
    } else {
        fprintf(stderr, "%s/%u| xta_transaction_manager_get_transaction: "
                "transaction reference is %p\n", pgm, pid, tx);
    }
    /* register the native XA Resource to the transaction manager: this step
     * is useless but it's not dangerous */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)native_xa_res))) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                "native_xa_res: returned %d\n", pgm, pid, rc);
        return 1;
    }
    /* register the dynamic native XA Resource (Oracle) to the transaction
     * manager */
#ifdef HAVE_ORACLE
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)dynamic_native_xa_res))) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                "dynamic_native_xa_res: returned %d\n", pgm, pid, rc);
        return 1;
    }
#endif

    /* open all the resources for Distributed Transactions */
    if (LIXA_RC_OK != (rc = xta_transaction_open(tx))) {
        fprintf(stderr, "%s/%u| xta_transaction_open: returned %d\n",
                pgm, pid, rc);
        return 1;
    }

    if (SUPERIOR == phase || NO_PHASE == phase) {
        /* start a Distributed Transaction */
        if (LIXA_RC_OK != (rc = xta_transaction_start(tx))) {
            fprintf(stderr, "%s/%u| xta_transaction_start: returned %d\n",
                    pgm, pid, rc);
            return 1;
        }
        /* get XID as a string */
        if (NULL == (xid_string = xta_xid_to_string(
                         xta_transaction_get_xid(tx)))) {
            fprintf(stderr, "%s/%u| xta XID is NULL\n", pgm, pid);
            return 1;
        } else {
            fprintf(stderr, "%s/%u| passing XID '%s' to subordinate\n",
                    pgm, pid, xid_string);
        }
        if (SUPERIOR == phase) {
            /* write to xid_file the transaction that will be branched */
            fprintf(xid_file, "%s", xid_string);
            fclose(xid_file);
            xid_file = NULL;
        }
        /* release xid_string */
        free(xid_string);
        xid_string = NULL;
    } else {
        char buffer[1000];
        /* read from xid_file the transaction that must be resumed */
        if (NULL == fgets(buffer, sizeof(buffer), xid_file)) {
            fprintf(stderr, "%s/%u| error while retrieving XID from file "
                    "'%s'\n", pgm, pid, filename);
            return 1;
        }
        fprintf(stderr, "%s/%u| retrieved XID is '%s'\n", pgm, pid, buffer);
        fclose(xid_file);
        
        /* branch the transaction */
        if (LIXA_RC_OK != (rc = xta_transaction_branch(
                               tx, buffer))) {
            fprintf(stderr, "%s/%u| xta_transaction_branch returned %d\n",
                    pgm, pid, rc);
            return 1;
        }

        /* write to xid_file2 the transaction that will be branched again */
        if (NULL != xid_file2) {
            fprintf(stderr, "%s/%u| passing XID '%s' to subordinate\n",
                    pgm, pid, buffer);
            fprintf(xid_file2, "%s", buffer);
            fclose(xid_file2);
            xid_file2 = NULL;
        }
    }
    
#ifdef HAVE_ORACLE
    /* retrieve environment and context */
    if (NULL == (oci_env = xaoEnv(NULL))) {
        fprintf(stderr, "%s/%u| xaoEnv returned a NULL pointer\n", pgm, pid);
        return 1;
    }
    if (NULL == (oci_svc_ctx = xaoSvcCtx(NULL))) {
        fprintf(stderr, "%s/%u| xaoSvcCtx returned a NULL pointer\n",
                pgm, pid);
        return 1;
    }
    /* allocate statement and error handles */
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_stmt_hndl,
                             OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "%s/%u| Unable to allocate OCI statement handle\n",
                pgm, pid);
        return 1;
    }
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_err_hndl,
                             OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "%s/%u| Unable to allocate OCI error handle\n",
                pgm, pid);
        return 1;
    }
    /* insert data */
    if (insert) {
        if (OCI_SUCCESS != OCIStmtPrepare(
                oci_stmt_hndl, oci_err_hndl, oci_stmt_insert,
                (ub4) strlen((char *)oci_stmt_insert),
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
            fprintf(stderr, "%s/%u| Unable to prepare INSERT OCI statement "
                    "for execution\n", pgm, pid);
            return 1;
        }
        oci_rc = OCIStmtExecute(
            oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
            (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
            (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != oci_rc && OCI_SUCCESS_WITH_INFO != oci_rc) {
            fprintf(stderr, "%s/%u| Error while executing INSERT statement; "
                    "ocirc = %d\n", pgm, pid, oci_rc);
            return oci_rc;
        }
        fprintf(stderr, "%s/%u| OCI statement >%s< completed\n",
                pgm, pid, (char *)oci_stmt_insert);
    } else {
        if (OCI_SUCCESS != OCIStmtPrepare(
                oci_stmt_hndl, oci_err_hndl, oci_stmt_delete,
                (ub4) strlen((char *)oci_stmt_delete),
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
            fprintf(stderr, "%s/%u| Unable to prepare DELETE statement for "
                    "execution\n", pgm, pid);
            return 1;
        }
        oci_rc = OCIStmtExecute(
            oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
            (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
            (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != oci_rc && OCI_SUCCESS_WITH_INFO != oci_rc) {
            fprintf(stderr, "%s/%u| Error while executing DELETE statement; "
                    "ocirc = %d\n", pgm, pid, oci_rc);
            return oci_rc;
        }
        fprintf(stderr, "%s/%u| OCI statement >%s< completed\n",
                pgm, pid, (char *)oci_stmt_delete);
    }
    /* free the allocated handles */
    OCIHandleFree((dvoid *)oci_stmt_hndl, (ub4)OCI_HTYPE_STMT);
    OCIHandleFree((dvoid *)oci_err_hndl, (ub4)OCI_HTYPE_ERROR);
#endif /* HAVE_ORACLE */

    /* put a simple delay to allow the progress of the other branches */
    sleep(1);

    /* commit the Distributed Transaction */
    if (commit) {
        if (test_rc != (rc = xta_transaction_commit(tx, FALSE))) {
            fprintf(stderr, "%s/%u| xta_transaction_commit: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            return 1;
        }
        fprintf(stderr, "%s/%u| XTA commit returned %d as expected\n",
                pgm, pid, rc);
    } else {
        if (test_rc != (rc = xta_transaction_rollback(tx))) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            return 1;
        }
        fprintf(stderr, "%s/%u| XTA rollback returned %d as expected\n",
                pgm, pid, rc);
    }
    
    /* close all the resources for Distributed Transactions */
    if (LIXA_RC_OK != (rc = xta_transaction_close(tx))) {
        fprintf(stderr, "%s/%u| xta_transaction_close: returned %d\n",
                pgm, pid, rc);
        return 1;
    }
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
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
    fprintf(stderr, "%s/%u| ...finished\n", pgm, pid);
    return 0;
}
