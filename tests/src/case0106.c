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
#include "case_test_functions.h"



/*
 * Synchronous branch case test for XTA
 *
 * NOTE: this is not a good example to learn the C programming language
 *       because the usage of global variables has been abused and you should
 *       not write software as this one.
 *       A huge amount of global variables has been used to avoid parameters
 *       for boilerplate functions: the meaning of the flow should be more
 *       evident without useless details.
 */



/*
 * Boilerplate functions: they are defined only to improve the readability of
 * the interesting part of the program
 */
void statements_setup(void);
void create_dynamic_native_xa_resources();
void create_a_new_transaction_manager(void);
void create_a_new_transaction(void);
void enlist_resources_to_transaction(void);
void open_all_the_resources(void);
void use_xa_resources(void);
void reply_to_superior(const char *msg);
void close_all_the_resources(void);
void delete_transaction_manager(void);
void delete_all_xa_resources(void);



/*
 * These are the MOST interesting functions:
 * - superior implements the logic of the superior branch
 * - subordinate implements the logic of the subordinate branch
 */
void superior(void);
void subordinate(void);



/*
 * Common variables: they are declared here to avoid parameter passing and
 * to improve readability
 */
int      rc;
char    *pgm = NULL;
unsigned pid;
enum BranchType { SUPERIOR, SUBORDINATE, NO_BRANCH_TYPE } branch_type;
/* XTA variables (objects) */
xta_transaction_manager_t *tm;
xta_transaction_t *tx;
xta_native_xa_resource_t *dynamic_native_xa_res_monkey;
xta_native_xa_resource_t *dynamic_native_xa_res_ora;
/* work variables */
char response_buffer[100];
char request_buffer[100];
/* control variables */
int         statement;
int         commit;
int         insert;
const char *fifo_request = NULL;
const char *fifo_reply = NULL;
const char *monkeyrm_config = NULL;
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
text          *oci_stmt_insert = NULL;
text          *oci_stmt_delete = NULL;
#endif



int main(int argc, char *argv[])
{
    /* boilerplate code, nothing really interesting... */
    pid = (unsigned)getpid();
    /* turn ON trace for debugging purpose */
    xta_init();

    /* parse command line parameters */
    pgm = argv[0];
    fprintf(stderr, "%s/%u| starting...\n", pgm, pid);
    if (argc < 8) {
        fprintf(stderr, "%s/%u: at least seven options must be specified\n",
                argv[0], pid);
        return 1;
    }
    branch_type = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    statement = strtol(argv[3], NULL, 0);
    commit = strtol(argv[4], NULL, 0);
    fifo_request = argv[5];
    fifo_reply = argv[6];
    monkeyrm_config = argv[7];

    /* choose the SQL statements that must be executed by this branch */
    statements_setup();
    
    /* check branch_type */
    switch (branch_type) {
        case SUPERIOR:
            /* this is the superior task: it calls subordinate */
            superior();
            break;
        case SUBORDINATE:
            /* this is the subordinate task: it's called by superior */
            subordinate();
            break;
        default:
            fprintf(stderr, "%s/%u| branch_type=%d UNKNOWN!\n",
                    pgm, pid, branch_type);
            exit(1);
    } /* switch(branch_type) */


    /*
     * end of XTA API calls
     */
    fprintf(stderr, "%s/%u| ...finished\n", pgm, pid);
    return 0;
}



void superior(void)
{
    char *xid_string = NULL;
    
    fprintf(stderr, "%s/%u| branch_type=%d (SUPERIOR)\n",
            pgm, pid, branch_type);
    /* initial boilerplate code */
    create_dynamic_native_xa_resources();
    create_a_new_transaction_manager();
    create_a_new_transaction();
    enlist_resources_to_transaction();
    open_all_the_resources();
    /*
     * interesting code for XTA branching
     */
    /* start a new Distributed Transaction */
    rc = xta_transaction_start(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_start: returned %d\n",
                pgm, pid, rc);
        exit(1);
    }
    /* retrieve the XID associated to the started transaction */
    xid_string = xta_xid_to_string(xta_transaction_get_xid(tx));
    if (xid_string == NULL) {
        fprintf(stderr, "%s/%u| xta XID is NULL\n", pgm, pid);
        exit(1);
    }
    /* calling subordinate passig the XID */
    fprintf(stderr, "%s/%u| calling subordinate task and passing XID '%s'\n",
            pgm, pid, xid_string);
    /* a typical Remote Procedure Call is emulated with a synchronous
       message passing */
    if (msg_send(fifo_request, xid_string)) {
        fprintf(stderr, "%s/%u| error calling subordinate task (msg_send)\n",
                pgm, pid);
        exit(1);
    }
    if (msg_receive(fifo_reply, response_buffer, sizeof(response_buffer))) {
        fprintf(stderr, "%s/%u| error calling subordinate task (msg_receive)"
                "\n", pgm, pid);
        exit(1);
    }
    /* release xid_string */
    free(xid_string); xid_string = NULL;
    /* subordinate call succeeded */
    fprintf(stderr, "%s/%u| subordinate task replied '%s'\n",
            pgm, pid, response_buffer);
    /* use XA resources under the control of XTA */
    // use_xa_resources(); @@@
    /* the application can decide to commit or rollback the transaction */
    if (commit) {
        /* commit is performed with "non_block" flag set to FALSE: this is
           necessary to synchronize with the subordinate branch */
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_commit returned %d (%s)\n",
                    pgm, pid, rc, lixa_strerror(rc));
            exit(1);
        }
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d (%s)"
                    "\n", pgm, pid, rc, lixa_strerror(rc));
            exit(1);
        }
    }
    /* final boilerplate code */
    close_all_the_resources();
    delete_transaction_manager();    
    delete_all_xa_resources();
}



void subordinate(void)
{
    fprintf(stderr, "%s/%u| branch_type=%d (SUBORDINATE)\n",
            pgm, pid, branch_type);
    /* initial boilerplate code */
    create_dynamic_native_xa_resources();
    create_a_new_transaction_manager();
    create_a_new_transaction();
    enlist_resources_to_transaction();
    open_all_the_resources();
    /*
     * interesting code for XTA branching
     */
    /* this "message receive" emulates an incoming call request */
    if (msg_receive(fifo_request, request_buffer, sizeof(request_buffer))) {
        fprintf(stderr, "%s/%u| error receiving XID from superior branch\n",
                pgm, pid);
        exit(1);
    }
    /*
     * CREATE A NEW BRANCH IN THE SAME GLOBAL TRANSACTION
     */
    rc = xta_transaction_branch(tx, request_buffer);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_branch returned %d\n",
                pgm, pid, rc);
        exit(1);
    }
    /* upadate XA resources under the control of XTA */
    // use_xa_resources(); @@@
    /* the application can decide to commit or rollback the transaction */
    if (commit) {
        /* commit is performed with "non_block" flag set to TRUE: this is
           necessary to allow the superior branch to commit */
        rc = xta_transaction_commit(tx, TRUE);
        /* xta_transaction_commit should return LIXA_RC_WOULD_BLOCK */
        if (rc == LIXA_RC_WOULD_BLOCK) {
            /* at this point the branch has been prepared and the caller can
               be informed to start commit itself */
            reply_to_superior("PREPARED for COMMIT");
            /* now the commit can be completed */
            rc = xta_transaction_commit(tx, FALSE);
            if (rc != LIXA_RC_OK) {
                fprintf(stderr, "%s/%u| xta_transaction_commit (second "
                        "phase) returned %d (%s)\n",
                        pgm, pid, rc, lixa_strerror(rc));
                exit(1);
            }
        } else {
            fprintf(stderr, "%s/%u| xta_transaction_commit (first "
                    "phase) returned %d (%s)\n",
                    pgm, pid, rc, lixa_strerror(rc));
            exit(1);
        }
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d (%s)"
                    "\n", pgm, pid, rc, lixa_strerror(rc));
            exit(1);
        }
        /* return to superior branch ROLLBACK message */
        reply_to_superior("ROLLBACK");
    }
    /* final boilerplate code */
    close_all_the_resources();
    delete_transaction_manager();    
    delete_all_xa_resources();
}



void statements_setup(void)
{
    /* choose the statements that must be executed to avoid conflicts
       among concurrent branches */
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
        default:
            fprintf(stderr, "%s/%u| statement=%d is not valid!\n",
                    pgm, pid, statement);
            exit(1);
    } /* check statement */
}



void create_dynamic_native_xa_resources()
{
    /*
     * dynamically create an XA native resource object for the LIXA
     * Monkey Resource Manager (test & debugging tool)
     */
    dynamic_native_xa_res_monkey = xta_native_xa_resource_new(
        "LIXA Monkey RM (static)",
        "/opt/lixa/lib/switch_lixa_monkeyrm_stareg.so",
        monkeyrm_config, "");
    if (dynamic_native_xa_res_monkey == NULL) {
        fprintf(stderr, "%s/%u| xta_native_xa_resource_new: returned NULL for "
                "dynamically creted resource\n", pgm, pid);
        exit(1);
    }
    /*
     * dynamically create an XA native resource object for Oracle database
     */
#ifdef HAVE_ORACLE
    dynamic_native_xa_res_ora = xta_native_xa_resource_new(
        "OracleIC_stareg",
        "/opt/lixa/lib/switch_oracle_stareg.so",
        "ORACLE_XA+Acc=P/hr/hr+SesTm=30+LogDir=/tmp+"
        "threads=true+DbgFl=7+SqlNet=lixa_ora_db+"
        "Loose_Coupling=true", "");
    if (dynamic_native_xa_res_ora == NULL) {
        fprintf(stderr, "%s/%u| xta_native_xa_resource_new: returned NULL for "
                "dynamically creted resource\n", pgm, pid);
        exit(1);
    }
#endif
}



void create_a_new_transaction_manager(void)
{
    /*
     * create a Transaction Manager object
     */
    tm = xta_transaction_manager_new();
    if (tm == NULL) {
        fprintf(stderr, "%s/%u| xta_transaction_manager_new: returned NULL\n",
                pgm, pid);
        exit(1);
    }
}



void create_a_new_transaction(void)
{
    /* create a new transaction for this thread */
    tx = xta_transaction_manager_create_transaction(tm);
    if (tx == NULL) {
        fprintf(stderr, "%s/%u| xta_transaction_manager_begin: returned "
                "NULL\n", pgm, pid);
        exit(1);
    } else {
        fprintf(stderr, "%s/%u| xta_transaction_manager_get_transaction: "
                "transaction reference is %p\n", pgm, pid, tx);
    }
}



void enlist_resources_to_transaction(void)
{
    /* enlist the dynamic native XA Resources to the transaction */
    rc = xta_transaction_enlist_resource(
        tx, (xta_xa_resource_t *)dynamic_native_xa_res_monkey);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                "dynamic_native_xa_res_monkey: returned %d\n", pgm, pid, rc);
        exit(1);
    }
#ifdef HAVE_ORACLE
    rc = xta_transaction_enlist_resource(tx,
        (xta_xa_resource_t *)dynamic_native_xa_res_ora);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                "dynamic_native_xa_res_ora: returned %d\n", pgm, pid, rc);
        exit(1);
    }
#endif
}



void open_all_the_resources(void)
{
    /* open all the resources for Distributed Transactions */
    rc = xta_transaction_open(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_open: returned %d\n",
                pgm, pid, rc);
        exit(1);
    }
}



void use_xa_resources(void)
{
#ifdef HAVE_ORACLE
    /* retrieve environment and context */
    oci_env = xaoEnv(NULL);
    if (oci_env == NULL) {
        fprintf(stderr, "%s/%u| xaoEnv returned a NULL pointer\n", pgm, pid);
        exit(1);
    }
    oci_svc_ctx = xaoSvcCtx(NULL);
    if (oci_svc_ctx == NULL) {
        fprintf(stderr, "%s/%u| xaoSvcCtx returned a NULL pointer\n",
                pgm, pid);
        exit(1);
    }
    /* allocate statement and error handles */
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_stmt_hndl,
                             OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "%s/%u| Unable to allocate OCI statement handle\n",
                pgm, pid);
        exit(1);
    }
    if (0 != OCIHandleAlloc( (dvoid *)oci_env, (dvoid **)&oci_err_hndl,
                             OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
        fprintf(stderr, "%s/%u| Unable to allocate OCI error handle\n",
                pgm, pid);
        exit(1);
    }
    /* insert data */
    if (insert) {
        if (OCI_SUCCESS != OCIStmtPrepare(
                oci_stmt_hndl, oci_err_hndl, oci_stmt_insert,
                (ub4) strlen((char *)oci_stmt_insert),
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
            fprintf(stderr, "%s/%u| Unable to prepare INSERT OCI statement "
                    "for execution\n", pgm, pid);
            exit(1);
        }
        oci_rc = OCIStmtExecute(
            oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
            (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
            (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != oci_rc && OCI_SUCCESS_WITH_INFO != oci_rc) {
            fprintf(stderr, "%s/%u| Error while executing INSERT statement; "
                    "ocirc = %d\n", pgm, pid, oci_rc);
            exit(oci_rc);
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
            exit(1);
        }
        oci_rc = OCIStmtExecute(
            oci_svc_ctx, oci_stmt_hndl, oci_err_hndl,
            (ub4)1, (ub4)0, (CONST OCISnapshot *)NULL,
            (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != oci_rc && OCI_SUCCESS_WITH_INFO != oci_rc) {
            fprintf(stderr, "%s/%u| Error while executing DELETE statement; "
                    "ocirc = %d\n", pgm, pid, oci_rc);
            exit(oci_rc);
        }
        fprintf(stderr, "%s/%u| OCI statement >%s< completed\n",
                pgm, pid, (char *)oci_stmt_delete);
    }
    /* free the allocated handles */
    OCIHandleFree((dvoid *)oci_stmt_hndl, (ub4)OCI_HTYPE_STMT);
    OCIHandleFree((dvoid *)oci_err_hndl, (ub4)OCI_HTYPE_ERROR);
#endif /* HAVE_ORACLE */
}



void reply_to_superior(const char *msg)
{
    /* this "message send" emulates the response to the caller after activity
       "completion" */
    if (msg_send(fifo_reply, msg)) {
        fprintf(stderr, "%s/%u| error replying OK to superior branch\n",
                pgm, pid);
        exit(1);
    }        
}



void close_all_the_resources(void)
{
    /* close all the resources for Distributed Transactions */
    rc = xta_transaction_close(tx);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_close: returned %d\n",
                pgm, pid, rc);
        exit(1);
    }
}



void delete_transaction_manager(void)
{
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
}



void delete_all_xa_resources(void)
{
#ifdef HAVE_ORACLE
    /*
     * delete dynamically created native XA Resource object for Oracle
     */
    xta_native_xa_resource_delete(dynamic_native_xa_res_ora);
#endif 
    /*
     * delete native XA Resource object
     */
    xta_native_xa_resource_delete(dynamic_native_xa_res_monkey);
}
