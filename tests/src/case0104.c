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
 * - superior implements the logic of the superior branch: it passes the
 *   transactional context to intermediate
 * - intermediate implements the logic of the intermediate branch: it receives
 *   the transactional context by superior and it passes it to subordinate
 * - subordinate implements the logic of the subordinate branch: it receives
 *   the transactional context by intermediate
 */
void superior(void);
void intermediate(void);
void subordinate(void);



/*
 * Common variables: they are declared here to avoid parameter passing and
 * to improve readability
 */
char *pgm = NULL;
int rc;
char *xid_string = NULL;
FILE *xid_file = NULL;
FILE *xid_file2 = NULL;
unsigned pid;

/* XTA variables (objects) */
xta_transaction_manager_t *tm;
xta_transaction_t *tx;
xta_native_xa_resource_t *dynamic_native_xa_res_monkey;
xta_native_xa_resource_t *dynamic_native_xa_res_ora;
/* work variables */
char request_buffer[100];
/* control variables */
enum BranchType { SUPERIOR, INTERMEDIATE,
                  SUBORDINATE, NO_BRANCH_TYPE } branch_type;
int        commit;
int        insert;
int        statement;
int        test_rc;
const char *fifo_to = NULL;
const char *fifo_from = NULL;
const char *monkeyrm_config = NULL;
/* MySQL variables */
#ifdef HAVE_MYSQL
xta_mysql_xa_resource_t *mysql_xa_res;
MYSQL *mysql_conn = NULL;
char  *mysql_stmt_insert1 =
    "INSERT INTO authors VALUES(101, 'Ernest', 'Hemingway')";
char  *mysql_stmt_delete1 = "DELETE FROM authors WHERE id=101";
char  *mysql_stmt_insert2 =
    "INSERT INTO authors VALUES(102, 'Giorgio', 'Saviane')";
char  *mysql_stmt_delete2 = "DELETE FROM authors WHERE id=102";
char  *mysql_stmt_insert3 =
    "INSERT INTO authors VALUES(103, 'Philip', 'Roth')";
char  *mysql_stmt_delete3 = "DELETE FROM authors WHERE id=103";
char  *mysql_stmt_insert = NULL;
char  *mysql_stmt_delete = NULL;
#endif
/* Oracle variables */
#ifdef HAVE_ORACLE
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
/* PostgreSQL variables */
#ifdef HAVE_POSTGRESQL
xta_postgresql_xa_resource_t *postgresql_xa_res;
PGconn *postgres_conn = NULL;
PGresult *postgres_res;
char *postgres_stmt_insert1 = "INSERT INTO authors VALUES("
    "101, 'Milan', 'Kundera');";
char *postgres_stmt_delete1 = "DELETE FROM authors WHERE id=101;";
char *postgres_stmt_insert2 = "INSERT INTO authors VALUES("
    "102, 'Jostein', 'Gaarder');";
char *postgres_stmt_delete2 = "DELETE FROM authors WHERE id=102;";
char *postgres_stmt_insert3 = "INSERT INTO authors VALUES("
    "103, 'Patrick', 'Suskind');";
char *postgres_stmt_delete3 = "DELETE FROM authors WHERE id=103;";
char *postgres_stmt_insert = NULL;
char *postgres_stmt_delete = NULL;
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
    if (argc < 7) {
        fprintf(stderr, "%s/%u: at least six options must be specified\n",
                argv[0], pid);
        return 1;
    }
    branch_type = strtol(argv[1], NULL, 0);
    insert = strtol(argv[2], NULL, 0);
    statement = strtol(argv[3], NULL, 0);
    commit = strtol(argv[4], NULL, 0);
    test_rc = strtol(argv[5], NULL, 0);
    fifo_to = argv[6];
    fifo_from = argv[7];
    monkeyrm_config = argv[8];

    /* choose the SQL statements that must be executed by this branch */
    statements_setup();
    
    /* check branch_type */
    switch (branch_type) {
        case SUPERIOR:
            /* this is the superior task: it passes the transactional
             * context to intermediate */
            superior();
            break;
        case INTERMEDIATE:
            /* this is the intermediate task: it receives the transactional
             * context by superior and it passes it to subordinate */
            intermediate();
            break;
        case SUBORDINATE:
            /* this is the subordinate task: it receives the transactional
             * context by intermediate */
            subordinate();
            break;
        default:
            fprintf(stderr, "%s/%u| branch_type=%d UNKNOWN\n",
                    pgm, pid, branch_type);
            exit(1);
    } /* switch(branch_type) */    
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
    fprintf(stderr, "%s/%u| passing XID '%s' to intermediate branch\n",
            pgm, pid, xid_string);
    /* passing the XID to intermediate branch */
    if (msg_send(fifo_to, xid_string)) {
        fprintf(stderr, "%s/%u| error calling subordinate task (msg_send)\n",
                pgm, pid);
        exit(1);
    }
    /* release xid_string */
    free(xid_string); xid_string = NULL;
    /* upadate XA resources under the control of XTA */
    use_xa_resources();
    /* put a simple delay to allow the progress of the other branches */
    sleep(1);
    /* commit or rollback the Distributed Transaction */
    if (commit) {
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != test_rc) {
            fprintf(stderr, "%s/%u| xta_transaction_commit: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            exit(1);
        }
        fprintf(stderr, "%s/%u| XTA commit returned %d as expected\n",
                pgm, pid, rc);
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != test_rc) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            exit(1);
        }
        fprintf(stderr, "%s/%u| XTA rollback returned %d as expected\n",
                pgm, pid, rc);
    }
    /* final boilerplate code */
    close_all_the_resources();
    delete_transaction_manager();    
    delete_all_xa_resources();
}



void intermediate(void)
{
    fprintf(stderr, "%s/%u| branch_type=%d (INTERMEDIATE)\n",
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
    /* receiving XID (transactional context) from superior branch */
    if (msg_receive(fifo_from, request_buffer, sizeof(request_buffer))) {
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
    fprintf(stderr, "%s/%u| passing XID '%s' to subordinate branch\n",
            pgm, pid, request_buffer);
    /* passing the XID to intermediate branch */
    if (msg_send(fifo_to, request_buffer)) {
        fprintf(stderr, "%s/%u| error passing XID to subordinate branch "
                "(msg_send)\n", pgm, pid);
        exit(1);
    }
    /* upadate XA resources under the control of XTA */
    use_xa_resources();
    /* put a simple delay to allow the progress of the other branches */
    sleep(1);
    /* commit or rollback the Distributed Transaction */
    if (commit) {
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != test_rc) {
            fprintf(stderr, "%s/%u| xta_transaction_commit: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            exit(1);
        }
        fprintf(stderr, "%s/%u| XTA commit returned %d as expected\n",
                pgm, pid, rc);
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != test_rc) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            exit(1);
        }
        fprintf(stderr, "%s/%u| XTA rollback returned %d as expected\n",
                pgm, pid, rc);
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
    /* receiving XID (transactional context) from intermediate branch */
    if (msg_receive(fifo_from, request_buffer, sizeof(request_buffer))) {
        fprintf(stderr, "%s/%u| error receiving XID from intermediate "
                "branch\n", pgm, pid);
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
    use_xa_resources();
    /* put a simple delay to allow the progress of the other branches */
    sleep(1);
    /* commit or rollback the Distributed Transaction */
    if (commit) {
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != test_rc) {
            fprintf(stderr, "%s/%u| xta_transaction_commit: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            exit(1);
        }
        fprintf(stderr, "%s/%u| XTA commit returned %d as expected\n",
                pgm, pid, rc);
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != test_rc) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d "
                    "instead of %d\n", pgm, pid, rc, test_rc);
            exit(1);
        }
        fprintf(stderr, "%s/%u| XTA rollback returned %d as expected\n",
                pgm, pid, rc);
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
#ifdef HAVE_MYSQL
            mysql_stmt_insert = mysql_stmt_insert1;
            mysql_stmt_delete = mysql_stmt_delete1;
#endif
#ifdef HAVE_ORACLE
            oci_stmt_insert = oci_stmt_insert1;
            oci_stmt_delete = oci_stmt_delete1;
#endif
#ifdef HAVE_POSTGRESQL
            postgres_stmt_insert = postgres_stmt_insert1;
            postgres_stmt_delete = postgres_stmt_delete1;
#endif
            break;
        case 2:
#ifdef HAVE_MYSQL
            mysql_stmt_insert = mysql_stmt_insert2;
            mysql_stmt_delete = mysql_stmt_delete2;
#endif
#ifdef HAVE_ORACLE
            oci_stmt_insert = oci_stmt_insert2;
            oci_stmt_delete = oci_stmt_delete2;
#endif
#ifdef HAVE_POSTGRESQL
            postgres_stmt_insert = postgres_stmt_insert2;
            postgres_stmt_delete = postgres_stmt_delete2;
#endif
            break;
        case 3:
#ifdef HAVE_MYSQL
            mysql_stmt_insert = mysql_stmt_insert3;
            mysql_stmt_delete = mysql_stmt_delete3;
#endif
#ifdef HAVE_ORACLE
            oci_stmt_insert = oci_stmt_insert3;
            oci_stmt_delete = oci_stmt_delete3;
#endif
#ifdef HAVE_POSTGRESQL
            postgres_stmt_insert = postgres_stmt_insert3;
            postgres_stmt_delete = postgres_stmt_delete3;
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
#ifdef HAVE_MYSQL
    /*
     * create a MySQL native connection
     */
    if (NULL == (mysql_conn = mysql_init(NULL))) {
        fprintf(stderr, "%s/%u| mysql_init: returned NULL\n", pgm, pid);
        exit(1);
    }
    if (NULL == mysql_real_connect(mysql_conn, "localhost", "lixa", "",
                                   "lixa", 0, NULL, 0)) {
        fprintf(stderr, "%s/%u| mysql_real_connect: returned error: %u, %s\n",
               pgm, pid, mysql_errno(mysql_conn), mysql_error(mysql_conn));
        exit(1);
    }
    /*
     * create a MySQL XA resource object
     */
    if (NULL == (mysql_xa_res = xta_mysql_xa_resource_new(
                     mysql_conn, "MySQL", "localhost,0,lixa,,lixa"))) {
        fprintf(stderr, "%s/%u| xta_mysql_xa_resource_new: returned NULL\n",
                pgm, pid);
        exit(1);
    }
#endif
#ifdef HAVE_ORACLE
    /*
     * dynamically create an XA native resource object for Oracle database
     */
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
#ifdef HAVE_POSTGRESQL
    /*
     * create a PostgreSQL native connection
     */
    postgres_conn = PQconnectdb("dbname=testdb");
    if (CONNECTION_OK != PQstatus(postgres_conn)) {
        fprintf(stderr, "%s/%u| PQconnectdb: returned error %s\n",
               pgm, pid, PQerrorMessage(postgres_conn));
        PQfinish(postgres_conn);
        exit(1);
    }
    /*
     * create a PostgreSQL XA resource object
     */
    if (NULL == (postgresql_xa_res = xta_postgresql_xa_resource_new(
                     postgres_conn, "PostgreSQL",
                     "dbname=testdb"))) {
        fprintf(stderr, "%s/%u| xta_postgresql_xa_resource_new: returned "
                "NULL\n", pgm, pid);
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
#ifdef HAVE_MYSQL
    /* register the MySQL XA Resource to the transaction manager */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)mysql_xa_res))) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/mysql_xa_res: "
                "returned %d\n", pgm, pid, rc);
        exit(1);
    }
#endif    
#ifdef HAVE_ORACLE
    rc = xta_transaction_enlist_resource(tx,
        (xta_xa_resource_t *)dynamic_native_xa_res_ora);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                "dynamic_native_xa_res_ora: returned %d\n", pgm, pid, rc);
        exit(1);
    }
#endif
#ifdef HAVE_POSTGRESQL
    /* register the PostgreSQL XA Resource to the transaction manager */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)postgresql_xa_res))) {
        fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                "postgresql_xa_res: returned %d\n", pgm, pid, rc);
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
#ifdef HAVE_MYSQL
    /* insert data */
    if (insert) {
        if (mysql_query(mysql_conn, mysql_stmt_insert)) {
            fprintf(stderr, "%s/%u| MySQL INSERT INTO authors: %u/%s",
                    pgm, pid, mysql_errno(mysql_conn),
                    mysql_error(mysql_conn));
            mysql_close(mysql_conn);
            exit(1);
        }
        fprintf(stderr, "%s/%u| MySQL statement >%s< completed\n",
                pgm, pid, mysql_stmt_insert);
    } else {
        if (mysql_query(mysql_conn, mysql_stmt_delete)) {
            fprintf(stderr, "%s/%u| MySQL DELETE FROM authors: %u/%s",
                    pgm, pid, mysql_errno(mysql_conn),
                    mysql_error(mysql_conn));
            mysql_close(mysql_conn);
            exit(1);
        }
        printf("%s/%u| MySQL statement >%s< completed\n",
               pgm, pid, mysql_stmt_delete);
    }
#endif /* HAVE_MYSQL */
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
        /* delete data */
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
#ifdef HAVE_POSTGRESQL
    if (insert) {
        postgres_res = PQexec(
            postgres_conn, postgres_stmt_insert);
        if (PGRES_COMMAND_OK != PQresultStatus(postgres_res)) {
            fprintf(stderr, "%s/%u| PostgreSQL error while executing "
                    ">%s< %s\n", pgm, pid, postgres_stmt_insert,
                    PQerrorMessage(postgres_conn));
            PQclear(postgres_res);
            PQfinish(postgres_conn);
            exit(1);
        }
        PQclear(postgres_res);
        fprintf(stderr, "%s/%u| PostgreSQL statement >%s< completed\n",
                pgm, pid, postgres_stmt_insert);
    } else {
        postgres_res = PQexec(
            postgres_conn, postgres_stmt_delete);
        if (PGRES_COMMAND_OK != PQresultStatus(postgres_res)) {
            fprintf(stderr, "%s/%u| PostgreSQL error while executing "
                    ">%s< %s\n", pgm, pid, postgres_stmt_delete,
                    PQerrorMessage(postgres_conn));
            PQclear(postgres_res);
            PQfinish(postgres_conn);
            exit(1);
        }
        PQclear(postgres_res);
        fprintf(stderr, "%s/%u| PostgreSQL statement >%s< completed\n",
                pgm, pid, postgres_stmt_delete);
    }
#endif /* HAVE_POSTGRESQL */    
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
#ifdef HAVE_ORACLE
    /*
     * delete dynamically created native XA Resource object for Oracle
     */
    xta_native_xa_resource_delete(dynamic_native_xa_res_ora);
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
    /*
     * delete native XA Resource object
     */
    xta_native_xa_resource_delete(dynamic_native_xa_res_monkey);
}
