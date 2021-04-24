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



#include "xta/xta.h"
#include "case_test_functions.h"



/*
 * Pseudo synchronous branch case test for XTA
 *
 * This is similar to case0106, but:
 * - without Oracle DBMS to avoid some long locks that are not practicals for
 *   automatic testing
 * - optional MonkeyRM (without MonkeyRM superior is just MySQL and
 *   subordinate only PostgreSQL)
 * - subordinate open the connection to the LIXA state server after the
 *   emulated RPC call from the superior
 *
 * NOTE: this is not a good example to learn the C programming language
 *       because the usage of global variables has been abused and you should
 *       not write software as this one.
 *       A huge amount of global variables has been used to avoid parameters
 *       for boilerplate functions: the meaning of the flow should be more
 *       evident without useless details.
 */



/*
 * EXIT CODES:
 *  0:   OK
 *  1:   generic error
 *  2:   superior branch / xta_transaction_start() error
 *  3:   subordinate branch / xta_transaction_branch() error
 *  4:   superior branch / xta_transaction_commit() generic error
 *  5:   superior branch / xta_transaction_commit() -> LIXA_RC_TX_ROLLBACK
 *  6:   subordinate branch / xta_transaction_commit() -> LIXA_RC_TX_ROLLBACK
 *  7:   superior branch / xta_transaction_commit() -> LIXA_RC_TX_MIXED
 *  8:   subordinate branch / xta_transaction_commit() -> LIXA_RC_TX_MIXED
 *  9:   superior branch / xta_transaction_commit() -> LIXA_RC_TX_HAZARD
 * 10:   subordinate branch / xta_transaction_commit() -> LIXA_RC_TX_HAZARD
 * 11:   superior branch / xta_transaction_rollback() generic error
 * 12:   subordinate branch / xta_transaction_rollback() generic error
 * 13:   subordinate branch / xta_transaction_commit() generic error
 * 14:   xta_transaction_close() generic error
 * 15:   subordinate branch / xta_transaction_commit() -> LIXA_RC_MULTIBRANCH_PREPARE_FAILED
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
void use_xa_resources(void);
void reply_to_superior(const char *msg);
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
/* MySQL variables */
#ifdef HAVE_MYSQL
xta_mysql_xa_resource_t *mysql_xa_res;
MYSQL *mysql_conn = NULL;
char  *mysql_stmt_insert1 =
    "INSERT INTO authors VALUES(1899, 'Hemingway', 'Ernest')";
char  *mysql_stmt_delete1 = "DELETE FROM authors WHERE id=1899";
char  *mysql_stmt_insert2 =
    "INSERT INTO authors VALUES(1916, 'Saviane', 'Giorgio')";
char  *mysql_stmt_delete2 = "DELETE FROM authors WHERE id=1916";
char  *mysql_stmt_insert = NULL;
char  *mysql_stmt_delete = NULL;
#endif
/* PostgreSQL variables */
#ifdef HAVE_POSTGRESQL
xta_postgresql_xa_resource_t *postgresql_xa_res;
PGconn *postgres_conn = NULL;
PGresult *postgres_res;
char *postgres_stmt_insert1 = "INSERT INTO authors VALUES("
    "1929, 'Kundera', 'Milan');";
char *postgres_stmt_delete1 = "DELETE FROM authors WHERE id=1929;";
char *postgres_stmt_insert2 = "INSERT INTO authors VALUES("
    "1952, 'Gaarder', 'Jostein');";
char *postgres_stmt_delete2 = "DELETE FROM authors WHERE id=1952;";
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
    /* check if the seventh parameter is available */
    if (argc > 7)
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
    /*
     * interesting code for XTA branching
     */
    /* start a new Distributed Transaction */
    rc = xta_transaction_start(tx, TRUE);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_start: returned %d\n",
                pgm, pid, rc);
        exit(2);
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
    use_xa_resources();
    /* the application can decide to commit or rollback the transaction */
    if (commit) {
        /* commit is performed with "non_block" flag set to FALSE: this is
           necessary to synchronize with the subordinate branch */
        rc = xta_transaction_commit(tx, FALSE);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_commit returned %d (%s)\n",
                    pgm, pid, rc, lixa_strerror(rc));
            if (rc == LIXA_RC_TX_ROLLBACK)
                exit(5);
            else if (rc == LIXA_RC_TX_MIXED)
                exit(7);
            else if (rc == LIXA_RC_TX_HAZARD)
                exit(9);
            else
                exit(4);
        }
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d (%s)"
                    "\n", pgm, pid, rc, lixa_strerror(rc));
            exit(11);
        }
    }
    /* final boilerplate code */
    delete_transaction_manager();    
    delete_all_xa_resources();
}



void subordinate(void)
{
    fprintf(stderr, "%s/%u| branch_type=%d (SUBORDINATE)\n",
            pgm, pid, branch_type);
    /*
     * interesting code for XTA branching
     */
    /* this "message receive" emulates an incoming call request */
    if (msg_receive(fifo_request, request_buffer, sizeof(request_buffer))) {
        fprintf(stderr, "%s/%u| error receiving XID from superior branch\n",
                pgm, pid);
        exit(1);
    }
    /* initial boilerplate code */
    create_dynamic_native_xa_resources();
    create_a_new_transaction_manager();
    create_a_new_transaction();
    enlist_resources_to_transaction();
    /*
     * CREATE A NEW BRANCH IN THE SAME GLOBAL TRANSACTION
     */
    rc = xta_transaction_branch(tx, request_buffer);
    if (rc != LIXA_RC_OK) {
        fprintf(stderr, "%s/%u| xta_transaction_branch returned %d\n",
                pgm, pid, rc);
        exit(3);
    }
    /* upadate XA resources under the control of XTA */
    use_xa_resources();
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
                if (rc == LIXA_RC_TX_ROLLBACK)
                    exit(6);
                else if (rc == LIXA_RC_TX_MIXED)
                    exit(8);
                else if (rc == LIXA_RC_TX_HAZARD)
                    exit(10);
                else
                    exit(13);
            }
        } else {
            fprintf(stderr, "%s/%u| xta_transaction_commit (first "
                    "phase) returned %d (%s)\n",
                    pgm, pid, rc, lixa_strerror(rc));
            if (rc == LIXA_RC_TX_ROLLBACK)
                exit(6);
            else if (rc == LIXA_RC_MULTIBRANCH_PREPARE_FAILED)
                exit(15);
            else
                exit(1);
        }
    } else {
        rc = xta_transaction_rollback(tx);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_rollback: returned %d (%s)"
                    "\n", pgm, pid, rc, lixa_strerror(rc));
            exit(12);
        }
        /* return to superior branch ROLLBACK message */
        reply_to_superior("ROLLBACK");
    }
    /* final boilerplate code */
    delete_transaction_manager();    
    delete_all_xa_resources();
}



void statements_setup(void)
{
    /* choose the statements that must be executed to avoid conflicts
       among concurrent branches */
    switch (statement % 2) {
        case 0:
#ifdef HAVE_MYSQL
            mysql_stmt_insert = mysql_stmt_insert2;
            mysql_stmt_delete = mysql_stmt_delete2;
#endif
#ifdef HAVE_POSTGRESQL
            postgres_stmt_insert = postgres_stmt_insert2;
            postgres_stmt_delete = postgres_stmt_delete2;
#endif
            break;
        case 1:
#ifdef HAVE_MYSQL
            mysql_stmt_insert = mysql_stmt_insert1;
            mysql_stmt_delete = mysql_stmt_delete1;
#endif
#ifdef HAVE_POSTGRESQL
            postgres_stmt_insert = postgres_stmt_insert1;
            postgres_stmt_delete = postgres_stmt_delete1;
#endif
            break;
    } /* switch (statement % 2) */
}



void create_dynamic_native_xa_resources()
{
    /*
     * dynamically create an XA native resource object for the LIXA
     * Monkey Resource Manager (test & debugging tool)
     */
    if (NULL != monkeyrm_config) {
        dynamic_native_xa_res_monkey = xta_native_xa_resource_new(
            "LIXA Monkey RM (static)",
            "/opt/lixa/lib/switch_lixa_monkeyrm_stareg.so",
            monkeyrm_config, "FREE MEMORY");
        if (dynamic_native_xa_res_monkey == NULL) {
            fprintf(stderr, "%s/%u| xta_native_xa_resource_new: returned NULL "
                    "for dynamically creted resource\n", pgm, pid);
            exit(1);
        }
    }
#ifdef HAVE_MYSQL
    if (branch_type == SUPERIOR) {
        /*
         * create a MySQL native connection
         */
        if (NULL == (mysql_conn = mysql_init(NULL))) {
            fprintf(stderr, "%s/%u| mysql_init: returned NULL\n", pgm, pid);
            exit(1);
        }
        if (NULL == mysql_real_connect(mysql_conn, "localhost", "lixa", "",
                                       "lixa", 0, NULL, 0)) {
            fprintf(stderr, "%s/%u| mysql_real_connect: returned error: "
                    "%u, %s\n", pgm, pid, mysql_errno(mysql_conn),
                    mysql_error(mysql_conn));
            exit(1);
        }
        /*
         * create a MySQL XA resource object
         */
        if (NULL == (mysql_xa_res = xta_mysql_xa_resource_new(
                         mysql_conn, "MySQL", "localhost,0,lixa,,lixa"))) {
            fprintf(stderr, "%s/%u| xta_mysql_xa_resource_new: returned "
                    "NULL\n", pgm, pid);
            exit(1);
        }
    } /* if (branch_type == SUPERIOR) */
#endif
#ifdef HAVE_POSTGRESQL
    if (branch_type == SUBORDINATE) {
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
    } /* if (branch_type == SUBORDINATE) */
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
    if (NULL != monkeyrm_config) {
        rc = xta_transaction_enlist_resource(
            tx, (xta_xa_resource_t *)dynamic_native_xa_res_monkey);
        if (rc != LIXA_RC_OK) {
            fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                    "dynamic_native_xa_res_monkey: returned %d\n",
                    pgm, pid, rc);
            exit(1);
        }
    }
#ifdef HAVE_MYSQL
    if (branch_type == SUPERIOR) {
        /* register the MySQL XA Resource to the transaction manager */
        if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                               tx, (xta_xa_resource_t *)mysql_xa_res))) {
            fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                    "mysql_xa_res: returned %d\n", pgm, pid, rc);
            exit(1);
        }
    } /* if (branch_type == SUPERIOR) */
#endif    
#ifdef HAVE_POSTGRESQL
    if (branch_type == SUBORDINATE) {
        /* register the PostgreSQL XA Resource to the transaction manager */
        if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                               tx, (xta_xa_resource_t *)postgresql_xa_res))) {
            fprintf(stderr, "%s/%u| xta_transaction_enlist_resource/"
                    "postgresql_xa_res: returned %d\n", pgm, pid, rc);
            exit(1);
        }
    } /* if (branch_type == SUBORDINATE) */
#endif    
}



void use_xa_resources(void)
{
#ifdef HAVE_MYSQL
    if (branch_type == SUPERIOR) {
        /* insert data */
        if (insert) {
            fprintf(stderr, "%s/%u| MySQL executing statement >%s<\n",
                    pgm, pid, mysql_stmt_insert);
            if (mysql_query(mysql_conn, mysql_stmt_insert)) {
                fprintf(stderr, "%s/%u| INSERT INTO authors: %u/%s",
                        pgm, pid, mysql_errno(mysql_conn),
                        mysql_error(mysql_conn));
                mysql_close(mysql_conn);
                exit(1);
            }
            fprintf(stderr, "%s/%u| MySQL statement >%s< completed\n",
                    pgm, pid, mysql_stmt_insert);
        } else {
            fprintf(stderr, "%s/%u| MySQL executing statement >%s<\n",
                   pgm, pid, mysql_stmt_delete);
            if (mysql_query(mysql_conn, mysql_stmt_delete)) {
                fprintf(stderr, "%s/%u| DELETE FROM authors: %u/%s",
                        pgm, pid, mysql_errno(mysql_conn),
                        mysql_error(mysql_conn));
                mysql_close(mysql_conn);
                exit(1);
            }
            fprintf(stderr, "%s/%u| MySQL statement >%s< completed\n",
                   pgm, pid, mysql_stmt_delete);
        }
    } /* if (branch_type == SUPERIOR) */
#endif /* HAVE_MYSQL */
#ifdef HAVE_POSTGRESQL
    if (branch_type == SUBORDINATE) {
        if (insert) {
            fprintf(stderr, "%s/%u| PostgreSQL executing statement >%s<\n",
                    pgm, pid, postgres_stmt_insert);
            postgres_res = PQexec(
                postgres_conn, postgres_stmt_insert);
            if (PGRES_COMMAND_OK != PQresultStatus(postgres_res)) {
                fprintf(stderr, "%s/%u| error while executing >%s< %s\n",
                        pgm, pid, postgres_stmt_insert,
                        PQerrorMessage(postgres_conn));
                PQclear(postgres_res);
                PQfinish(postgres_conn);
                exit(1);
            }
            PQclear(postgres_res);
            fprintf(stderr, "%s/%u| PostgreSQL statement >%s< completed\n",
                    pgm, pid, postgres_stmt_insert);
        } else {
            fprintf(stderr, "%s/%u| PostgreSQL executing statement >%s<\n",
                    pgm, pid, postgres_stmt_delete);
            postgres_res = PQexec(
                postgres_conn, postgres_stmt_delete);
            if (PGRES_COMMAND_OK != PQresultStatus(postgres_res)) {
                fprintf(stderr, "%s/%u| error while executing >%s< %s\n",
                        pgm, pid, postgres_stmt_delete,
                        PQerrorMessage(postgres_conn));
                PQclear(postgres_res);
                PQfinish(postgres_conn);
                exit(1);
            }
            PQclear(postgres_res);
            fprintf(stderr, "%s/%u| PostgreSQL statement >%s< completed\n",
                    pgm, pid, postgres_stmt_delete);
        }
    } /* if (branch_type == SUBORDINATE) */
#endif /* HAVE_POSTGRESQL */    
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
    if (branch_type == SUBORDINATE) {
        /*
         * delete the PostgreSQL XA resource object
         */
        xta_postgresql_xa_resource_delete(postgresql_xa_res);
        /*
         * close PostgreSQL database connection
         */
        PQfinish(postgres_conn);
    } /* if (branch_type == SUBORDINATE) */
#endif    
#ifdef HAVE_MYSQL
    if (branch_type == SUPERIOR) {
        /*
         * delete the MySQL XA resource object
         */
        xta_mysql_xa_resource_delete(mysql_xa_res);
        /*
         * close MySQL database connection
         */
        mysql_close(mysql_conn);
    } /* if (branch_type == SUPERIOR) */
#endif    
    /*
     * delete native XA Resource object
     */
    if (NULL != monkeyrm_config)
        xta_native_xa_resource_delete(dynamic_native_xa_res_monkey);
}
