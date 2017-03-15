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
#include <config.h>



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



/* header files necessary for specific Resource Managers */
#ifdef HAVE_ORACLE
# include <oci.h>
#endif

#ifdef HAVE_MYSQL
# include <mysql.h>
# include "lixamy.h"
#endif

#ifdef HAVE_POSTGRESQL
# include <libpq-fe.h>
# include "lixapq.h"
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_utils.h"
#include "lixa_xid.h"
#include "xa.h"
#include "client_config.h"
#include "client_status.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_GENERIC



/*
 * LIXA Very Stupid Robot
 *
 * This is probably the worst artificial intelligence robot or, alternatively
 * the best artificial stupidity implementation.
 * Nevertheless, the purpose of this utility is the empirical test of the
 * behavior that an XA Resource Manager implements.
 */



#define RECORD_SIZE 1000
#define MAX_THREADS_OF_CONTROL 10



/* default command line options */
static char *filename = NULL;
static gboolean threads = FALSE;
static gboolean print_version = FALSE;
static gboolean sleep_before_kill = FALSE;
static GOptionEntry entries[] =
{
    { "filename", 'f', 0, G_OPTION_ARG_STRING, &filename, "Name of the file with the actions that must be executed", NULL },
    { "threads", 't', 0, G_OPTION_ARG_NONE, &threads, "Use threads instead of processes", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { "sleep", 's', 0, G_OPTION_ARG_NONE, &sleep_before_kill, "Sleep 1 second before killing children at exit", NULL },
    { NULL }
};



/* parsable XA functions: strings */
static const char *PARSABLE_FUNCTIONS[] = {
    "xa_close", "xa_commit", "xa_end", "xa_forget", "xa_open", "xa_prepare",
    "xa_rollback", "xa_start", "rm_ora_exec1", "rm_mys_exec1", "rm_pql_exec1",
    "vsr_quit"
};
/* accepted functions: internal */
typedef enum {
    XA_CLOSE
    , XA_COMMIT
    , XA_END
    , XA_FORGET
    , XA_OPEN
    , XA_PREPARE
    , XA_ROLLBACK
    , XA_START
    , RM_ORA_EXEC1
    , RM_MYS_EXEC1
    , RM_PQL_EXEC1
    , VSR_QUIT
} function_id_t;



/* parsable flags: strings */
static const char *PARSABLE_FLAGS[] = {
    "TMNOFLAGS", "TMASYNC", "TMONEPHASE", "TMFAIL", "TMNOWAIT", "TMRESUME"
    , "TMSUCCESS", "TMSUSPEND", "TMSTARTRSCAN", "TMENDRSCAN", "TMMULTIPLE"
    , "TMJOIN", "TMMIGRATE"
};
static const long FLAG_VALUES[] = {
    TMNOFLAGS, TMASYNC, TMONEPHASE, TMFAIL, TMNOWAIT, TMRESUME
    , TMSUCCESS, TMSUSPEND, TMSTARTRSCAN, TMENDRSCAN, TMMULTIPLE
    , TMJOIN, TMMIGRATE
};



/* parsed function internal representation */
typedef struct {
    function_id_t            fid;
    char                     info[RECORD_SIZE];
    XID                      xid;
    int                      rmid;
    long                     flags;
} parsed_function_t;



/* parsed statement internal representation */
typedef struct {
    int                      thread_of_control;
    parsed_function_t        function;
    int                      expected_rc;
} parsed_statement_t;



/* context for XA statement processing */
typedef struct {
    int                      down_pipefd[MAX_THREADS_OF_CONTROL][2];
    int                      up_pipefd[MAX_THREADS_OF_CONTROL][2];
    pid_t                    child_process[MAX_THREADS_OF_CONTROL];
    GThread                 *child_thread[MAX_THREADS_OF_CONTROL];
} xa_context_t;



/* a couple of pipes for father/child communication */
typedef struct {
    int                      read;
    int                      write;
} pipes_t;



void xa_context_reset(xa_context_t *xa_context)
{
    int i;
    for (i=0; i<MAX_THREADS_OF_CONTROL; ++i) {
        xa_context->down_pipefd[i][0] =
            xa_context->down_pipefd[i][1] = LIXA_NULL_FD;
        xa_context->up_pipefd[i][0] =
            xa_context->up_pipefd[i][1] = LIXA_NULL_FD;
        xa_context->child_process[i] = -1;
        xa_context->child_thread[i] = NULL;
    }
}



int lixavsr_parse_flag(const char *token, long *flag)
{
    enum Exception { INVALID_OPTION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_flag\n"));
    TRY {
        int i;
        int found = FALSE;
        for (i=0; i<sizeof(PARSABLE_FLAGS)/sizeof(char *); ++i) {
            if (0 == strcmp(token, PARSABLE_FLAGS[i])) {
                *flag = FLAG_VALUES[i];
                LIXA_TRACE(("lixavsr_parse_flag: flag='%s', 0x%8.8x\n",
                            PARSABLE_FLAGS[i], *flag));
                found = TRUE;
                break;
            } /* if (0 == strcmp(token */
        } /* for (i=0; */
        if (!found)
            THROW(INVALID_OPTION);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_OPTION:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_flag/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



/* check if rmid is allowable comparing it with the profile */
int lixavsr_check_rmid(const int rmid)
{
    if (global_ccc.actconf.rsrmgrs->len <= rmid) {
        LIXA_TRACE(("lixavsr_check_rmid: rmid is out of range %d/%d\n",
                    rmid, global_ccc.actconf.rsrmgrs->len));
        return FALSE;
    }
    return TRUE;
}



int lixavsr_parse_flags(const char *token, long *flags)
{
    enum Exception { PARSE_FLAG1
                     , PARSE_FLAG2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_flags\n"));
    TRY {
        char statement[RECORD_SIZE];
        char flag[RECORD_SIZE];
        char *tk = NULL;
        char *saveptr;
        long tmp;

        *flags = 0;
        strncpy(statement, token, sizeof(statement));

        if (NULL == strchr(statement, '|')) {
            /* only one flag */
            strncpy(flag, statement, sizeof(flag));
        } else {
            /* extracting first flag */
            tk = strtok_r(statement, "|", &saveptr);
            strncpy(flag, tk, sizeof(flag));
        }
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_flag(flag, &tmp)))
            THROW(PARSE_FLAG1);
        *flags |= tmp;
        LIXA_TRACE(("lixavsr_parse_flags: flags=0x%8.8x\n", *flags));
        /* extracting other flags */
        while (NULL != tk) {
            if (NULL == (tk = strtok_r(NULL, "|", &saveptr)))
                break;
            strncpy(flag, tk, sizeof(flag));
            if (LIXA_RC_OK != (ret_cod = lixavsr_parse_flag(flag, &tmp)))
                THROW(PARSE_FLAG2);
            *flags |= tmp;
            LIXA_TRACE(("lixavsr_parse_flags: flags=0x%8.8x\n", *flags));
        } /* while (NULL != tk) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_FLAG1:
            case PARSE_FLAG2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_flags/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_info_rmid_flags(const char *token, parsed_function_t *pf)
{
    enum Exception { OUT_OF_RANGE
                     , PARSE_FLAGS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_info_rmid_flags\n"));
    TRY {
        char statement[RECORD_SIZE];
        char *tk;
        char *saveptr;
        strncpy(statement, token, sizeof(statement));

        /* extracting function */
        tk = strtok_r(statement, "(", &saveptr);
        LIXA_TRACE(("lixavsr_parse_info_rmid_flags: function='%s'\n", tk));
        /* extracting info */
        tk = strtok_r(NULL, ",", &saveptr);
        /* removing quotes */
        if (tk[strlen(tk)-1] == '"')
            tk[strlen(tk)-1] = '\0';
        if (tk[0] == '"')
            tk++;
        strncpy(pf->info, tk, RECORD_SIZE);
        LIXA_TRACE(("lixavsr_parse_info_rmid_flags: info='%s'\n", pf->info));
        /* extracting rmid */
        tk = strtok_r(NULL, ",", &saveptr);
        pf->rmid = (int)strtol(tk, NULL, 0);
        if (!lixavsr_check_rmid(pf->rmid))
            THROW(OUT_OF_RANGE);
        LIXA_TRACE(("lixavsr_parse_info_rmid_flags: rmid=%d\n", pf->rmid));
        /* extracting flags */
        tk = strtok_r(NULL, ")", &saveptr);
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_flags(tk, &pf->flags)))
            THROW(PARSE_FLAGS);
        LIXA_TRACE(("lixavsr_parse_info_rmid_flags: flags=0x%8.8x\n",
                    pf->flags));
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case PARSE_FLAGS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_info_rmid_flags/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_xid(lixa_ser_xid_t lsx, XID *xid)
{
    enum Exception { XID_DESERIALIZED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_xid\n"));
    TRY {
        if (!lixa_xid_deserialize(xid, lsx))
            THROW(XID_DESERIALIZED);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XID_DESERIALIZED:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_xid_rmid_flags(const char *token, parsed_function_t *pf)
{
    enum Exception { PARSE_XID
                     , OUT_OF_RANGE
                     , PARSE_FLAGS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_xid_rmid_flags\n"));
    TRY {
        char statement[RECORD_SIZE];
        char *tk;
        char *saveptr;
        char serialized_xid[RECORD_SIZE];
        strncpy(statement, token, sizeof(statement));

        /* extracting function */
        tk = strtok_r(statement, "(\"", &saveptr);
        LIXA_TRACE(("lixavsr_parse_xid_rmid_flags: function='%s'\n", tk));
        /* extracting xid */
        tk = strtok_r(NULL, ",", &saveptr);
        /* removing quotes */
        if (tk[strlen(tk)-1] == '"')
            tk[strlen(tk)-1] = '\0';
        if (tk[0] == '"')
            tk++;
        strncpy(serialized_xid, tk, RECORD_SIZE);
        LIXA_TRACE(("lixavsr_parse_xid_rmid_flags: xid='%s'\n",
                    serialized_xid));
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_xid(tk, &pf->xid)))
            THROW(PARSE_XID);
        /* extracting rmid */
        tk = strtok_r(NULL, ",", &saveptr);
        pf->rmid = (int)strtol(tk, NULL, 0);
        if (!lixavsr_check_rmid(pf->rmid))
            THROW(OUT_OF_RANGE);
        LIXA_TRACE(("lixavsr_parse_xid_rmid_flags: rmid=%d\n", pf->rmid));
        /* extracting flags */
        tk = strtok_r(NULL, ")", &saveptr);
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_flags(tk, &pf->flags)))
            THROW(PARSE_FLAGS);
        LIXA_TRACE(("lixavsr_parse_xid_rmid_flags: flags=0x%8.8x\n",
                    pf->flags));
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case PARSE_XID:
            case PARSE_FLAGS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_xid_rmid_flags/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int lixavsr_parse_rm_exec1_args(const char *token, parsed_function_t *pf)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_rm_exec1_args\n"));
    TRY {
        char statement[RECORD_SIZE];
        char *tk;
        char *saveptr;
        strncpy(statement, token, sizeof(statement));

        /* extracting function */
        tk = strtok_r(statement, "\"", &saveptr);
        LIXA_TRACE(("lixavsr_parse_rm_exec1_args: function='%s'\n", tk));
        /* extracting info */
        tk = strtok_r(NULL, "\"", &saveptr);
        strncpy(pf->info, tk, RECORD_SIZE);
        LIXA_TRACE(("lixavsr_parse_rm_exec1_args: info='%s'\n", pf->info));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_rm_exec1_args/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_function(const char *token, parsed_function_t *pf)
{
    enum Exception { INVALID_OPTION
                     , INTERNAL_ERROR
                     , PARSE_TWO_ARGS
                     , PARSE_THREE_ARGS
                     , PARSE_RM_EXEC1_ARGS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_function\n"));
    TRY {
        int i;
        int function = -1;
        for (i=0; i<sizeof(PARSABLE_FUNCTIONS)/sizeof(char *); ++i) {
            if (token == strstr(token, PARSABLE_FUNCTIONS[i])) {
                function = i;
                LIXA_TRACE(("lixavsr_parse_function: parsed function '%s' "
                            "(%d)\n", PARSABLE_FUNCTIONS[function], function));
                break;
            }
        } /* for (i=0;  */
        if (0 > function)
            THROW(INVALID_OPTION);
        pf->fid = function;
        switch(pf->fid) {
            case XA_CLOSE:
            case XA_OPEN:
                if (LIXA_RC_OK != (ret_cod = lixavsr_parse_info_rmid_flags(
                                       token, pf)))
                    THROW(PARSE_TWO_ARGS);
                break;
            case XA_COMMIT:
            case XA_END:
            case XA_FORGET:
            case XA_PREPARE:
            case XA_ROLLBACK:
            case XA_START:
                if (LIXA_RC_OK != (ret_cod = lixavsr_parse_xid_rmid_flags(
                                       token, pf)))
                    THROW(PARSE_THREE_ARGS);
                break;
            case RM_ORA_EXEC1:
            case RM_MYS_EXEC1:
            case RM_PQL_EXEC1:
                if (LIXA_RC_OK != (ret_cod = lixavsr_parse_rm_exec1_args(
                                       token, pf)))
                    THROW(PARSE_RM_EXEC1_ARGS);
                break;
            case VSR_QUIT:
                /* no args to parse */
                break;
            default:
                THROW(INTERNAL_ERROR);
        } /* switch(pf->function) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_OPTION:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case PARSE_TWO_ARGS:
            case PARSE_THREE_ARGS:
            case PARSE_RM_EXEC1_ARGS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_function/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_record(const char *record,
                         parsed_statement_t *parsed_statement)
{
    enum Exception { NULL_OBJECT
                     , NO_TOC
                     , NO_FUNC
                     , PARSE_FUNCTION
                     , NO_RETCOD
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    char buffer[RECORD_SIZE];
    
    LIXA_TRACE(("lixavsr_parse_record\n"));
    TRY {
        strncpy(buffer, record, sizeof(buffer));
        char *token;
        char *saveptr;
        const char token_separator[] = "/";
        
        if (NULL == record)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixavsr_parse_record: parsing record '%s'\n", record));

        /* extracting thread of control */
        token = strtok_r(buffer, token_separator, &saveptr);
        if (NULL == token) {
            LIXA_TRACE(("lixavsr_parse_record: thread of control not "
                        "found\n"));
            THROW(NO_TOC);
        }
        parsed_statement->thread_of_control = strtol(token, NULL, 0);
        LIXA_TRACE(("lixavsr_parse_record: thread of control is %ld\n",
                    parsed_statement->thread_of_control));
        /* extracting function */
        token = strtok_r(NULL, token_separator, &saveptr);
        if (NULL == token) {
            LIXA_TRACE(("lixavsr_parse_record: function not found\n"));
            THROW(NO_FUNC);
        }
        LIXA_TRACE(("lixavsr_parse_record: function is '%s'\n", token));
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_function(
                               token, &parsed_statement->function)))
            THROW(PARSE_FUNCTION);
        /* extracting return code */
        token = strtok_r(NULL, token_separator, &saveptr);
        if (NULL == token) {
            LIXA_TRACE(("lixavsr_parse_record: expected return code not "
                        "found\n"));
            THROW(NO_RETCOD);
        }
        parsed_statement->expected_rc = (int)strtol(token, NULL, 0);
        LIXA_TRACE(("lixavsr_parse_record: expected return code is %d\n",
                    parsed_statement->expected_rc));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NO_TOC:
            case NO_FUNC:
            case NO_RETCOD:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PARSE_FUNCTION:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_parse_record/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int rm_pql_exec1(const char *sql_statement)
{
    enum Exception { NULL_OBJECT
                     , PQL_QUERY
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

#ifdef HAVE_POSTGRESQL
    /* MySQL connection */
    PGconn     *conn = NULL;
    PGresult   *res = NULL;
    LIXA_TRACE(("rm_pql_exec1\n"));
    TRY {
        /* get connection */
        if (NULL == (conn = lixa_pq_get_conn()))
            THROW(NULL_OBJECT);
        LIXA_TRACE(("rm_pql_exec1: executing SQL statement \"%s\"\n",
                    sql_statement));
        /* execute the passed statement */
        res = PQexec(conn, sql_statement);
        if (PGRES_COMMAND_OK != PQresultStatus(res)) {
            LIXA_TRACE(("rm_pql_exec1: SQL error %u/%s\n",
                        PQerrorMessage(conn)));
            PQclear(res);
            THROW(PQL_QUERY);
        } else {
            LIXA_TRACE(("rm_pql_exec1: SQL statement executed!\n"));
            PQclear(res);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PQL_QUERY:
                ret_cod = LIXA_RC_RM_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("rm_pql_exec1/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
#else
    LIXA_TRACE(("rm_pql_exec1: not configured for PostgreSQL Database!\n"));
    return LIXA_RC_BYPASSED_OPERATION;
#endif
}



int rm_mys_exec1(const char *sql_statement)
{
    enum Exception { NULL_OBJECT
                     , MYSQL_QUERY
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

#ifdef HAVE_MYSQL
    /* MySQL connection */
    MYSQL *conn = NULL;
    LIXA_TRACE(("rm_mys_exec1\n"));
    TRY {
        /* get connection */
        if (NULL == (conn = lixa_my_get_conn()))
            THROW(NULL_OBJECT);
        LIXA_TRACE(("rm_mys_exec1: executing SQL statement \"%s\"\n",
                    sql_statement));
        /* execute the passed statement */
        if (mysql_query(conn, sql_statement)) {
            LIXA_TRACE(("rm_mys_exec1: SQL error %u/%s\n",
                        mysql_errno(conn), mysql_error(conn)));
            THROW(MYSQL_QUERY);
        } else {
            LIXA_TRACE(("rm_mys_exec1: SQL statement executed!\n"));
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case MYSQL_QUERY:
                ret_cod = LIXA_RC_RM_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("rm_mys_exec1/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
#else
    LIXA_TRACE(("rm_mys_exec1: not configured for MySQL Database!\n"));
    return LIXA_RC_BYPASSED_OPERATION;
#endif
}



int rm_ora_exec1(const char *sql_statement)
{
    enum Exception { XAO_ENV_ERROR
                     , XAO_SVC_CTX_ERROR
                     , OCI_HANDLE_ALLOC_ERROR1
                     , OCI_HANDLE_ALLOC_ERROR2
                     , OCI_STMT_PREPARE
                     , OCI_STMT_EXECUTE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
#ifdef HAVE_ORACLE
    LIXA_TRACE(("rm_ora_exec1\n"));
    TRY {
        int           ocirc;
        OCIEnv        *oci_env;
        OCISvcCtx     *oci_svc_ctx;
        OCIStmt       *stmt_hndl;
        OCIError      *err_hndl;
        
        /*
         * retrieve environment and context
         */
        if (NULL == (oci_env = xaoEnv(NULL))) {
            LIXA_TRACE(("rm_ora_exec1: xaoEnv returned a NULL pointer\n"));
            THROW(XAO_ENV_ERROR);
        }
        if (NULL == (oci_svc_ctx = xaoSvcCtx(NULL))) {
            LIXA_TRACE(("rm_ora_exec1: xaoSvcCtx returned a NULL pointer\n"));
            THROW(XAO_SVC_CTX_ERROR);
        }
        
        /* allocate statement and error handles */
        if (0 != OCIHandleAlloc((dvoid *)oci_env, (dvoid **)&stmt_hndl,
                                OCI_HTYPE_STMT, (size_t)0, (dvoid **)0)) {
            LIXA_TRACE(("rm_ora_exec1: unable to allocate statement "
                        "handle\n"));
            THROW(OCI_HANDLE_ALLOC_ERROR1);
        }
        if (0 != OCIHandleAlloc((dvoid *)oci_env, (dvoid **)&err_hndl,
                                 OCI_HTYPE_ERROR, (size_t)0, (dvoid **)0)) {
            LIXA_TRACE(("rm_ora_exec1: unable to allocate error handle\n"));
            THROW(OCI_HANDLE_ALLOC_ERROR2);
        }

        /* preparing SQL statement */
        if (OCI_SUCCESS != OCIStmtPrepare(
                stmt_hndl, err_hndl, (text *)sql_statement,
                (ub4) strlen(sql_statement),
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)) {
            LIXA_TRACE(("rm_ora_exec1: unable to prepare INSERT statement "
                        "('%s') for execution\n", sql_statement));
            THROW(OCI_STMT_PREPARE);
        }
        /* executing SQL statement */
        ocirc = OCIStmtExecute(oci_svc_ctx, stmt_hndl, err_hndl,
                               (ub4)1, (ub4)0,
                               (CONST OCISnapshot *)NULL,
                               (OCISnapshot *)NULL, OCI_DEFAULT);
        if (OCI_SUCCESS != ocirc && OCI_SUCCESS_WITH_INFO != ocirc) {
            LIXA_TRACE(("rm_ora_exec1: error while executing INSERT "
                        "statement; ocirc = %d\n", ocirc));
            THROW(OCI_STMT_EXECUTE);
        } else {
            LIXA_TRACE(("rm_ora_exec1: SQL statement executed!\n"));
        }
        /* free the allocated handles */
        OCIHandleFree((dvoid *)stmt_hndl, (ub4)OCI_HTYPE_STMT);
        OCIHandleFree((dvoid *)err_hndl, (ub4)OCI_HTYPE_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XAO_ENV_ERROR:
            case XAO_SVC_CTX_ERROR:
            case OCI_HANDLE_ALLOC_ERROR1:
            case OCI_HANDLE_ALLOC_ERROR2:
            case OCI_STMT_PREPARE:
            case OCI_STMT_EXECUTE:
                ret_cod = LIXA_RC_RM_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("rm_ora_exec1/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
#else
    LIXA_TRACE(("rm_ora_exec1: not configured for Oracle Database!\n"));
    return LIXA_RC_BYPASSED_OPERATION;
#endif
}



int lixavsr_execute_function(parsed_function_t *parsed_function,
                             int *rc)
{
    enum Exception { OUT_OF_RANGE1
                     , OUT_OF_RANGE2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_execute_function\n"));
    TRY {
        struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
            global_ccc.actconf.rsrmgrs, struct act_rsrmgr_config_s,
            parsed_function->rmid);
        char *info = NULL;
        lixa_ser_xid_t lsx;
        LIXA_TRACE(("lixavsr_execute_function: fid=%d, rmid=%d, "
                    "flags=0x%8.8x\n",
                    parsed_function->fid, parsed_function->rmid,
                    parsed_function->flags));
        *rc = XA_OK;
        switch (parsed_function->fid) {
            case XA_CLOSE:
                /* use generic info or passed by the robot file? */
                if (strlen(parsed_function->info) == 0)
                    info = (char *)act_rsrmgr->generic->xa_close_info;
                else
                    info = parsed_function->info;
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            info, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_close_entry(
                    info, parsed_function->rmid, parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            info, parsed_function->rmid,
                            parsed_function->flags, *rc));                
                break;
            case XA_COMMIT:
                lixa_xid_serialize(&parsed_function->xid, lsx);
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_commit_entry(
                    &parsed_function->xid, parsed_function->rmid,
                    parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags, *rc));
                break;
            case XA_END:
                lixa_xid_serialize(&parsed_function->xid, lsx);
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_end_entry(
                    &parsed_function->xid, parsed_function->rmid,
                    parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags, *rc));
                break;
            case XA_FORGET:
                lixa_xid_serialize(&parsed_function->xid, lsx);
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_forget_entry(
                    &parsed_function->xid, parsed_function->rmid,
                    parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags, *rc));
                break;
            case XA_OPEN:
                /* use generic info or passed by the robot file? */
                if (strlen(parsed_function->info) == 0)
                    info = (char *)act_rsrmgr->generic->xa_open_info;
                else
                    info = parsed_function->info;
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            info, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_open_entry(
                    info, parsed_function->rmid, parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            info, parsed_function->rmid,
                            parsed_function->flags, *rc));                
                break;
            case XA_PREPARE:
                lixa_xid_serialize(&parsed_function->xid, lsx);
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_prepare_entry(
                    &parsed_function->xid, parsed_function->rmid,
                    parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags, *rc));
                break;
            case XA_ROLLBACK:
                lixa_xid_serialize(&parsed_function->xid, lsx);
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_rollback_entry(
                    &parsed_function->xid, parsed_function->rmid,
                    parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags, *rc));
                break;
            case XA_START:
                lixa_xid_serialize(&parsed_function->xid, lsx);
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\",%d,0x%8.8x)\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags));
                *rc = act_rsrmgr->xa_switch->xa_start_entry(
                    &parsed_function->xid, parsed_function->rmid,
                    parsed_function->flags);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\",%d,0x%8.8x)=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            lsx, parsed_function->rmid,
                            parsed_function->flags, *rc));
                break;
            case RM_ORA_EXEC1:
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\")\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            parsed_function->info));
                *rc = rm_ora_exec1(parsed_function->info);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\")=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            parsed_function->info, *rc));
                break;
            case RM_MYS_EXEC1:
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\")\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            parsed_function->info));
                *rc = rm_mys_exec1(parsed_function->info);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\")=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            parsed_function->info, *rc));
                break;
            case RM_PQL_EXEC1:
                LIXA_TRACE(("lixavsr_execute_function: executing "
                            "%s(\"%s\")\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            parsed_function->info));
                *rc = rm_pql_exec1(parsed_function->info);
                LIXA_TRACE(("lixavsr_execute_function: executed "
                            "%s(\"%s\")=%d\n",
                            PARSABLE_FUNCTIONS[parsed_function->fid],
                            parsed_function->info, *rc));
                break;
            default:
                THROW(OUT_OF_RANGE1);
        } /* switch (parsed_function->fid) */
        /* stdout reporting */
        switch (parsed_function->fid) {
            case XA_CLOSE:
            case XA_OPEN:
                printf("%s(\"%s\",%d,0x%8.8x)=%d\n",
                       PARSABLE_FUNCTIONS[parsed_function->fid],
                       info, parsed_function->rmid,
                       (unsigned int)parsed_function->flags, *rc);
                break;
            case XA_COMMIT:
            case XA_END:
            case XA_FORGET:
            case XA_PREPARE:
            case XA_ROLLBACK:
            case XA_START:
                printf("%s(\"%s\",%d,0x%8.8x)=%d\n",
                       PARSABLE_FUNCTIONS[parsed_function->fid],
                       lsx, parsed_function->rmid,
                       (unsigned int)parsed_function->flags, *rc);
                break;
            case RM_ORA_EXEC1:
            case RM_MYS_EXEC1:
            case RM_PQL_EXEC1:
                printf("%s(\"%s\")=%d\n",
                       PARSABLE_FUNCTIONS[parsed_function->fid],
                       parsed_function->info, *rc);
                break;
            default:
                THROW(OUT_OF_RANGE1);                
        } /* switch (parsed_function->fid) */
        fflush(stdout);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE1:
            case OUT_OF_RANGE2:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_execute_function/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void lixavsr_threadofcontrol(pipes_t *pipes)
{
    enum Exception { READ_ERROR
                     , EXECUTE_XA_FUNCTION
                     , WRITE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    pipes_t my_pipes = *pipes; /* make a local copy */
    
    LIXA_TRACE(("lixavsr_threadofcontrol\n"));
    TRY {
        parsed_function_t parsed_function;
        ssize_t bytes;
        
        LIXA_TRACE(("lixavsr_threadofcontrol: my_pipes->read=%d, "
                    "my_pipes->write=%d\n", my_pipes.read, my_pipes.write));
        while (TRUE) {
            int xa_rc = XA_OK;
            /* read a command to execute */
            bytes = read(my_pipes.read, &parsed_function,
                         sizeof(parsed_function));
            if (bytes != sizeof(parsed_function))
                THROW(READ_ERROR);
            /* execute XA function */
            if (VSR_QUIT != parsed_function.fid &&
                LIXA_RC_OK != (ret_cod = lixavsr_execute_function(
                                   &parsed_function, &xa_rc))) {
                THROW(EXECUTE_XA_FUNCTION);
            }
            if (VSR_QUIT == parsed_function.fid) {
                /* reporting thread of control exit */
                printf("exiting...\n");
                fflush(stdout);
            }
            /* write the return code */
            bytes = write(my_pipes.write, &xa_rc, sizeof(xa_rc));
            if (bytes != sizeof(xa_rc))
                THROW(WRITE_ERROR);
            /* exiting? */
            if (VSR_QUIT == parsed_function.fid) {
                LIXA_TRACE(("lixavsr_threadofcontrol: VSR_QUIT asked, "
                            "leaving...\n"));
                break;
            }
        } /* while (TRUE) */
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case READ_ERROR:
                ret_cod = LIXA_RC_READ_ERROR;
                break;
            case EXECUTE_XA_FUNCTION:
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_threadofcontrol/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    /* closing pipes before exiting */
    close(my_pipes.read);
    close(my_pipes.write);
    if (!threads)
        exit (LIXA_RC_OK == ret_cod ? 0 : 1);
}



int lixavsr_activate_threadofcontrol(xa_context_t *xa_context,
                                     int thread_of_control,
                                     pipes_t *tmp_pipes)
{
    enum Exception { PIPE_ERROR1
                     , PIPE_ERROR2
                     , G_THREAD_CREATE_ERROR
                     , FORK_ERROR
                     , THREADOFCONTROL2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_activate_threadofcontrol\n"));
    TRY {
        int tmp[2];
        /* create down pipes */
        if (0 > pipe(tmp))
            THROW(PIPE_ERROR1);
        xa_context->down_pipefd[thread_of_control][0] = tmp[0];
        xa_context->down_pipefd[thread_of_control][1] = tmp[1];
        /* create up pipes */
        if (0 > pipe(tmp))
            THROW(PIPE_ERROR2);
        xa_context->up_pipefd[thread_of_control][0] = tmp[0];
        xa_context->up_pipefd[thread_of_control][1] = tmp[1];
        /* prepare pipes for father/child communication */
        tmp_pipes->read = xa_context->down_pipefd[thread_of_control][0];
        tmp_pipes->write = xa_context->up_pipefd[thread_of_control][1];
        /* create child thread_of_control */
        if (threads) {
            LIXA_TRACE(("lixavsr_activate_threadofcontrol: starting a "
                        "new thread\n"));
            xa_context->child_thread[thread_of_control] =
                g_thread_new("", (GThreadFunc)lixavsr_threadofcontrol,
                             (gpointer)tmp_pipes);
            if (NULL == xa_context->child_thread[thread_of_control])
                THROW(G_THREAD_CREATE_ERROR);
        } else {
            LIXA_TRACE(("lixavsr_activate_threadofcontrol: starting a "
                        "new process...\n"));
            xa_context->child_process[thread_of_control] = fork();
            switch (xa_context->child_process[thread_of_control]) {
                case -1:
                    THROW(FORK_ERROR);
                    break;
                case 0:
                    /* this is the child */
                    lixavsr_threadofcontrol(tmp_pipes);
                    break;
                default:
                    /* this is the father */
                    LIXA_TRACE(("lixavsr_activate_threadofcontrol: started "
                                "process " PID_T_FORMAT "\n",
                                xa_context->child_process[thread_of_control]));
                    break;
            } /* switch (xa_context->child_process) */
        } /* if (threads) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PIPE_ERROR1:
            case PIPE_ERROR2:
                ret_cod = LIXA_RC_PIPE_ERROR;
                break;
            case G_THREAD_CREATE_ERROR:
                ret_cod = LIXA_RC_G_THREAD_CREATE_ERROR;
                break;
            case FORK_ERROR:
                ret_cod = LIXA_RC_FORK_ERROR;
                break;
            case THREADOFCONTROL2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_activate_threadofcontrol/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_execute_record(xa_context_t *xa_context,
                           parsed_statement_t *parsed_statement)
{
    enum Exception { OUT_OF_RANGE
                     , ACTIVATE_THREADOFCONTROL
                     , WRITE_ERROR
                     , READ_ERROR
                     , XA_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_execute_record\n"));
    TRY {
        ssize_t bytes;
        int xa_rc;
        pipes_t tmp_pipes; /* allocated here to avoid concurrency issues
                              between 'lixavsr_activate_threadofcontrol'
                              and 'lixavsr_threadofcontrol'. If allocated in
                              the last function, stack is reused before
                              child thread get data from it */
        
        /* is thread of control valid? */
        if (MAX_THREADS_OF_CONTROL <= parsed_statement->thread_of_control) {
            LIXA_TRACE(("lixavsr_execute_record: thread of control is "
                        "too high (%d/%d)\n",
                        parsed_statement->thread_of_control,
                        MAX_THREADS_OF_CONTROL));
            THROW(OUT_OF_RANGE);
        }
        /* new thread of control? */
        if (LIXA_NULL_FD == xa_context->down_pipefd[
                parsed_statement->thread_of_control][0]) {
            LIXA_TRACE(("lixavsr_execute_record: %d is a new thread of "
                        "control that must be activated...\n",
                        parsed_statement->thread_of_control));
            if (LIXA_RC_OK != (ret_cod = lixavsr_activate_threadofcontrol(
                                   xa_context,
                                   parsed_statement->thread_of_control,
                                   &tmp_pipes)))
                THROW(ACTIVATE_THREADOFCONTROL);
        }
        /* write the thread of control */
        printf("toc=%d\t", parsed_statement->thread_of_control);
        fflush(stdout);
        /* write command to the child */
        bytes = write(xa_context->down_pipefd[
                          parsed_statement->thread_of_control][1],
                      &parsed_statement->function, sizeof(parsed_function_t));
        if (sizeof(parsed_function_t) != bytes)
            THROW(WRITE_ERROR);
        /* read return code from the child */
        bytes = read(xa_context->up_pipefd[
                         parsed_statement->thread_of_control][0],
                     &xa_rc, sizeof(xa_rc));
        if (sizeof(int) != bytes)
            THROW(READ_ERROR);
        LIXA_TRACE(("lixavsr_execute_record: xa_rc=%d\n", xa_rc));
        if (xa_rc != parsed_statement->expected_rc) {
            LIXA_TRACE(("lixavsr_execute_record: XA function expected "
                        "return code is %d, retrieved return code is %d\n",
                        parsed_statement->expected_rc, xa_rc));
            printf("XA function expected "
                   "return code is %d, retrieved return code is %d\n",
                   parsed_statement->expected_rc, xa_rc);
            fflush(stdout);
            THROW(XA_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case ACTIVATE_THREADOFCONTROL:
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
                break;
            case READ_ERROR:
                ret_cod = LIXA_RC_READ_ERROR;
                break;
            case XA_ERROR:
                ret_cod = LIXA_RC_XA_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_execute_record/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_terminate_children(xa_context_t *xa_context)
{
    enum Exception { KILL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_terminate_children\n"));
    TRY {
        int i;
        if (sleep_before_kill)
            sleep(1);
        if (!threads) {
            for (i=0; i<MAX_THREADS_OF_CONTROL; ++i) {
                if (xa_context->child_process[i] > 0) {
                    LIXA_TRACE(("lixavsr_terminate_children: killing child "
                                PID_T_FORMAT "\n",
                                xa_context->child_process[i]));
                    if (0 > kill(xa_context->child_process[i],
                                 SIGINT))
                        THROW(KILL_ERROR);
                } /* if (xa_context->child_process[i] > 0) */
            } /* for (i=0; */
        } /* if (!threads) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case KILL_ERROR:
                ret_cod = LIXA_RC_KILL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixavsr_terminate_children/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int lixavsr_parse_file(const char *filename)
{
    enum Exception { NULL_OBJECT
                     , FOPEN_ERROR
                     , PARSE_RECORD
                     , EXECUTE_RECORD
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    FILE *file = NULL;
    char buffer[RECORD_SIZE];
    xa_context_t xa_context;
    int rc_term;
        
    LIXA_TRACE(("lixavsr_parse_file\n"));
    TRY {
        parsed_statement_t parsed_statement;

        xa_context_reset(&xa_context);
        if (NULL == filename)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixavsr_parse_file: parsing file '%s'\n", filename));
        if (NULL == (file = fopen(filename, "r")))
            THROW(FOPEN_ERROR);
        while (!feof(file)) {
            if (NULL == fgets(buffer, sizeof(buffer), file))
                break;
            /* removing trailing newline */
            buffer[strcspn(buffer, "\r\n")] = '\0';
            /* skipping comment lines */
            if ('#' == buffer[0]) {
                LIXA_TRACE(("lixavsr_parse_file: skipping comment line '%s'\n",
                            buffer));
                continue;
            }
            memset(&parsed_statement, 0, sizeof(parsed_statement));
            if (LIXA_RC_OK != (ret_cod = lixavsr_parse_record(
                                   buffer, &parsed_statement)))
                THROW(PARSE_RECORD);
            if (LIXA_RC_OK != (ret_cod = lixavsr_execute_record(
                                   &xa_context, &parsed_statement)))
                THROW(EXECUTE_RECORD);
        } /* while (!feof(file)) */
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case FOPEN_ERROR:
                ret_cod = LIXA_RC_FOPEN_ERROR;
                break;
            case PARSE_RECORD:
            case EXECUTE_RECORD:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    if (NULL != file)
        fclose(file);
        /* terminate children */
    if (LIXA_RC_OK != (rc_term = lixavsr_terminate_children(
                           &xa_context))) {
        LIXA_TRACE(("lixavsr_parse_file/lixavsr_terminate_children: "
                    "rc_term=%d ('%s')\n", rc_term, lixa_strerror(rc_term)));
    }
    LIXA_TRACE(("lixavsr_parse_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int main(int argc, char *argv[])
{
    int ret_cod;
    GError *error = NULL;
    GOptionContext *option_context = NULL;
    
    option_context = g_option_context_new("- LIXA test utility");
    g_option_context_add_main_entries(option_context, entries, NULL);

    LIXA_TRACE_INIT;
    LIXA_TRACE(("%s: starting\n", argv[0]));
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        fprintf(stderr, "option parsing failed: %s\n", error->message);
        exit(1);
    }

    g_option_context_free(option_context);
    
    if (print_version) {
        lixa_print_version(stdout);
        exit(0);
    }

    /* configure a client standard environment */
    if (LIXA_RC_OK != (ret_cod = client_config(&global_ccc))) {
        LIXA_TRACE(("%s/client_config: ret_cod=%d ('%s')\n", argv[0],
                    ret_cod, lixa_strerror(ret_cod)));
        exit(1);
    }
    
    /* parse file and execute command */
    if (LIXA_RC_OK != (ret_cod = lixavsr_parse_file(filename))) {
        LIXA_TRACE(("%s/lixavsr_parse_file: ret_cod=%d ('%s')\n", argv[0],
                    ret_cod, lixa_strerror(ret_cod)));
    }
    
    LIXA_TRACE(("%s: ending\n", argv[0]));
    exit (LIXA_RC_OK == ret_cod ? 0 : 1);
}
