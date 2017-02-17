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
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_utils.h"
#include "xa.h"



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



#define RECORD_SIZE 500
#define MAX_THREADS_OF_CONTROL 10



/* default command line options */
static char *filename = NULL;
static gboolean threads = FALSE;
static gboolean print_version = FALSE;
static GOptionEntry entries[] =
{
    { "filename", 'f', 0, G_OPTION_ARG_STRING, &filename, "Name of the file with the actions that must be executed", NULL },
    { "threads", 't', 0, G_OPTION_ARG_NONE, &threads, "Use threads instead of processes", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { NULL }
};



/* parsable XA functions: strings */
static const char *PARSABLE_FUNCTIONS[] = {
    "xa_close", "xa_commit", "xa_end", "xa_forget", "xa_open", "xa_prepare",
    "xa_rollback", "xa_start"
};
/* accepted XA functions: internal */
typedef enum {
    XA_CLOSE
    , XA_COMMIT
    , XA_END
    , XA_FORGET
    , XA_OPEN
    , XA_PREPARE
    , XA_ROLLBACK
    , XA_START
} function_id_t;



/* parsable XIDs: strings */
static const char *PARSABLE_XIDS[] = {
    "xid11", "xid12", "xid21", "xid22"
};
/* accepted XIDs: internal */
typedef enum {
    XID11
    , XID12
    , XID21
    , XID22
} accepted_xids_t;



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



/* pre-defined XID values */
static const XID XID_VALUES[] = {
    { 0x6e416967, 4, 4, "00001111" } ,
    { 0x6e416967, 4, 4, "00002222" } ,
    { 0x6e416967, 4, 4, "33334444" } ,
    { 0x6e416967, 4, 4, "33335555" }
};



/* parsed function internal representation */
typedef struct {
    function_id_t            fid;
    char                     info[RECORD_SIZE];
    accepted_xids_t          xid;
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
    int                      pipefd[MAX_THREADS_OF_CONTROL][2];
} xa_context_t;



void xa_context_reset(xa_context_t *xa_context)
{
    int i;
    for (i=0; i<MAX_THREADS_OF_CONTROL; ++i)
        xa_context->pipefd[i][0] = xa_context->pipefd[i][1] = LIXA_NULL_FD;
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
    enum Exception { PARSE_FLAGS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_info_rmid_flags\n"));
    TRY {
        char statement[RECORD_SIZE];
        char *tk;
        char *saveptr;
        strncpy(statement, token, sizeof(statement));

        /* extracting function */
        tk = strtok_r(statement, "(\"", &saveptr);
        LIXA_TRACE(("lixavsr_parse_info_rmid_flags: function='%s'\n", tk));
        /* extracting info */
        tk = strtok_r(NULL, "\",", &saveptr);
        strncpy(pf->info, tk, RECORD_SIZE);
        LIXA_TRACE(("lixavsr_parse_info_rmid_flags: info='%s'\n", pf->info));
        /* extracting rmid */
        tk = strtok_r(NULL, ",", &saveptr);
        pf->rmid = (int)strtol(tk, NULL, 0);
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



int lixavsr_parse_xid(const char *token, accepted_xids_t *xid)
{
    enum Exception { INVALID_OPTION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_xid\n"));
    TRY {
        int i;
        int tmp_xid = -1;
        for (i=0; i<sizeof(PARSABLE_XIDS)/sizeof(char *); ++i) {
            if (0 == strcmp(token, PARSABLE_XIDS[i])) {
                tmp_xid = i;
                LIXA_TRACE(("lixavsr_parse_xid: parsed xid '%s' (%d)\n",
                            PARSABLE_XIDS[tmp_xid], tmp_xid));
                break;
            }
        } /* for (i=0;  */
        if (0 > tmp_xid)
            THROW(INVALID_OPTION);
        *xid = tmp_xid;
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
    LIXA_TRACE(("lixavsr_parse_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_xid_rmid_flags(const char *token, parsed_function_t *pf)
{
    enum Exception { PARSE_XID
                     , PARSE_FLAGS
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_xid_rmid_flags\n"));
    TRY {
        char statement[RECORD_SIZE];
        char *tk;
        char *saveptr;
        strncpy(statement, token, sizeof(statement));

        /* extracting function */
        tk = strtok_r(statement, "(\"", &saveptr);
        LIXA_TRACE(("lixavsr_parse_xid_rmid_flags: function='%s'\n", tk));
        /* extracting xid */
        tk = strtok_r(NULL, "\",", &saveptr);
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_xid(tk, &pf->xid)))
            THROW(PARSE_XID);
        LIXA_TRACE(("lixavsr_parse_xid_rmid_flags: xid=%d\n", pf->xid));
        /* extracting rmid */
        tk = strtok_r(NULL, ",", &saveptr);
        pf->rmid = (int)strtol(tk, NULL, 0);
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



int lixavsr_parse_function(const char *token, parsed_function_t *pf)
{
    enum Exception { INVALID_OPTION
                     , INTERNAL_ERROR
                     , PARSE_TWO_ARGS
                     , PARSE_THREE_ARGS
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



int lixavsr_execute_record(xa_context_t *xa_context,
                           parsed_statement_t *parsed_statement)
{
    enum Exception { OUT_OF_RANGE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_execute_record\n"));
    TRY {
        /* is thread of control valid? */
        if (MAX_THREADS_OF_CONTROL <= parsed_statement->thread_of_control) {
            LIXA_TRACE(("lixavsr_execute_record: thread of control is "
                        "too high (%d/%d)\n",
                        parsed_statement->thread_of_control,
                        MAX_THREADS_OF_CONTROL));
            THROW(OUT_OF_RANGE);
        }
        /* new thread of control? */
        if (LIXA_NULL_FD == xa_context->pipefd[
                parsed_statement->thread_of_control][0]) {
            LIXA_TRACE(("lixavsr_execute_record: %d is a new thread of "
                        "control that must be activated...\n",
                        parsed_statement->thread_of_control));
            /* @@@ */
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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

    LIXA_TRACE(("lixavsr_parse_file\n"));
    TRY {
        parsed_statement_t parsed_statement;
        xa_context_t xa_context;

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

    if (LIXA_RC_OK != (ret_cod = lixavsr_parse_file(filename))) {
        LIXA_TRACE(("%s/lixavsr_parse_file: ret_cod=%d ('%s')\n", argv[0],
                    ret_cod, lixa_strerror(ret_cod)));
    }
    
    LIXA_TRACE(("%s: ending\n", argv[0]));
    return LIXA_RC_OK == ret_cod ? 0 : 1;
}
