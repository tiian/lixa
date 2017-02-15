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


/* default command line options */
static char *filename = NULL;
static gboolean print_version = FALSE;
static GOptionEntry entries[] =
{
    { "filename", 'f', 0, G_OPTION_ARG_STRING, &filename, "Name of the file with the actions that must be executed", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { NULL }
};



/* parsable XA functions: strings */
static const char *PARSABLE_FUNCTIONS[] = {
    "xa_close", "xa_commit", "xa_end", "xa_forget", "xa_open", "xa_prepare",
    "xa_recover", "xa_rollback", "xa_start"
};
/* accepted XA functions: internal */
typedef enum {
    XA_CLOSE
    , XA_COMMIT
    , XA_END
    , XA_FORGET
    , XA_OPEN
    , XA_PREPARE
    , XA_RECOVER
    , XA_ROLLBACK
    , XA_START
} accepted_functions_t;



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



/* parsed function internal representation */
typedef struct {
    accepted_functions_t     function;
    accepted_xids_t          xid;
    int                      rmid;
    long                     flags;
} parsed_function_t;



int lixavsr_parse_two_args(const char *token, parsed_function_t *pf)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_two_args\n"));
    TRY {
        /* @@@ */        
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
    LIXA_TRACE(("lixavsr_parse_two_args/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixavsr_parse_three_args(const char *token, parsed_function_t *pf)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixavsr_parse_three_args\n"));
    TRY {
        /* @@@ */
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
    LIXA_TRACE(("lixavsr_parse_three_args/excp=%d/"
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
        pf->function = function;
        switch(pf->function) {
            case XA_CLOSE:
            case XA_OPEN:
                if (LIXA_RC_OK != (ret_cod = lixavsr_parse_two_args(
                                       token, pf)))
                    THROW(PARSE_TWO_ARGS);
                break;
            case XA_COMMIT:
            case XA_END:
            case XA_FORGET:
            case XA_PREPARE:
            case XA_RECOVER:
            case XA_ROLLBACK:
            case XA_START:
                if (LIXA_RC_OK != (ret_cod = lixavsr_parse_three_args(
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



int lixavsr_parse_record(const char *record)
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
        const char token_separator[] = "/";
        long thread_of_control;
        parsed_function_t parsed_function;
        long return_code;
        
        if (NULL == record)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixavsr_parse_record: parsing record '%s'\n", record));

        /* extracting thread of control */
        token = strtok(buffer, token_separator);
        if (NULL == token) {
            LIXA_TRACE(("lixavsr_parse_record: thread of control not "
                        "found\n"));
            THROW(NO_TOC);
        }
        thread_of_control = strtol(token, NULL, 0);
        LIXA_TRACE(("lixavsr_parse_record: thread of control is %ld\n",
                    thread_of_control));
        /* extracting function */
        token = strtok(NULL, token_separator);
        if (NULL == token) {
            LIXA_TRACE(("lixavsr_parse_record: function not found\n"));
            THROW(NO_FUNC);
        }
        memset(&parsed_function, 0, sizeof(parsed_function));
        LIXA_TRACE(("lixavsr_parse_record: function is '%s'\n", token));
        if (LIXA_RC_OK != (ret_cod = lixavsr_parse_function(
                               token, &parsed_function)))
            THROW(PARSE_FUNCTION);
        /* @@@ lixavsr_parse_function */
        /* extracting return code */
        token = strtok(NULL, token_separator);
        if (NULL == token) {
            LIXA_TRACE(("lixavsr_parse_record: return code not found\n"));
            THROW(NO_RETCOD);
        }
        return_code = strtol(token, NULL, 0);
        LIXA_TRACE(("lixavsr_parse_record: return code is %ld\n",
                    return_code));
        
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



int lixavsr_parse_file(const char *filename)
{
    enum Exception { NULL_OBJECT
                     , FOPEN_ERROR
                     , PARSE_RECORD
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    FILE *file = NULL;
    char buffer[RECORD_SIZE];

    LIXA_TRACE(("lixavsr_parse_file\n"));
    TRY {
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
            if (LIXA_RC_OK != (ret_cod = lixavsr_parse_record(buffer)))
                THROW(PARSE_RECORD);
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
    return 0;
}
