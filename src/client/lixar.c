/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif
#ifdef HAVE_LIBXML_XMLVERSION_H
# include <libxml/xmlversion.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif



#include <lixa_crash.h>
#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_tx.h>
#include <lixa_utils.h>
#include <lixa_syslog.h>
#include <client_config.h>
#include <tx.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_RECOVERY



/* default command line options */
static gboolean report = FALSE;
static char *xid = NULL;
static char *xid_file = NULL;
static gboolean commit = FALSE;
static gboolean rollback = FALSE;
static gboolean print_version = FALSE;
static gboolean bypass_bqual_check = FALSE;
static gboolean bypass_formatid_check = FALSE;
static gboolean use_tmendrscan_flag = FALSE;
/* command line options: DO NOT CHANGE ORDER, only append!!! */
static GOptionEntry entries[] =
{
    { "print", 'p', 0, G_OPTION_ARG_NONE, &report, "Print a report of all the prepared and in-doubt transactions compatible with current configuration and profile", NULL },
    { "xid", 'x', 0, G_OPTION_ARG_STRING, &xid, "Select specified transaction for rollback/commit", NULL },
    { "xid-file", 'X', 0, G_OPTION_ARG_STRING, &xid_file, "Select specified file as a list of transaction to rollback/commit", NULL },
    { "commit", 'c', 0, G_OPTION_ARG_NONE, &commit, "Commit prepared & in-doubt transactions", NULL },
    { "rollback", 'r', 0, G_OPTION_ARG_NONE, &rollback, "Rollback prepared & in-doubt transactions", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { "bypass-bqual-check", 'b', 0, G_OPTION_ARG_NONE, &bypass_bqual_check, "Bypass xid branch qualifier check", NULL },
    { "bypass-formatid-check", 'B', 0, G_OPTION_ARG_NONE, &bypass_formatid_check, "Bypass xid format id check", NULL },
    { "use-tmendrscan-flag", 'e', 0, G_OPTION_ARG_NONE, &use_tmendrscan_flag, "Use TMENDRSCAN flag for last xa_recover call", NULL },
    { NULL }
};



void output_environment(void);


void output_options(void);


int main(int argc, char *argv[])
{
    int rc = LIXA_RC_OK;
    int tx_rc;
    
    GError *error = NULL;
    GOptionContext *option_context;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("main: starting\n"));
    openlog("lixar", LOG_PID, LOG_DAEMON);
    syslog(LOG_NOTICE, LIXA_SYSLOG_LXR000I,
           LIXA_PACKAGE_NAME, LIXA_PACKAGE_VERSION);
    
    option_context = g_option_context_new("- LIXA recovery utility");
    g_option_context_add_main_entries(option_context, entries, NULL);
    
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        syslog(LOG_ERR, LIXA_SYSLOG_LXR001E, error->message);
        LIXA_TRACE(("main: option parsing failed: %s\n", error->message));
        fprintf(stderr, "option parsing failed: %s\n", error->message);
        exit(1);
    }

    if (print_version) {
        lixa_print_version(stdout);
        exit(0);
    }
    
    if (xid && xid_file) {
        fprintf(stderr, "'-%c' ('--%s') and '-%c' ('--%s') options are "
                "mutually exclusive\n",
                entries[1].short_name, entries[1].long_name,
                entries[2].short_name, entries[2].long_name);
        exit(1);
    }

    if (commit && rollback) {
        fprintf(stderr, "'-%c' ('--%s') and '-%c' ('--%s') options are "
                "mutually exclusive\n",
                entries[3].short_name, entries[3].long_name,
                entries[4].short_name, entries[4].long_name);
        exit(1);
    }

    if ((commit || rollback) && (!xid && !xid_file)) {
        fprintf(stderr, "No transaction to be committed/rolled back was "
                "specified; see '-%c' ('--%s') and '-%c' ('--%s') options \n",
                entries[1].short_name, entries[1].long_name,
                entries[2].short_name, entries[2].long_name);
        exit(1);
    }
    
    /* initialize libxml2 library */
    LIBXML_TEST_VERSION;

    /* echo execution options */
    output_options();
    
    /* echo environment variables */
    if (report)
        output_environment();

    /* open the resource managers for the current profile */
    lixa_tx_open(&tx_rc, TRUE);
    LIXA_TRACE(("lixar/tx_open: tx_rc = %d\n", tx_rc));
    switch (tx_rc) {
        case TX_OK:
            break;
        case TX_ERROR:
            printf("tx_open() returned TX_ERROR: unable to proceed\n");
            break;
        case TX_FAIL:
            printf("tx_open() returned TX_FAIL: unable to proceed\n");
            break;
        default:
            printf("tx_open() returned %d: unable to proceed\n", tx_rc);
    }
    if (TX_OK != tx_rc) {
        syslog(LOG_ERR, LIXA_SYSLOG_LXR003E, tx_rc);
        exit(1);
    }

    if (LIXA_RC_OK != (rc = lixa_tx_recover(
                           report, commit, rollback, bypass_bqual_check,
                           bypass_formatid_check, use_tmendrscan_flag,
                           xid, xid_file))) {
        printf("There was an error while recoverying transactions: "
               "%d ('%s'); look at system log to collect additional "
               "information or activate the trace to debug the problem\n",
               rc, lixa_strerror(rc));
        exit(1);
    }

    if (xid)
        g_free(xid);
    if (xid_file)
        g_free(xid_file);
    
    /* it's time to exit */
    syslog(LOG_NOTICE, LIXA_SYSLOG_LXR002I);

    LIXA_TRACE(("lixar/main: exiting\n"));    
    return 0;
}



void output_environment(void)
{
    char *tmp_str;
    const char *null_str = "(null)";

    printf("Recovery environment:\n");
    
    tmp_str = getenv(LIXA_CONFIG_FILE_ENV_VAR);
    printf("LIXA_CONFIG_FILE_ENV_VAR = '%s'\n",
           tmp_str ? tmp_str : null_str);

    tmp_str = getenv(LIXA_PROFILE_ENV_VAR);
    printf("LIXA_PROFILE_ENV_VAR = '%s'\n",
           tmp_str ? tmp_str : null_str);

    tmp_str = getenv(LIXA_JOB_ENV_VAR);
    printf("LIXA_JOB_ENV_VAR = '%s'\n",
           tmp_str ? tmp_str : null_str);

    return;
}



void output_options(void)
{
    static const char *true_string = "yes";
    static const char *false_string = "no";
    printf("Execution options:\n");
    printf("\t- print report = %s\n", report ? true_string : false_string);
    if (xid)
        printf("\t- transaction to commit/rollback = %s\n", xid);
    if (xid_file)
        printf("\t- (file) list of transactions to commit/rollback = %s\n",
               xid_file);
    printf("\t- transaction(s) will be committed = %s\n",
           commit ? true_string : false_string);
    printf("\t- transaction(s) will be rolled back = %s\n",
           rollback ? true_string : false_string);
    printf("\t- bypass xid branch qualifier check = %s\n",
           bypass_bqual_check ? true_string : false_string);
    printf("\t- bypass xid format id check = %s\n",
           bypass_formatid_check ? true_string : false_string);
    printf("\t- use TMENDRSCAN flag for last xa_recover call = %s\n\n",
           use_tmendrscan_flag ? true_string : false_string);
    return;
}
