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
#include <lixa_syslog.h>
#include <client_config.h>
#include <tx.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_RECOVERY



/* default command line options */
static gboolean run_as_daemon = FALSE;
static gboolean dump_and_exit = FALSE;
/* command line options */
static GOptionEntry entries[] =
{
    { "daemon", 'd', 0, G_OPTION_ARG_NONE, &run_as_daemon, "Run the process as a daemon", NULL },
    { "dump", 'u', 0, G_OPTION_ARG_NONE, &dump_and_exit, "Dump the content of status files and exit", NULL }
};



void output_environment(void);



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
    syslog(LOG_NOTICE, LIXA_SYSLOG_LXR000I);

    option_context = g_option_context_new("- LIXA recovery utility");
    g_option_context_add_main_entries(option_context, entries, NULL);

    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        syslog(LOG_ERR, LIXA_SYSLOG_LXR001E, error->message);
        LIXA_TRACE(("main: option parsing failed: %s\n", error->message));
        g_print("option parsing failed: %s\n", error->message);
        exit(1);
    }

    /* initialize libxml2 library */
    LIBXML_TEST_VERSION;

    /* echo environment variables */
    output_environment();

    /* open the resource managers for the current profile */
    tx_rc = tx_open();
    LIXA_TRACE(("lixar/tx_open: tx_rc = %d\n", tx_rc));
    switch (tx_rc) {
        case TX_OK:
            break;
        case TX_ERROR:
            printf("tx_open() returned TX_ERROR: unable to proceed\n");
            break;
        case TX_FAIL:
            printf("tx_open() returned TX_FAIL: unable to procee\n");
            break;
        default:
            printf("tx_open() returned %d: unable to procee\n", tx_rc);
    }
    if (TX_OK != tx_rc)
        exit(1);
    
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
