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
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_LIBXML_XMLVERSION_H
# include <libxml/xmlversion.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_syslog.h>
#include <server_config.h>
#include <server_listener.h>
#include <server_manager.h>
#include <server_recovery.h>
#include <server_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER



/**
 * Transform the current process in a daemon, detaching terminals and
 * changing parent group
 * This function is copied from an example showed by Richard Stevens in
 * UNIX network programming book
 */
void daemonize(void);



/* default command line options */
static gboolean run_as_daemon = FALSE;
static gboolean dump_and_exit = FALSE;
/* command line options */
static GOptionEntry entries[] =
{
    { "daemon", 'd', 0, G_OPTION_ARG_NONE, &run_as_daemon, "Run the process as a daemon", NULL },
    { "dump", 'u', 0, G_OPTION_ARG_NONE, &dump_and_exit, "Dump the content of status files and exit", NULL }
};



int main(int argc, char *argv[])
{
    int rc = LIXA_RC_OK;
    struct server_config_s sc;
    struct listener_status_array_s lsa;
    struct thread_pipe_array_s tpa;
    struct thread_status_array_s tsa;
    srvr_rcvr_tbl_t srt = SRVR_RCVR_TBL_INIT;
    GError *error = NULL;
    GOptionContext *option_context;

    LIXA_TRACE_INIT;
    LIXA_TRACE(("main: starting\n"));
    openlog("lixad", LOG_PID, LOG_DAEMON);
    syslog(LOG_NOTICE, LIXA_SYSLOG_LXD000N);

    option_context = g_option_context_new("- Lixa server");
    g_option_context_add_main_entries(option_context, entries, NULL);
    /*
    g_option_context_add_group (context, gtk_get_option_group (TRUE));
    */
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        syslog(LOG_ERR, LIXA_SYSLOG_LXD001E, error->message);
        LIXA_TRACE(("main: option parsing failed: %s\n", error->message));
        g_print("option parsing failed: %s\n", error->message);
        exit(1);
    }
    if (run_as_daemon && dump_and_exit) {
        syslog(LOG_WARNING, LIXA_SYSLOG_LXD002W);
        LIXA_TRACE(("main: dump option overrides daemon option\n"));
        g_print("Warning: dump option overrides daemon option\n");
        run_as_daemon = FALSE;
    }
    if (run_as_daemon)
        daemonize();

    /* initialize libxml2 library */
    LIBXML_TEST_VERSION;
        
    /* initialize configuration structure */
    server_config_init(&sc, &tpa);
    if (LIXA_RC_OK != (rc = server_config(&sc, &tpa, ""))) {
        LIXA_TRACE(("main/server_config: rc = %d\n", rc));
        syslog(LOG_ERR, LIXA_SYSLOG_LXD003E, lixa_strerror(rc));
        return rc;
    }

    /* start configured manager(s) */
    if (LIXA_RC_OK != (rc = server_manager(&sc, &tpa, &tsa, &srt,
                                           dump_and_exit))) {
        LIXA_TRACE(("main/server_manager: rc = %d\n", rc));
        syslog(LOG_ERR, LIXA_SYSLOG_LXD004E, lixa_strerror(rc));
        return rc;
    }

    /* start configured listener(s) */
    if (!dump_and_exit &&
        LIXA_RC_OK != (rc = server_listener(&sc, &lsa, &tsa))) {
        LIXA_TRACE(("main/server_listener: rc = %d\n", rc));
        syslog(LOG_ERR, LIXA_SYSLOG_LXD005E, lixa_strerror(rc));
        return rc;
    }

    /* it's time to exit */
    syslog(LOG_NOTICE, LIXA_SYSLOG_LXD006N);

    LIXA_TRACE(("lixad/main: exiting\n"));    
    return 0;
}



/**
 * Courtesy of Mr. Richard Stevens, UNIX Network Programming
 */
void daemonize(void)
{
        pid_t pid;
        int i;

        LIXA_TRACE(("lixad/daemonize: fork()\n"));
        if (0 != (pid = fork()))
            exit(0);

        LIXA_TRACE(("lixad/daemonize: setsid()\n"));
        setsid();

        LIXA_TRACE(("lixad/daemonize: signal()\n"));
        signal(SIGHUP, SIG_IGN);
        
        LIXA_TRACE(("lixad/daemonize: fork()\n"));
        if (0 != (pid = fork()))
            exit(0);

        LIXA_TRACE(("lixad/daemonize: chdir()\n"));
        chdir("/");

        LIXA_TRACE(("lixad/daemonize: umask()\n"));
        umask(0);

        for (i = 0; i < 64; ++i)
            close(i);

        syslog(LOG_NOTICE, "entered daemon status");

        freopen("/tmp/lixad.stderr", "w", stderr);

        LIXA_TRACE(("lixad/daemonize: now daemonized!\n"));

        return;
}
