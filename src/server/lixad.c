/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
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



#include "lixa_crash.h"
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_utils.h"
#include "lixa_syslog.h"
#include "server_config.h"
#include "server_listener.h"
#include "server_manager.h"
#include "server_recovery.h"
#include "server_status.h"



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
gboolean run_as_daemon = FALSE;
static gboolean maintenance = FALSE;
static char *dump_specs = NULL;
static char *config_file = NULL;
static char *trace_file = NULL;
static gboolean clean_failed = FALSE;
static gboolean print_version = FALSE;
/* command line options */
static GOptionEntry entries[] =
{
    {"daemon",       'd', 0, G_OPTION_ARG_NONE,   &run_as_daemon, "Run the process as a daemon",                                                       NULL},
    {"maintenance",  'm', 0, G_OPTION_ARG_NONE,   &maintenance,   "Start the server in maintenance mode only",                                         NULL},
    {"dump",         'u', 0, G_OPTION_ARG_STRING, &dump_specs,    "Dump the content of status files using order [ufs] (u=used, f=free, s=sequential)", NULL},
    {"config-file",  'c', 0, G_OPTION_ARG_STRING, &config_file,   "Specify an alternate configuration file",                                           NULL},
    {"trace-file",   't', 0, G_OPTION_ARG_STRING, &trace_file,    "Specify trace file name",                                                           NULL},
    {"clean-failed", 'l', 0, G_OPTION_ARG_NONE,   &clean_failed,  "Clean recovery failed transactions at start-up",                                    NULL},
    {"version",      'v', 0, G_OPTION_ARG_NONE,   &print_version, "Print package info and exit",                                                       NULL},
    {NULL}
};



int main(int argc, char *argv[])
{
    int rc = LIXA_RC_OK;
    struct server_config_s sc;
    struct listener_status_array_s lsa;
    struct thread_status_array_s tsa;
    srvr_rcvr_tbl_t srt;
    server_trans_tbl_t stt;
    GError *error = NULL;
    GOptionContext *option_context;
    struct ts_dump_spec_s tsds;
    struct ts_recovery_spec_s tsrs;
    int pid_fd;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;

    openlog("lixad", LOG_PID, LOG_DAEMON);
    LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD000N,
                 LIXA_PACKAGE_NAME, LIXA_PACKAGE_VERSION));

    option_context = g_option_context_new("- LIXA server");
    g_option_context_add_main_entries(option_context, entries, NULL);
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD001E, error->message));
        g_print("option parsing failed: %s\n", error->message);
        exit(1);
    }
    g_option_context_free(option_context);

    if (NULL != trace_file) {
        FILE *dummy;
        dummy = freopen(trace_file, "w", stderr);
#ifdef LIXA_DEBUG
        /* remove buffering: it's the default behavior before freopen, but it
           changes calling it */
        setbuf(dummy, NULL);
#endif /* LIXA_DEBUG */
    }

    if (print_version) {
        lixa_print_version(stdout);
        exit(0);
    }
    if (run_as_daemon && NULL != dump_specs) {
        LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD002W));
        LIXA_TRACE(("main: dump option overrides daemon option\n"));
        g_print("Warning: dump option overrides daemon option\n");
        run_as_daemon = FALSE;
    }

    if (maintenance)
        LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD020N));

    /* daemonize the server process */
    if (run_as_daemon)
        daemonize();

    /* initialize libxml2 library */
    LIBXML_TEST_VERSION;

    /* initialize configuration structure */
    server_config_init(&sc, &tpa);
    if (LIXA_RC_OK != (rc = server_config(&sc, &tpa, config_file))) {
        LIXA_TRACE(("main/server_config: rc=%d\n", rc));
        LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD003E, lixa_strerror(rc)));
        return rc;
    }

    /* write pid file */
    if (!dump_specs) {
        if (-1 == (pid_fd = open(sc.pid_file, O_CREAT | O_WRONLY,
                                 S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)))
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD015W, sc.pid_file,
                         errno, strerror(errno)));
        else {
            char array[100];
            ssize_t written_bytes;
            snprintf(array, sizeof(array), PID_T_FORMAT
                     "\n", getpid());
            written_bytes = write(pid_fd, array, strlen(array));
            close(pid_fd);
        }
    }

    if (LIXA_RC_OK != (rc = server_pipes_init(&tpa))) {
        LIXA_TRACE(("main/server_pipes_init: rc=%d\n", rc));
        LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD017E));
        return rc;
    }

    /* start configured manager(s) */
    tsds.dump = NULL != dump_specs;
    if (tsds.dump) {
        tsds.free = NULL != strchr(dump_specs, 'f');
        tsds.used = NULL != strchr(dump_specs, 'u');
        tsds.seq = NULL != strchr(dump_specs, 's');
    }
    tsrs.clean_failed = clean_failed;
    if (tsrs.clean_failed)
        LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD022N));
    LIXA_TRACE(("lixad/main: starting\n"));
    if (LIXA_RC_OK != (rc = server_manager(
                           &sc, &tpa, &tsa, &srt, &stt, &tsds, &tsrs,
                           maintenance))) {
        LIXA_TRACE(("main/server_manager: rc=%d\n", rc));
        LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD004E, lixa_strerror(rc)));
        return rc;
    }

    /* start configured listener(s) */
    if (NULL == dump_specs &&
        LIXA_RC_OK != (rc = server_listener(&sc, &lsa, &tsa))) {
        LIXA_TRACE(("main/server_listener: rc=%d\n", rc));
        LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD005E, lixa_strerror(rc)));
        return rc;
    }

    /* removing pid file */
    if (!dump_specs) {
        if (0 != (unlink(sc.pid_file))) {
            LIXA_TRACE(("main/unlink: errno=%d ('%s')\n",
                        errno, strerror(errno)));
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD024N, sc.pid_file, errno,
                         strerror(errno)));
        }
    }

    /* clean-up memory to enhance memory leak detection */
    server_cleanup(&sc, &tpa, &tsa, &srt, &stt);

    /* it's time to exit */
    LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD006N));

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
    FILE *dummy;

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
    i = chdir("/");

    LIXA_TRACE(("lixad/daemonize: umask()\n"));
    umask(0);

    for (i = 0; i < 64; ++i)
        close(i);

    if (NULL != trace_file)
        dummy = freopen(trace_file, "a", stderr);
    else
        dummy = freopen("/dev/null", "a", stderr);

    LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD014N));

    LIXA_TRACE(("lixad/daemonize: now daemonized!\n"));

    return;
}
