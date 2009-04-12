/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <config.h>

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
#include <server_config.h>
#include <server_listener.h>
#include <server_manager.h>
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



int main(int argc, char *argv[])
{
    int rc = LIXA_RC_OK;
    struct server_config_s sc;
    struct listener_status_array_s lsa;
    struct thread_pipe_array_s tpa;
    struct thread_status_array_s tsa;
    
    LIXA_TRACE_INIT;
    LIXA_TRACE(("main: starting\n"));
    openlog("lixad", LOG_PID, LOG_DAEMON);
    syslog(LOG_NOTICE, "starting");
    /*
    daemonize();
    */
    /* initialize libxml2 library */
    LIBXML_TEST_VERSION;
        
    /* initialize configuration structure */
    server_config_init(&sc, &tpa);
    if (LIXA_RC_OK != (rc = server_config(&sc, &tpa, ""))) {
        LIXA_TRACE(("main/server_config: rc = %d\n", rc));
        syslog(LOG_ERR, "configuration error (%s), premature exit",
               lixa_strerror(rc));
        return rc;
    }

    /* start configured manager(s) */
    if (LIXA_RC_OK != (rc = server_manager(&sc, &tpa, &tsa))) {
        LIXA_TRACE(("main/server_manager: rc = %d\n", rc));
        syslog(LOG_ERR, "error (%s) while starting manager(s), "
               "premature exit", lixa_strerror(rc));
        return rc;
    }

    /* start configured listener(s) */
    if (LIXA_RC_OK != (rc = server_listener(&sc, &lsa))) {
        LIXA_TRACE(("main/server_listener: rc = %d\n", rc));
        syslog(LOG_ERR, "error (%s) while starting listener(s), "
               "premature exit", lixa_strerror(rc));
        return rc;
    }

    sleep(30);
    
    /* it's time to exit */
    syslog(LOG_NOTICE, "exiting");

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
