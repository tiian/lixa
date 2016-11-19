/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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

#include <glib.h>

#endif
#ifdef HAVE_SYSLOG_H

#include <syslog.h>

#endif
#ifdef HAVE_LIBXML_XMLVERSION_H

#include <libxml/xmlversion.h>

#endif
#ifdef HAVE_LIBXML_PARSER_H

#include <libxml/parser.h>

#endif

#include <lixa_trace.h>
#include <lixa_crash.h>
#include <lixa_errors.h>
#include <lixa_tx.h>

/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_CLIENT_TPM

/* command line options */
static gboolean report = FALSE;
static GOptionEntry entries[] =
    {
        {"report", 'r', 0, G_OPTION_ARG_NONE, &report, "Report on all transactions linked to the current configuration and profile"},
        {NULL}
    };

int main(int argc, char **argv)
{
    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;

    LIXA_TRACE(("lixatpm/main: starting\n"));

    GOptionContext *option_context = g_option_context_new(
        "- LIXA transaction process monitor client");
    g_option_context_add_main_entries(option_context, entries, NULL);

    GError *error = NULL;
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        fprintf(stderr, "lixatpm/options: failed=%s\n",
                error->message);

        LIXA_TRACE(("lixatpm/main: exiting"));

        return EXIT_FAILURE;
    }

    g_option_context_free(option_context);

    if (!report) {
        LIXA_TRACE(("lixatpm/main: exiting\n"));

        return EXIT_FAILURE;
    }

    /* initialise libxml2 library */
    LIBXML_TEST_VERSION;

    /* open the resource managers for the current profile */
    int tx_rc;
    lixa_tx_open(&tx_rc, TRUE);
    if (TX_OK != tx_rc) {
        LIXA_TRACE(("lixatpm/lixa_tx_open: tx_rc=%d\n", tx_rc));

        lixa_tx_close(&tx_rc);
        LIXA_TRACE(("lixatpm/lixa_tx_close: tx_rc=%d\n", tx_rc));

        LIXA_TRACE(("lixatpm/main: exiting"));

        return EXIT_FAILURE;
    }

    int rc = LIXA_RC_OK;
    if (LIXA_RC_OK != (rc = lixa_tx_tpm(report))) {
        LIXA_TRACE(("lixatpm/lixa_tx_tpm: rc=%d/%s\n", rc, lixa_strerror(rc)));

        lixa_tx_close(&tx_rc);
        LIXA_TRACE(("lixatpm/lixa_tx_close: tx_rc=%d\n", tx_rc));

        LIXA_TRACE(("lixatpm/main: exiting"));

        return EXIT_FAILURE;
    }

    lixa_tx_close(&tx_rc);
    LIXA_TRACE(("lixatpm/lixa_tx_close: tx_rc=%d\n", tx_rc));

    LIXA_TRACE(("lixatpm/main: exiting"));

    return EXIT_SUCCESS;
}