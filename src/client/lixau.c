/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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



#include <tx.h>
#include <lixa_xid.h>
#include <lixa_utils.h>



/* this utility program is useful for some special tricks */



/* default command line options */
static gboolean formatid = FALSE;
static gboolean print_version = FALSE;
static GOptionEntry entries[] =
{
    { "formatid", 'f', 0, G_OPTION_ARG_NONE, &formatid, "Print the LIXA formatID", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { NULL }
};



void print_info(TXINFO *info);



int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *option_context = NULL;
    
    option_context = g_option_context_new("- LIXA test utility");
    g_option_context_add_main_entries(option_context, entries, NULL);
    
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        fprintf(stderr, "option parsing failed: %s\n", error->message);
        exit(1);
    }

    g_option_context_free(option_context);
    
    if (print_version) {
        lixa_print_version(stdout);
        exit(0);
    }

    if (formatid) {
        lixa_ser_xid_t lsx;
        lixa_xid_formatid(lsx);
        printf("%s\n", lsx);
    }

    return 0;
}



