/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include <tx.h>
#include <lixa_common_status.h>



/* this utility program can be used to verify the installation */



/* default command line options */
static gboolean commit = FALSE;
static gboolean rollback = FALSE;
static GOptionEntry entries[] =
{
    { "commit", 'c', 0, G_OPTION_ARG_NONE, &commit, "Perform a commit transaction", NULL },
    { "rollback", 'r', 0, G_OPTION_ARG_NONE, &rollback, "Perform a rollback transaction", NULL },
    { NULL }
};



int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *option_context = NULL;

    int rc;
    TXINFO info;

    option_context = g_option_context_new("- LIXA test utility");
    g_option_context_add_main_entries(option_context, entries, NULL);
    
    if (!g_option_context_parse(option_context, &argc, &argv, &error)) {
        fprintf(stderr, "option parsing failed: %s\n", error->message);
        exit(1);
    }

    printf("tx_open(): %d\n", rc = tx_open());
    assert(TX_OK == rc);
    
    if (commit) {
        char *ser_xid = NULL;
        
        printf("tx_begin(): %d\n", rc = tx_begin());
        assert(TX_OK == rc);
        printf("tx_info(): %d\n", rc = tx_info(&info));
        assert(1 == rc);
        ser_xid = xid_serialize(&info.xid);
        printf("\txid[formatID]    = 0x%lx\n", info.xid.formatID);
        printf("\txid[gtrid.bqual] = %s\n", ser_xid);
        if (NULL != ser_xid)
            free(ser_xid);
        printf("tx_commit(): %d\n", rc = tx_commit());
        assert(TX_OK == rc);
    }
    
    if (rollback) {
        char *ser_xid = NULL;
        
        printf("tx_begin(): %d\n", rc = tx_begin());
        assert(TX_OK == rc);
        printf("tx_info(): %d\n", rc = tx_info(&info));
        assert(1 == rc);
        ser_xid = xid_serialize(&info.xid);
        printf("\txid[formatID]    = 0x%lx\n", info.xid.formatID);
        printf("\txid[gtrid.bqual] = %s\n", ser_xid);
        if (NULL != ser_xid)
            free(ser_xid);
        printf("tx_rollback(): %d\n", rc = tx_rollback());
        assert(TX_OK == rc);
    }
    
    printf("tx_close(): %d\n", rc = tx_close());
    assert(TX_OK == rc);
    return 0;
}
