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



#include <tx.h>
/* @@@
#include <lixa_common_status.h>
*/
#include <lixa_xid.h>



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



void print_info(TXINFO *info);



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
    if (TX_OK != rc) exit(1);
    
    if (commit) {
        printf("tx_begin(): %d\n", rc = tx_begin());
        if (TX_OK != rc) exit(1);
        printf("tx_info(): %d\n", rc = tx_info(&info));
        if (1 != rc) exit(1);
        print_info(&info);
        printf("tx_commit(): %d\n", rc = tx_commit());
        if (TX_OK != rc) exit(1);
    }
    
    if (rollback) {
        printf("tx_begin(): %d\n", rc = tx_begin());
        if (TX_OK != rc) exit(1);
        printf("tx_info(): %d\n", rc = tx_info(&info));
        if (1 != rc) exit(1);
        print_info(&info);
        printf("tx_rollback(): %d\n", rc = tx_rollback());
        if (TX_OK != rc) exit(1);
    }
    
    printf("tx_close(): %d\n", rc = tx_close());
    if (TX_OK != rc) exit(1);
    return 0;
}



void print_info(TXINFO *info)
{
    lixa_ser_xid_t ser_xid = "";
    
    lixa_ser_xid_serialize(ser_xid, &info->xid);
    printf("\txid/formatID    = 0x%lx\n", info->xid.formatID);
    printf("\txid/gtrid.bqual = %s\n", ser_xid);
    return;
}
