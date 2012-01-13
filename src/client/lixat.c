/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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



/* this utility program can be used to test the installation and to test
   the performance (benchmark) */



/* default command line options */
static gboolean commit = FALSE;
static gboolean rollback = FALSE;
static gboolean print_version = FALSE;
static gboolean benchmark = FALSE;
static gboolean open_close = FALSE;
static gint clients = 10;
static gint medium_delay = 1000;
static gint delta_delay = 500;
static gint medium_processing = 100000;
static gint delta_processing = 50000;
static GOptionEntry entries[] =
{
    { "commit", 'c', 0, G_OPTION_ARG_NONE, &commit, "Perform commit transaction(s)", NULL },
    { "rollback", 'r', 0, G_OPTION_ARG_NONE, &rollback, "Perform rollback transaction(s)", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { "benchmark", 'b', 0, G_OPTION_ARG_NONE, &benchmark, "Perform benchmark execution", NULL },
    { "open-close", 'o', 0, G_OPTION_ARG_NONE, &open_close, "Execute tx_open & tx_close for every transaction [benchmark only]", NULL },
    { "clients", 'l', 0, G_OPTION_ARG_INT, &clients, "Number of clients (threads) will stress the state server [benchmark only]", NULL },
    { "medium-delay", 'd', 0, G_OPTION_ARG_INT, &medium_delay, "Medium (random) delay between TX functions [benchmark only]", NULL },
    { "delta-delay", 'D', 0, G_OPTION_ARG_INT, &delta_delay, "Delta (random) delay between TX functions [benchmark only]", NULL },
    { "medium-processing", 'p', 0, G_OPTION_ARG_INT, &medium_processing, "Medium (random) delay introduced by Resource Managers operations between tx_begin and tx_commit/tx_rollback [benchmark only]", NULL },
    { "delta-processing", 'P', 0, G_OPTION_ARG_INT, &delta_processing, "Delta (random) delay introduced by Resource Managers operations between tx_begin and tx_commit/tx_rollback [benchmark only]", NULL },
    { NULL }
};

/*
 * T[0]: tx_open() timing
 * T[1]: tx_begin() timing
 * T[2]: tx_commit()/tx_rollback() timing
 * T[3]: tx_close timing
 * T[4]: global timing
 */
#define NUMBER_OF_SAMPLES 5

struct timings_s {
    long min; /* minimum value */
    long max; /* masimum value */
    double sum; /* sum of the values until now */
    double sum2; /* sum of the square values until now */
    double std_dev; /* standard deviation of the values until now */
};

typedef struct timings_s values_s[NUMBER_OF_SAMPLES];


void print_info(TXINFO *info);
int exec_benchmark(void);



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

    g_option_context_free(option_context);
    
    if (print_version) {
        lixa_print_version(stdout);
        exit(0);
    }

    if (benchmark && commit && rollback) {
        fprintf(stderr, "Either 'commit' or 'rollback' can be used when "
                "benchmark mode is specified\n");
        exit(1);
    }

    if (benchmark && (!commit) && (!rollback))
        commit = TRUE;
    
    if (!benchmark) {
        /* perform one shot test execution */
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
    } else {
        /* perform benchmark execution */
        if (0 != exec_benchmark())
            exit (1);
    }
    
    return 0;
}



void print_info(TXINFO *info)
{
    lixa_ser_xid_t ser_xid = "";
    
    lixa_xid_serialize(&info->xid, ser_xid);
    printf("\txid/formatID.gtrid.bqual = %s\n", ser_xid);
    return;
}



int exec_benchmark(void)
{
    fprintf(stderr, "Benchmark mode activated; execution parameters are:\n"
            "Number of clients (threads): %d\n"
            "TX completion type: %s\n"
            "tx_open & tx_close for every transaction: %s\n"
            "Delay range between TX functions (in microseconds): [%d,%d]\n"
            "Delay due to Resource Managers (in microseconds): [%d,%d]\n",
            clients,
            commit ? "commit" : "rollback",
            open_close ? "yes" : "no",
            medium_delay-delta_delay, medium_delay+delta_delay,
            medium_processing-delta_processing,
            medium_processing+delta_processing);
    /* @@@ start threads with a dedicated function */

    /* @@@ display results */
    return 0;
}
