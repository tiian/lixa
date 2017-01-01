/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_MATH_H
# include <math.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_TIME_H
# include <time.h>
#endif
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
static gboolean csv = FALSE;
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
    { "csv", 's', 0, G_OPTION_ARG_NONE, &csv, "Send result to stdout using CSV format [benchmark only]", NULL },
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
 */
#define NUMBER_OF_SAMPLES 4

/* Maximum number of clients */
#define MAX_CLIENTS 100

/* Number of cycles executed during warm-up */
#define WARM_UP_CYCLES  10
/* Number of cycles executed during benchmark */
#define BENCH_CYCLES   100



struct timings_s {
    double sum; /* sum of the values until now */
    double avg; /* average value */
    double sum2; /* sum of the square values until now */
    double std_dev; /* standard deviation of the values until now */
};

struct thread_parameters_s {
    int              client_number;
    int              cycles; /* number of cycles to perform */
    struct timings_s timings[NUMBER_OF_SAMPLES];
};

typedef struct thread_parameters_s thread_parameters_t;



/* print help and exit */
void print_info(TXINFO *info);
/* main function for benchmarking */
int exec_benchmark(void);
/* single client benchmark */
gpointer perform_benchmark(gpointer data);
/* compute aggregate statistics */
void compute_statistics(thread_parameters_t *tp);



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

    if (clients > MAX_CLIENTS || clients < 1) {
        fprintf(stderr, "Number of clients (%d) is out of range [%d, %d]\n",
                clients, 1, MAX_CLIENTS);
        exit(1);
    }
    
    if (benchmark && (!commit) && (!rollback))
        commit = TRUE;

    if (medium_delay < 0) {
        fprintf(stderr, "medium_delay (%d) can not be <0, fixed to 0\n",
                medium_delay);
        medium_delay = 0;
    }
    if (delta_delay < 1) {
        fprintf(stderr, "delta_delay (%d) can not be <1, fixed to 1\n",
                delta_delay);
        delta_delay = 1;
    }
    if (medium_processing < 0) {
        fprintf(stderr, "medium_processing (%d) can not be <0, fixed to 0\n",
                medium_processing);
        medium_processing = 0;
    }
    if (delta_processing < 1) {
        fprintf(stderr, "delta_processing (%d) can not be <1, fixed to 1\n",
                delta_processing);
        delta_processing = 1;
    }
    
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
    thread_parameters_t parameters[MAX_CLIENTS];
    GThread *threads[MAX_CLIENTS];
    gint i;
    
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
    srandom((unsigned int)time(NULL));
    /* initialize multithread environment */
    g_thread_init(NULL);
    /* warm up phase */
    fprintf(stderr, "Warming up ");
    for (i=0; i<clients; ++i) {
        parameters[i].client_number = i;
        parameters[i].cycles = WARM_UP_CYCLES;
        threads[i] = g_thread_create(
            perform_benchmark, (gpointer *)&(parameters[i]), TRUE, NULL);
        if (NULL == threads[i]) {
            fprintf(stderr, "Error while creating thread number %d\n", i);
            break;
        }
    }
    /* wait thread termination */
    for (i=0; i<clients; ++i) {
        if (NULL != threads[i]) {
            g_thread_join(threads[i]);
        }
    }
    fprintf(stderr, "\n");

    /* measurement phase */
    fprintf(stderr, "Measuring  ");
    for (i=0; i<clients; ++i) {
        parameters[i].client_number = i;
        parameters[i].cycles = BENCH_CYCLES;
        threads[i] = g_thread_create(
            perform_benchmark, (gpointer *)&(parameters[i]), TRUE, NULL);
        if (NULL == threads[i]) {
            fprintf(stderr, "Error while creating thread number %d\n", i);
            break;
        }
    }
    /* wait thread termination */
    for (i=0; i<clients; ++i) {
        if (NULL != threads[i]) {
            g_thread_join(threads[i]);
        }
    }
    fprintf(stderr, "\n");
    
    compute_statistics(parameters);
    return 0;
}



gpointer perform_benchmark(gpointer data)
{
    thread_parameters_t *tp;
    int c, s, rc;
    long diff = 0;
    lixa_timer_t t2;

    tp = (thread_parameters_t *)data;
    /* reset measures */
    for (s=0; s<NUMBER_OF_SAMPLES; ++s) {
        tp->timings[s].sum = tp->timings[s].avg = tp->timings[s].sum2 =
            tp->timings[s].std_dev = 0.0;
    }
    /* perform cycles */
    for (c=0; c<tp->cycles; ++c) {
        if (c+1 == tp->cycles*tp->client_number/clients)
            fprintf(stderr, ".");
        /* application program delay */
        lixa_micro_sleep(
            (long)(medium_delay + random()%(delta_delay*2) - delta_delay));
        /* tx_open if necessary */
        if (c == 0 || open_close) {
            lixa_timer_start(&t2);
            rc = tx_open();
            lixa_timer_stop(&t2);
            if (TX_OK != rc) {
                fprintf(stderr, "client %d, tx_open(): %d\n",
                        tp->client_number, rc);
                exit(1);
            }
            diff = lixa_timer_get_diff(&t2);
            if (c == 0) {
                tp->timings[0].sum = (double)diff;
                tp->timings[0].sum2 = (double)diff * (double)diff;
                tp->timings[0].std_dev = 0;
            } else {
                tp->timings[0].sum += (double)diff;
                tp->timings[0].sum2 += (double)diff * (double)diff;
            }
        }
        /* application program delay */
        lixa_micro_sleep(
            (long)(medium_delay + random()%(delta_delay*2) - delta_delay));
        lixa_timer_start(&t2);
        rc = tx_begin();
        lixa_timer_stop(&t2);
        if (TX_OK != rc) {
            fprintf(stderr, "client %d, tx_begin(): %d\n",
                    tp->client_number, rc);
            exit(1);
        }
        diff = lixa_timer_get_diff(&t2);
        if (c == 0) {
            tp->timings[1].sum = (double)diff;
            tp->timings[1].sum2 = (double)diff * (double)diff;
            tp->timings[1].std_dev = 0;
        } else {
            tp->timings[1].sum += (double)diff;
            tp->timings[1].sum2 += (double)diff * (double)diff;
        }
        /* resource manager delay */
        lixa_micro_sleep(
            (long)(medium_processing + random()%(delta_processing*2) -
                   delta_processing));
        
        if (commit) {
            lixa_timer_start(&t2);
            rc = tx_commit();
            lixa_timer_stop(&t2);
            if (TX_OK != rc) {
                fprintf(stderr, "client %d, tx_commit(): %d\n",
                        tp->client_number, rc);
                exit(1);
            }
        } else {
            lixa_timer_start(&t2);
            rc = tx_rollback();
            lixa_timer_stop(&t2);
            if (TX_OK != rc) {
                fprintf(stderr, "client %d, tx_rollback(): %d\n",
                        tp->client_number, rc);
                exit(1);
            }        
        }
        diff = lixa_timer_get_diff(&t2);
        if (c == 0) {
            tp->timings[2].sum = (double)diff;
            tp->timings[2].sum2 = (double)diff * (double)diff;
            tp->timings[2].std_dev = 0;
        } else {
            tp->timings[2].sum += (double)diff;
            tp->timings[2].sum2 += (double)diff * (double)diff;
        }
        /* application program delay */
        lixa_micro_sleep(
            (long)(medium_delay + random()%(delta_delay*2) - delta_delay));
        
        if (c == tp->cycles-1 || open_close) {
            lixa_timer_start(&t2);
            rc = tx_close();
            lixa_timer_stop(&t2);
            if (TX_OK != rc) {
                fprintf(stderr, "client %d, tx_close(): %d\n",
                        tp->client_number, rc);
                exit(1);        
            }
            diff = lixa_timer_get_diff(&t2);
            if ((open_close && c == 0) || (!open_close && c == tp->cycles-1)) {
                tp->timings[3].sum = (double)diff;
                tp->timings[3].sum2 = (double)diff * (double)diff;
                tp->timings[3].std_dev = 0;
            } else if (open_close) {
                tp->timings[3].sum += (double)diff;
                tp->timings[3].sum2 += (double)diff * (double)diff;
            }
        }
        
        /* application program delay */
        lixa_micro_sleep(
            (long)(medium_delay + random()%(delta_delay*2) - delta_delay));
    }
    /* compute statistics */
    return NULL;
}



void compute_statistics(thread_parameters_t *tp)
{
    int c, s;
    double N, sum, sum2, avg, std_dev;

    if (csv) {
        printf(" clients,tx_open,,tx_begin,,");
        if (commit)
            printf("tx_commit,,");
        else
            printf("tx_rollback,,");
        printf("tx_close\n");
        printf(" N,avg,std,avg,std,avg,std,avg,std\n");
        printf("%d,", clients);
    }
    
    for (s=0; s<NUMBER_OF_SAMPLES; ++s) {
        sum = sum2 = avg = std_dev = 0.0;
        for (c=0; c<clients; ++c) {
            sum += tp[c].timings[s].sum;
            sum2 += tp[c].timings[s].sum2;
        }
        if (!open_close && (s==0 || s==3))
            N = (double)clients;
        else
            N = (double)clients * (double)tp[0].cycles;
        
        /* timing stats in ms instead of us */
        avg = sum / N / 1000;
        std_dev = sqrt(N*sum2 - sum*sum) / N / 1000;
        if (!csv) {
            switch (s) {
                case 0: printf("tx_open():\t");
                    break;
                case 1: printf("tx_begin():\t");
                    break;
                case 2:
                    if (commit)
                        printf("tx_commit():\t");
                    else
                        printf("tx_rollback():\t");
                    break;
                case 3: printf("tx_close():\t");
                    break;
                default:
                    fprintf(stderr, "Internal error: s=%d\n", s);
                    exit(1);
            }
            printf("avg=%7.3f ms,\tstd_dev=%7.3f ms\n", avg, std_dev);
        } else
            printf("%7.3f,%7.3f,", avg, std_dev);
    }
    if (csv)
        printf("\n");
}
