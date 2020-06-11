/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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



/* Maximum number of clients */
#define MAX_CLIENTS 100

/* Number of cycles executed during warm-up */
#define WARMUP_CYCLES  10
/* Number of cycles executed during benchmark */
#define BENCH_CYCLES   100



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
static gint warmup_cycles = WARMUP_CYCLES;
static gint bench_cycles = BENCH_CYCLES;
static GOptionEntry entries[] =
{
    { "commit", 'c', 0, G_OPTION_ARG_NONE, &commit, "Perform commit transaction(s)", NULL },
    { "rollback", 'r', 0, G_OPTION_ARG_NONE, &rollback, "Perform rollback transaction(s)", NULL },
    { "version", 'v', 0, G_OPTION_ARG_NONE, &print_version, "Print package info and exit", NULL },
    { "benchmark", 'b', 0, G_OPTION_ARG_NONE, &benchmark, "Perform benchmark execution", NULL },
    { "open-close", 'o', 0, G_OPTION_ARG_NONE, &open_close, "Execute tx_open & tx_close for every transaction [benchmark only]", NULL },
    { "csv", 's', 0, G_OPTION_ARG_NONE, &csv, "Send result to stdout using CSV format [benchmark only]", NULL },
    { "clients", 'l', 0, G_OPTION_ARG_INT, &clients, "Number of clients (threads) will stress the state server [benchmark only]", NULL },
    { "medium-delay", 'd', 0, G_OPTION_ARG_INT, &medium_delay, "Average time (expressed in us) used by the Application Program between two TX calls [benchmark only]", NULL },
    { "delta-delay", 'D', 0, G_OPTION_ARG_INT, &delta_delay, "Delta (random) time (expressed in us) used by the Application Program between two TX calls [benchmark only]", NULL },
    { "medium-processing", 'p', 0, G_OPTION_ARG_INT, &medium_processing, "Average time (expressed in us) used by Resource Managers between tx_begin and tx_commit/tx_rollback [benchmark only]", NULL },
    { "delta-processing", 'P', 0, G_OPTION_ARG_INT, &delta_processing, "Delta (random) time (expressed in us) used by Resource Managers between tx_begin and tx_commit/tx_rollback [benchmark only]", NULL },
    { "bench-cycles", 'C', 0, G_OPTION_ARG_INT, &bench_cycles, "Number of trasanctions to be executed by a client [benchmark only]", NULL },
    { NULL }
};



struct timings_s {
    double sum; /* sum of the values until now */
    double avg; /* average value */
    double sum2; /* sum of the square values until now */
    double std_dev; /* standard deviation of the values until now */
};

struct thread_parameters_s {
    int              client_number;
    int              cycles; /* number of cycles to perform */
    double          *open;
    double          *begin;
    double          *comrol; /* commit / rollback */
    double          *close;
};

typedef struct thread_parameters_s thread_parameters_t;



/* print help and exit */
void print_info(TXINFO *info);
/* main function for benchmarking */
int exec_benchmark(void);
/* single client benchmark */
gpointer perform_benchmark(gpointer data);
/* compute aggregate statistics */
void compute_statistics(thread_parameters_t *tp, long total_time);



/* arrays to collect data */
double *open_array;
double *begin_array;
double *comrol_array;
double *close_array;



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
    if (bench_cycles < 1) {
        fprintf(stderr, "bench_cycles (%d) can not be <1, fixed to 1\n",
                bench_cycles);
        bench_cycles = 1;
    }
    warmup_cycles = bench_cycles / WARMUP_CYCLES + 1;
    
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
    gint i, j;
    lixa_timer_t timer;
    long total_time;
    
    fprintf(stderr, "Benchmark mode activated; execution parameters are:\n"
            "Number of clients (threads): %d\n"
            "Number of cycles per client: %d\n"
            "TX completion type: %s\n"
            "tx_open & tx_close for every transaction: %s\n"
            "Delay range between TX functions (in microseconds): [%d,%d]\n"
            "Delay due to Resource Managers (in microseconds): [%d,%d]\n",
            clients, bench_cycles,
            commit ? "commit" : "rollback",
            open_close ? "yes" : "no",
            medium_delay-delta_delay, medium_delay+delta_delay,
            medium_processing-delta_processing,
            medium_processing+delta_processing);
    srandom((unsigned int)time(NULL));

    /* allocate arrays */
    if (NULL == (open_array = calloc(clients*bench_cycles, sizeof(double))) ||
        NULL == (begin_array = calloc(clients*bench_cycles, sizeof(double))) ||
        NULL == (comrol_array = calloc(clients*bench_cycles, sizeof(double))) ||
        NULL == (close_array = calloc(clients*bench_cycles, sizeof(double)))) {
        fprintf(stderr, "Error while allocating arrays\n");
        exit(1);
    }    
    /* warm up phase */
    fprintf(stderr, "Warming up ");
    for (i=0; i<clients; ++i) {
        parameters[i].client_number = i;
        parameters[i].cycles = warmup_cycles;
        parameters[i].open = open_array + i*bench_cycles;
        parameters[i].begin = begin_array + i*bench_cycles;
        parameters[i].comrol = comrol_array + i*bench_cycles;
        parameters[i].close = close_array + i*bench_cycles;
        threads[i] = g_thread_new(
            "", perform_benchmark, (gpointer *)&(parameters[i]));
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

    /* resetting values */
    for (i=0; i<clients; ++i)
        for (j=0; j<bench_cycles; ++j) {
            open_array[i*bench_cycles+j] = 0.0;
            begin_array[i*bench_cycles+j] = 0.0;
            comrol_array[i*bench_cycles+j] = 0.0;
            close_array[i*bench_cycles+j] = 0.0;
        }
            
    /* measurement phase */
    fprintf(stderr, "Measuring  ");
    /* starting timer */
    lixa_timer_start(&timer); 
    for (i=0; i<clients; ++i) {
        parameters[i].client_number = i;
        parameters[i].cycles = bench_cycles;
        parameters[i].open = open_array + i*bench_cycles;
        parameters[i].begin = begin_array + i*bench_cycles;
        parameters[i].comrol = comrol_array + i*bench_cycles;
        parameters[i].close = close_array + i*bench_cycles;
        threads[i] = g_thread_new(
            "", perform_benchmark, (gpointer *)&(parameters[i]));
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
    /* stopping timer */
    lixa_timer_stop(&timer);
    fprintf(stderr, "\n");
    
    total_time = lixa_timer_get_diff(&timer);
    compute_statistics(parameters, total_time);

    /* remove arrays */
    free(open_array);
    free(begin_array);
    free(comrol_array);
    free(close_array);
    
    return 0;
}



gpointer perform_benchmark(gpointer data)
{
    thread_parameters_t *tp;
    int c, rc;
    long diff = 0;
    lixa_timer_t t2;

    tp = (thread_parameters_t *)data;
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
            tp->open[c] = (double)diff/1000;
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
        tp->begin[c] = (double)diff/1000;
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
        tp->comrol[c] = (double)diff/1000;
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
            tp->close[c] = (double)diff/1000;
        }
        
        /* application program delay */
        lixa_micro_sleep(
            (long)(medium_delay + random()%(delta_delay*2) - delta_delay));
    }
    /* compute statistics */
    return NULL;
}



int compare(const void *a, const void *b) {
    if (*(double*)a < *(double*)b)
        return -1;
    else if (*(double*)a > *(double*)b)
        return 1;
    else
        return 0;
}



void compute_statistics(thread_parameters_t *tp, long total_time)
{
    double tps;

    /* sort the arrays with metrics */
    qsort(open_array, clients*bench_cycles, sizeof(double), compare);
    qsort(begin_array, clients*bench_cycles, sizeof(double), compare);
    qsort(comrol_array, clients*bench_cycles, sizeof(double), compare);
    qsort(close_array, clients*bench_cycles, sizeof(double), compare);
    
    tps = (double)clients * (double)tp[0].cycles * (double)1000000 /
        (double)total_time;
    if (!csv)
        printf("tx/sec:\t\t%1.3f tps\n", tps);

    /* New reporting */
    if (csv) {
        /* header 1 */
        printf(" clients,,");
        if (open_close)
            printf("tx_open,,,,,,,,");
        printf("tx_begin,,,,,,,,");
        if (commit)
            printf("tx_commit,,,,,,,,");
        else
            printf("tx_rollback,,,,,,,,");
        if (open_close)
            printf("tx_close,,,,,,,,");
        printf("tps\n");
        /* header 2 */
        printf(" N,samples,");
        if (open_close)
            printf("min,50th%%,75th%%,85th%%,90th%%,95th%%,98th%%,max,");
        printf("min,50th%%,75th%%,85th%%,90th%%,95th%%,98th%%,max,");
        printf("min,50th%%,75th%%,85th%%,90th%%,95th%%,98th%%,max,");
        if (open_close)
            printf("min,50th%%,75th%%,85th%%,90th%%,95th%%,98th%%,max,");
        printf("tps\n");
        /* data */
        printf("%d, %d, ", clients, clients*bench_cycles);
        if (open_close) {
            /* open statistics */
            printf("%1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, ",
                   open_array[0],
                   open_array[clients*bench_cycles*50/100],
                   open_array[clients*bench_cycles*75/100],
                   open_array[clients*bench_cycles*85/100],
                   open_array[clients*bench_cycles*90/100],
                   open_array[clients*bench_cycles*95/100],
                   open_array[clients*bench_cycles*98/100],
                   open_array[clients*bench_cycles-1]);
        }
        /* begin statistics */
        printf("%1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, ",
               begin_array[0],
               begin_array[clients*bench_cycles*50/100],
               begin_array[clients*bench_cycles*75/100],
               begin_array[clients*bench_cycles*85/100],
               begin_array[clients*bench_cycles*90/100],
               begin_array[clients*bench_cycles*95/100],
               begin_array[clients*bench_cycles*98/100],
               begin_array[clients*bench_cycles-1]);
        /* commit/rollback statistics */
        printf("%1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, ",
               comrol_array[0],
               comrol_array[clients*bench_cycles*50/100],
               comrol_array[clients*bench_cycles*75/100],
               comrol_array[clients*bench_cycles*85/100],
               comrol_array[clients*bench_cycles*90/100],
               comrol_array[clients*bench_cycles*95/100],
               comrol_array[clients*bench_cycles*98/100],
               comrol_array[clients*bench_cycles-1]);
        if (open_close) {
            /* close statistics */
            printf("%1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, ",
                   close_array[0],
                   close_array[clients*bench_cycles*50/100],
                   close_array[clients*bench_cycles*75/100],
                   close_array[clients*bench_cycles*85/100],
                   close_array[clients*bench_cycles*90/100],
                   close_array[clients*bench_cycles*95/100],
                   close_array[clients*bench_cycles*98/100],
                   close_array[clients*bench_cycles-1]);
        }
        printf("%1.3f\n", tps);
    } else {
        if (open_close) {
            printf("tx_open():\t");
            printf("min=%1.3fms \t50th%%=%1.3fms \t75th%%=%1.3fms "
                   "\t85th%%=%1.3fms \t90th%%=%1.3fms \t95th%%=%1.3fms "
                   "\t98th%%=%1.3fms \tmax=%1.3fms\n",
                   open_array[0],
                   open_array[clients*bench_cycles*50/100],
                   open_array[clients*bench_cycles*75/100],
                   open_array[clients*bench_cycles*85/100],
                   open_array[clients*bench_cycles*90/100],
                   open_array[clients*bench_cycles*95/100],
                   open_array[clients*bench_cycles*98/100],
                   open_array[clients*bench_cycles-1]);
        }
        printf("tx_begin():\t");
        printf("min=%1.3fms \t50th%%=%1.3fms \t75th%%=%1.3fms "
               "\t85th%%=%1.3fms \t90th%%=%1.3fms \t95th%%=%1.3fms "
               "\t98th%%=%1.3fms \tmax=%1.3fms\n",
               begin_array[0],
               begin_array[clients*bench_cycles*50/100],
               begin_array[clients*bench_cycles*75/100],
               begin_array[clients*bench_cycles*85/100],
               begin_array[clients*bench_cycles*90/100],
               begin_array[clients*bench_cycles*95/100],
               begin_array[clients*bench_cycles*98/100],
               begin_array[clients*bench_cycles-1]);
        if (commit)
            printf("tx_commit():\t");
        else
            printf("tx_rollback():\t");
        printf("min=%1.3fms \t50th%%=%1.3fms \t75th%%=%1.3fms "
               "\t85th%%=%1.3fms \t90th%%=%1.3fms \t95th%%=%1.3fms "
               "\t98th%%=%1.3fms \tmax=%1.3fms\n",
               comrol_array[0],
               comrol_array[clients*bench_cycles*50/100],
               comrol_array[clients*bench_cycles*75/100],
               comrol_array[clients*bench_cycles*85/100],
               comrol_array[clients*bench_cycles*90/100],
               comrol_array[clients*bench_cycles*95/100],
               comrol_array[clients*bench_cycles*98/100],
               comrol_array[clients*bench_cycles-1]);
        if (open_close) {
            printf("tx_close():\t");
            printf("min=%1.3fms \t50th%%=%1.3fms \t75th%%=%1.3fms "
                   "\t85th%%=%1.3fms \t90th%%=%1.3fms \t95th%%=%1.3fms "
                   "\t98th%%=%1.3fms \tmax=%1.3fms\n",
                   close_array[0],
                   close_array[clients*bench_cycles*50/100],
                   close_array[clients*bench_cycles*75/100],
                   close_array[clients*bench_cycles*85/100],
                   close_array[clients*bench_cycles*90/100],
                   close_array[clients*bench_cycles*95/100],
                   close_array[clients*bench_cycles*98/100],
                   close_array[clients*bench_cycles-1]);
        }
    }
    
}
