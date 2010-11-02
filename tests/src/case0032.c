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



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include <tx.h>
#include <liblixamonkey.h>


#include <gmodule.h>
#include <libxml/parser.h>



#define THREAD_NUMBER 100

#define MODE_MODULE_OPEN_CLOSE 0x00000001
#define MODE_XML_READ_FREE     0x00000002
#define MODE_TX_OPEN_CLOSE     0x00000004
#define MODE_THREAD_INIT       0x00000008

/* This case test is for memory leak inspection */



void *transaction(void *parm);


char *LIXAC, *LIXAD, *SWITCH;

GStaticMutex mutex = G_STATIC_MUTEX_INIT;



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    pthread_t tids[THREAD_NUMBER];
    int i,j,n,m;
    long mode;
    
    if (argc < 7) {
        fprintf(stderr, "%s: at least six options must be specified\n",
                argv[0]);
        exit (1);
    }

    m = (int)strtol(argv[1], NULL, 0);
    if (m < 1)
        m = 1;

    n = (int)strtol(argv[2], NULL, 0);
    if (n < 1)
        n = 1;
    else if (n > THREAD_NUMBER)
        n = THREAD_NUMBER;

    mode = strtol(argv[3], NULL, 0);

    LIXAC = argv[4];
    LIXAD = argv[5];
    SWITCH = argv[6];
    
    printf("%s| starting...\n", pgm);

    if (MODE_XML_READ_FREE & mode)
        xmlInitParser();

    for (j=0; j<m; ++j) {
        for (i=0; i<n; ++i) {
            rc = pthread_create(tids+i, NULL, transaction, (void *)&mode);
            assert(0 == rc);
        }
        for (i=0; i<n; ++i) {    
            rc = pthread_join(tids[i], NULL);
            assert(0 == rc);
        }
    }
    
    if (MODE_XML_READ_FREE & mode)
        xmlCleanupParser();

    if (MODE_TX_OPEN_CLOSE & mode)
        lixa_monkeyrm_call_cleanup();
    
    printf("%s| ...finished\n", pgm);
    return 0;
}



void *transaction(void *parm)
{
    GModule *m;
    xmlDocPtr doc;
    int rc;
    long *mode = (long *)parm;
    static const char *XMLBUFFER = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?><msg level=\"0\" verb=\"1\" step=\"8\"><client job=\"00c974e08d8d0e70ccbc420434f45198/127.0.0.1      \" config_digest=\"00c974e08d8d0e70ccbc420434f45198\" maint=\"0\"/><rsrmgrs><rsrmgr rmid=\"0\" dynamic=\"0\" name=\"LIXAmonkey1staRM\" xa_name=\"LIXA Monkey RM (static)\"/></rsrmgrs></msg>";

    if (MODE_TX_OPEN_CLOSE & *mode) {
        printf("tx_open(): %d\n", rc = tx_open());
        assert(TX_OK == rc);

        printf("tx_close(): %d\n", rc = tx_close());
        assert(TX_OK == rc);
    }

    if (MODE_MODULE_OPEN_CLOSE & *mode) {
        printf("locking mutex...\n");
        g_static_mutex_lock(&mutex);
        printf("opening module...\n");
        m = g_module_open(SWITCH, G_MODULE_BIND_LOCAL);
        printf("closing module...\n");
        g_module_close(m);
        printf("unlocking mutex...\n");
        g_static_mutex_unlock(&mutex);
    }

    if (MODE_XML_READ_FREE & *mode) {
        doc = xmlReadFile(LIXAC, NULL, 0);
        xmlFreeDoc(doc);
        doc = xmlReadFile(LIXAD, NULL, 0);
        xmlFreeDoc(doc);

        doc = xmlReadMemory(XMLBUFFER, strlen(XMLBUFFER), "buffer.xml",
                            NULL, 0);
        xmlFreeDoc(doc);
    }

    if (MODE_THREAD_INIT & *mode)
        if (!g_thread_supported ())
            g_thread_init(NULL);
    
    return NULL;
}
