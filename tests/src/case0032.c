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



/* This case test is for memory leak inspection */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    GModule *m;
    xmlDocPtr doc;
    
    /*
    if (argc < 1) {
        fprintf(stderr, "%s: at least two options must be specified\n",
                argv[0]);
        exit (1);
    }
    */
    
    printf("%s| starting...\n", pgm);

    /*
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);

    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    lixa_monkeyrm_call_cleanup();
    */

/*
    m = g_module_open("/tmp/lixa/lib/switch_lixa_monkeyrm_stareg.so",
                      G_MODULE_BIND_LOCAL);
    g_module_close(m);
*/
    /* try with multithread ... */
    doc = xmlReadFile("/tmp/lixa/etc/lixac_conf.xml", NULL, 0);
    xmlFreeDoc(doc);
    doc = xmlReadFile("/tmp/lixa/etc/lixad_conf.xml", NULL, 0);
    xmlFreeDoc(doc);
    xmlCleanupParser();

    printf("%s| ...finished\n", pgm);
    return 0;
}
