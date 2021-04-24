/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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


/* this case test is related to a possible libxml2 memory leak */


#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <libxml/parser.h>


char *XML_STRING="<?xml version=\"1.0\" encoding=\"UTF-8\" ?><foo></foo>";


void *thread1(void *parm);
void *thread2(void *parm);


int main(int argc, char *argv[])
{
    pthread_t t1, t2;
    
    pthread_create(&t1, NULL, thread1, NULL);
    pthread_create(&t2, NULL, thread2, NULL);
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    pthread_create(&t1, NULL, thread1, NULL);
    pthread_create(&t2, NULL, thread2, NULL);
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    return 0;
}


void *thread1(void *parm)
{
    xmlDocPtr doc;
    printf("[1] --- begin ---\n");
    printf("[1] xmlInitParser()\n");
    xmlInitParser();
    printf("[1] xmlReadMemory()\n");
    doc = xmlReadMemory(XML_STRING, strlen(XML_STRING), "buffer.xml", NULL, 0);
    printf("[1] xmlFreeDoc()\n");
    xmlFreeDoc(doc);
    printf("[1] sleep()\n");
    sleep(2);
    printf("[1] xmlCleanupParser()\n");
    xmlCleanupParser();
    printf("[1] --- end ---\n");
    return NULL;
}


void *thread2(void *parm)
{
    xmlDocPtr doc;
    printf("[2] --- begin ---\n");
    printf("[2] sleep()\n");
    sleep(1);
    printf("[2] xmlReadMemory()\n");
    doc = xmlReadMemory(XML_STRING, strlen(XML_STRING), "buffer.xml", NULL, 0);
    printf("[2] xmlFreeDoc()\n");
    xmlFreeDoc(doc);
    printf("[2] --- end ---\n");
    return NULL;
}

