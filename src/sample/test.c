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
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

#include <tx.h>



void *a_thread(void *useless)
{
    int i;
    for (i = 0; i < 10000; ++i) {
        /*
        tx_open();
        tx_close();
        */
        tx_open();  
        tx_close();
/*        tx_close();   */
    }
    return NULL;
}



int main(int argc, char *argv[])
{
    /*
    int foo;

    fprintf(stderr, "pathconf -> %ld\n", pathconf("/home/tiian/lixa", _PC_PATH_MAX));
 
    for (load=0; load<10; ++load) {
        for (i=0; i<load; ++i) {
            pthread_create(&foo, NULL, a_thread, NULL);
        }
        fprintf(stderr, "***************** %d *******************", load);
        sleep(3);
    }

    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    pthread_create(&foo, NULL, a_thread, NULL);
    sleep(30);
    */
    
/*    
    a_thread(&foo);
    a_thread(&foo);
    a_thread(&foo);
    a_thread(&foo);
    a_thread(&foo);
*/
    g_thread_init(NULL);
    return 0;
}
