/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
    for (i = 0; i < 1; ++i) {
        /*
        tx_open();
        tx_close();
        */
        tx_open();  
        tx_close();
/*        tx_close();   */
    }
}



int main(int argc, char *argv[])
{
    pthread_t foo;
    int load = 1, i;

    /*
    fprintf(stderr, "pathconf -> %ld\n", pathconf("/home/tiian/lixa", _PC_PATH_MAX));
 
    for (load=0; load<10; ++load) {
        for (i=0; i<load; ++i) {
            pthread_create(&foo, NULL, a_thread, NULL);
        }
        fprintf(stderr, "***************** %d *******************", load);
        sleep(3);
    }
/*
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
*/
    a_thread(&foo);
  
    return 0;
}
