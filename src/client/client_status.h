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
#ifndef CLIENT_STATUS_H
# define CLIENT_STATUS_H



#include <config.h>



#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif



#include <lixa_trace.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE       LIXA_TRACE_MOD_CLIENT_STATUS



/**
 * It's contain the status of a thread connected to a lixa transaction
 * manager
 */
struct client_status_s {
    /**
     * This boolean flag is used to verify if the instantiated object is active
     * or not (garbage can be removed)
     */
    int   active;
    /**
     * Transactional profile associated to the thread (it must be an heap
     * allocated variable)
     */
    char *profile;
};

typedef struct client_status_s client_status_t;



#define KEY_NOT_FOUND -1
#define KEY_ERROR     -2



/**
 * This structure is used to create an index (key,value) couples necessary
 * to efficiently manage the client status set; it's a private structure
 * not exposed to the world
 */
struct client_status_index_s {
    /**
     * thread_id returned by pthread_self is the search key
     */
    pthread_t          key;
    /**
     * position inside array is the value associated to the key
     */
    int                value;
};



/**
 * It's used to store the status of all the thread connected to a
 * lixa transaction manager
 */
struct client_status_coll_s {
    /**
     * This mutex is used to serialize update access to the array
     */
    pthread_rwlock_t              rwlock;
    /**
     * Number of elements allocated in index data array
     */
    int                           index_size;
    /**
     * Index elements
     */
    struct client_status_index_s *index_data;
    /**
     * Number of allocated data elements
     */
    int                           status_size;
    /**
     * Number of used data elements
     */
    int                           status_used;
    /**
     * Elements
     */
    client_status_t              *status_data;
};

typedef struct client_status_coll_s client_status_coll_t;



/**
 * This is a static structure used by all the threads of the program
 * linking the library; the structure i protected by a mutex to avoid
 * concurrency issues */
extern client_status_coll_t csc;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a new "object" of type client status
     * @param cs OUT object reference
     */
    void client_status_init(client_status_t *cs);

    

    /**
     * Is the client status an active slot?
     * @param cs ON object reference
     * @return a boolean condition
     */
    static inline int client_status_is_active(const client_status_t *cs) {
        return cs->active; }


    
    /**
     * Set active status for slot
     * @param cs ON object reference
     */
    static inline void client_status_active(client_status_t *cs) {
        cs->active = TRUE; }


    
    /**
     * Initialize the client status object; this object should be istantiated
     * only once, should be static, initialized at library load, protected
     * against race conditions by a mutex
     * @param csc OUT object reference
     * @return a standardized return code
     */
    int client_status_coll_init(client_status_coll_t *csc);



    /**
     * Register the current thread in the status set
     * @param csc IN/OUT object reference
     * @return a standardized return code
     */
    int client_status_coll_register(client_status_coll_t *csc);



    /**
     * Add a new client status to the set
     * @param csc IN/OUT object reference
     * @param status_pos OUT the position of the slot added and reserved
     * @return a standardized return code
     */
    int client_status_coll_add(client_status_coll_t *csc, int *status_pos);


    
    /**
     * Search the position of the current thread inside the index
     * @param csc IN object reference
     * @param pos PUT the position inside status
     * @return a standardized return code
     */
    int client_status_coll_search(client_status_coll_t *csc, int *pos);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* CLIENT_STATUS_H */
