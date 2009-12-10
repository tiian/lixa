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



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <lixa_trace.h>
#include <client_config.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE       LIXA_TRACE_MOD_CLIENT_STATUS



/**
 * No RMs have been opened or initialized. An application thread of control
 * cannot start a global transaction until it has successfully opened its RMs
 * via @ref tx_open().
 */
#define TX_STATE_S0    0
/**
 * The thread haso opened its RMs but is not in a transaction. Its transaction
 * control characteristics is TX_UNCHAINED
 */
#define TX_STATE_S1    1
/**
 * The thread haso opened its RMs but is not in a transaction. Its transaction
 * control characteristics is TX_CHAINED
 */
#define TX_STATE_S2    2
/**
 * The thread has opened its RMs and is in a transaction. Its transaction
 * control characteristics is TX_UNCHAINED
 */
#define TX_STATE_S3    3
/**
 * The thread has opened its RMs and is in a transaction. Its transaction
 * control characteristics is TX_CHAINED
 */
#define TX_STATE_S4    4



/**
 * It contains the status of a thread connected to a lixa transaction
 * manager
 */
struct client_status_s {
    /**
     * This boolean flag is used to verify if the instantiated object is active
     * or not (garbage can be removed)
     */
    int   active;
    /**
     * The file descriptor associated to the socket connected to the server
     */
    int   sockfd;
    /**
     * Status as described in table 7-1 ("C-language State Tables", chapter 7,
     * X/Open CAE Specification, Distributed Transaction Processing:
     * The TX (Transaction Demarcation) Specification
     */
    int   txstate;
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
     * This lock is used to serialize read & update access to the array
     */
    GStaticRWLock                 rwlock;
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
extern client_status_coll_t global_csc;



/*
 * This static structure is used by all the threads of the program and contains
 * the configuration read by the first thread and used by all the thread
 * hosted by the same process */
extern client_config_coll_t global_ccc;



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
     * @param cs IN/OUT object reference
     */
    static inline void client_status_active(client_status_t *cs) {
        cs->active = TRUE; }



    /**
     * Set the file descriptor associated to the socket used for client/server
     * communication
     * @param cs IN/OUT object reference
     * @param fd IN a valid file descriptor
     */
    static inline void client_status_set_sockfd(client_status_t *cs, int fd) {
        cs->sockfd = fd; }
    
    

    /**
     * Get the TX state associated to the thread
     * @param cs IN object reference
     * @return the TX state
     */
    static inline int client_status_get_txstate(const client_status_t *cs) {
        return cs->txstate; }


    
    /**
     * Set the TX state associated to the thread
     * @param cs IN/OUT object reference
     * @param txstate IN a valid TX state
     */
    static inline void client_status_set_txstate(client_status_t *cs,
                                                 int txstate) {
        cs->txstate = txstate; }
    
    

    /**
     * Get the file descriptor associated to the socket used for client/server
     * communication
     * @param cs IN object reference
     * @return the file descriptor
     */
    static inline int client_status_get_sockfd(const client_status_t *cs) {
        return cs->sockfd; }


    
    /* @@@ obsolete, remove!
     * Initialize the client status object; this object should be istantiated
     * only once, should be static, initialized at library load, protected
     * against race conditions by a mutex
     * @param csc OUT object reference
     * @return a standardized return code
    int client_status_coll_init(client_status_coll_t *csc);
     */



    /**
     * Return the status of a specific thread; this method MUST be protected
     * by a rdlock because the array can change while the method is in progress
     * @param csc IN object reference
     * @param pos IN position of the desired thread
     *               (@ref client_status_coll_search)
     * @return a reference to the desired object
     */
    static inline client_status_t *client_status_coll_get_status(
        client_status_coll_t *csc, int pos) {
        return &csc->status_data[pos]; }

    

    /**
     * Retrieve a (stable) reference to the current thread client status; this
     * method is safe because it's internally lock protected
     * @param csc IN object reference
     * @param cs OUT reference to the output object (current thread client
     *               status)
     * @return a standardized return code
     */
    int client_status_coll_get_cs(client_status_coll_t *csc,
                                  client_status_t **cs);


    
    /**
     * Register the current thread in the status set
     * @param csc IN/OUT object reference
     * @param pos OUT the position of the slot assigned to this thread
     * @return a standardized return code
     */
    int client_status_coll_register(client_status_coll_t *csc, int *pos);



    /**
     * Add a new client status to the set
     * @param csc IN/OUT object reference
     * @param status_pos OUT the position of the slot added and reserved
     * @return a standardized return code
     */
    int client_status_coll_add(client_status_coll_t *csc, int *status_pos);



    /**
     * Remove a client status from the set
     * @param csc IN/OUT object reference
     * @return a standardize return code
     */
    int client_status_coll_del(client_status_coll_t *csc);


    
    /**
     * Search the position of the current thread inside the index
     * @param csc IN object reference
     * @param pos OUT the position inside status
     * @param lock IN should the method lock the container: FALSE if the
     *                container is already locked, TRUE otherwise
     * @return a standardized return code
     */
    int client_status_coll_search(client_status_coll_t *csc, int *pos,
                                  int lock);


    
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
