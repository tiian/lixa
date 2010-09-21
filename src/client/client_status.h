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
#ifndef CLIENT_STATUS_H
# define CLIENT_STATUS_H



#include <config.h>



#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
# include <time.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <tx.h>
#include <lixa_crash.h>
#include <lixa_trace.h>
#include <lixa_common_status.h>
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
 * Status of the resource manager: this is a volatile status, the persistent
 * one is stored server side
 */
struct client_status_rsrmgr_s {
    /**
     * It store the common (client & server) part of the rsrmgr status
     */
    struct common_status_rsrmgr_s common;
    /**
     * It stores the return code of xa_prepare() that must be used when
     * evaluating the return code of xa_rollback() under some circumstances;
     * this field is NOT propagated to the server because the server already
     * stores it; 
     */
    int   prepare_rc;
};



/**
 * It contains the status of a thread connected to a lixa transaction
 * manager
 */
struct client_status_s {
    /**
     * This boolean flag is used to verify if the instantiated object is active
     * or not (garbage can be removed)
     */
    int                             active;
    /**
     * The file descriptor associated to the socket connected to the server
     */
    int                             sockfd;
    /**
     * State of the control thread
     */
    struct common_status_conthr_s   state;
    /**
     * State of the partecipating resource managers (see @ref
     * client_status_rsrmgr_s )
     */
    GArray                         *rmstatus;
    /**
     * The state of the transaction (with relationship to @ref tx_info and
     * @ref tx_set_transaction_timeout )
     */
    TRANSACTION_STATE               tx_state;
    /**
     * Number of seconds between @ref tx_begin and @ref tx_commit or
     * @ref tx_rollback ; 0 means not timeout
     */
    TRANSACTION_TIMEOUT             tx_timeout;
    /**
     * Tiemout time of the current transaction
     */
    time_t                          tx_timeout_time;
    /**
     * An operation returned TX_FAIL; the transaction manager and/or one or
     * more of the resource managers can no longer perform work on behalf of
     * the application
     */
    int                             failed;
#ifdef _CRASH
    /**
     * Counter used for crash simulation feature
     */
    int                             crash_count;
#endif
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
     * Initialize a @ref client_status_rsrmgr_s struct
     * @param csr OUT reference to the structure must be initialized
     * @param dynamic IN boolean: is the resource manager using dynamic
     *                   registration?
     */
    static inline void client_status_rsrmgr_init(
        struct client_status_rsrmgr_s *csr, int dynamic) {
        common_status_rsrmgr_init(&(csr->common), dynamic);
        csr->prepare_rc = XA_OK;
    }
    

    
    /**
     * Initialize a new "object" of type client status
     * @param cs OUT object reference
     */
    void client_status_init(client_status_t *cs);

    

    /**
     * Free the dynamic memory associated to a client status
     * @param cs OUT object reference
     */
    void client_status_free(client_status_t *cs);


    
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
     * Get the file descriptor associated to the socket used for client/server
     * communication
     * @param cs IN object reference
     * @return the file descriptor
     */
    static inline int client_status_get_sockfd(const client_status_t *cs) {
        return cs->sockfd; }


    
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
        return cs->state.txstate; }


    
    /**
     * Set the TX state associated to the thread
     * @param cs IN/OUT object reference
     * @param txstate IN a valid TX state
     */
    static inline void client_status_set_txstate(client_status_t *cs,
                                                 int txstate) {
        cs->state.txstate = txstate; }
    
    

    /**
     * Get the transaction ID associated to the thread
     * @param cs IN object reference
     * @return a reference to the transaction ID
     */
    static inline XID *client_status_get_xid(client_status_t *cs) {
        return &cs->state.xid; }


    
    /**
     * Get the transaction ID associated to the thread
     * @param cs IN/OUT object reference
     * @param xid IN a transaction ID 
     */
    static inline void
    client_status_set_xid(client_status_t *cs, const XID *xid) {
        cs->state.xid = *xid; }



    /**
     * Get the tx_state property; this seems a duplicate of txstate, but the
     * flaw is in TX specification (take a look to tx.h for the values of
     * this property
     */
    static inline TRANSACTION_STATE
    client_status_get_tx_state(client_status_t *cs) {
        return cs->tx_state; }


    
    /**
     * Set the tx_state property; this seems a duplicate of txstate, but the
     * flaw is in TX specification (take a look to tx.h for the values of
     * this property
     */
    static inline void client_status_set_tx_state(client_status_t *cs,
                                                  TRANSACTION_STATE tx_state) {
        cs->tx_state = tx_state; }


    
    /**
     * Get the tx_timeout property
     */
    static inline TRANSACTION_TIMEOUT
    client_status_get_tx_timeout(client_status_t *cs) {
        return cs->tx_timeout; }


    
    /**
     * Set the tx_timeout property
     */
    static inline void client_status_set_tx_timeout(
        client_status_t *cs, TRANSACTION_TIMEOUT tx_timeout) {
        cs->tx_timeout = tx_timeout; }


    
    /**
     * Get the tx_timeout_time property
     */
    static inline time_t
    client_status_get_tx_timeout_time(client_status_t *cs) {
        return cs->tx_timeout_time; }
    

    
    /**
     * Set the tx_timeout_time property
     */
    static inline void client_status_set_tx_timeout_time(
        client_status_t *cs, time_t tx_timeout_time) {
        cs->tx_timeout_time = tx_timeout_time; }
    


    /**
     * Is timeout time expired?
     * @param cs IN client status reference
     * @return a boolean value, TRUE means timeout is expired
     */
    static inline int client_status_is_tx_timeout_time(client_status_t *cs) {
        struct timeval tv;
        if (0 == cs->tx_timeout_time)
            return FALSE;
        else if (TX_TIMEOUT_ROLLBACK_ONLY == cs->tx_state)
            return TRUE;
        else {
            gettimeofday(&tv, NULL);
            if (tv.tv_sec > cs->tx_timeout_time) {
                cs->tx_state = TX_TIMEOUT_ROLLBACK_ONLY;
                return TRUE;
            } else
                return FALSE;
        }
    }



    /**
     * Is the failed flag active?
     * @param cs ON object reference
     * @return a boolean condition
     */
    static inline int client_status_is_failed(const client_status_t *cs) {
        return cs->failed; }


    
    /**
     * Set failed flag for the client
     * @param cs IN/OUT object reference
     */
    static inline void client_status_failed(client_status_t *cs) {
        cs->failed = TRUE; }



#ifdef _CRASH
    /**
     * Get a writable reference to crash_count property
     * @param cs IN object reference
     * @return a writable reference to the number of times the crash point
     * was traversed
     */
    static inline int *client_status_get_crash_count(client_status_t *cs) {
        return &cs->crash_count;
    }
#endif



    /**
     * Used to interrogate client status and understand if "one phase commit"
     * instead of two phase commit can be performed. The result is dynamically
     * computed and can depends on: <br>
     * - the number of static Resource Managers <br>
     * - the number of registered dynamic Resource Managers <br>
     * @param cs IN client status reference
     * @return a boolean value; if any error happens, it return FALSE
     *         (conservative behavior)
     */
    int client_status_could_one_phase(const client_status_t *cs);

    

    /**
     * Return the status of a specific thread; this method MUST be protected
     * by a rdlock because the array can change while the method is in progress
     * @param csc IN object reference
     * @param pos IN position of the desired thread
     *               ( @ref client_status_coll_search )
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



    /**
     * Check if client status collection is empty
     * @param csc IN object reference
     * @return a boolean value
     */
    int client_status_coll_is_empty(const client_status_coll_t *csc);


    
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
