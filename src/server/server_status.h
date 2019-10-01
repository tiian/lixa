/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_STATUS_H
# define SERVER_STATUS_H



#include "config.h"



#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif



#include "lixa_config.h"
#include "lixa_xml_msg.h"
#include "lixa_utils.h"
#include "lixa_state.h"
#include "xa.h"
#include "srvr_rcvr_tbl.h"
#include "server_fsm.h"
#include "server_trans_tbl.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Initial size of the status file
 */
#define STATUS_FILE_INIT_SIZE 10
/**
 * Percentual increment change of the status file
 */
#define STATUS_FILE_DELTA_SIZE 10
/**
 * Magic number of the lixa status files
 */
#define STATUS_FILE_MAGIC_NUMBER 0x6110
/**
 * Level (versione) of the lixa status files
 */
#define STATUS_FILE_LEVEL 0x0001
/**
 * Suffix for the first status file
 */
#define STATUS_FILE_SUFFIX_1  "_1"
/**
 * Suffix for the second status file
 */
#define STATUS_FILE_SUFFIX_2  "_2"






/**
 * Three different types of shutdown  can be requested to LIXA server:
 * QUIESCE: the server will wait every transaction completed; no new
 *          transaction can be started
 * IMMEDIATE: the server will flush the connected clients, synchronize status
 *            files and exit
 * FORCE: the server will synchronize status files and abruptly exit
 */
enum shutdown_type_e { SHUTDOWN_NULL
                       , SHUTDOWN_QUIESCE
                       , SHUTDOWN_IMMEDIATE
                       , SHUTDOWN_FORCE };



/**
 * It contains the configuration common to any thread
 */
struct thread_pipe_s
{
    /**
     * This pipe is used to receive commands from other threads
     * as specified by POSIX
     * http://www.opengroup.org/onlinepubs/009695399/toc.htm
     * pipefd[1] must be used for writing
     * pipefd[0] must be used for reading
     */
    int pipefd[2];
};



/**
 * It contains the common configuration of all activated threads
 * thread 0 is the listener thread, threads 1..n are the serving threads
 */
struct thread_pipe_array_s
{
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct thread_pipe_s *array;
};



/**
 * This "object" is statically allocated outside main stack because it must
 * be accessible from signal handler
 */
extern struct thread_pipe_array_s tpa;



/**
 * This is a convenience struct for @ref server_client_status_s
 */
struct thread_status_switch_s
{
    /**
     * This value is a flag (TRUE if != 0) that specifies the condition:
     * the current slot_id (and correlated block_id) must be transferred to
     * a different thread; the value is the destination thread id itself
     */
    int id;
    /**
     * Number of significative bytes in buffer
     */
    size_t buffer_size;
    /**
     * Points to a dynamically allocated buffer with the input read from the
     * source thread and must be passed to the destination thread; the buffer
     * must be released by the destination thread
     */
    char *buffer;
};



/**
 * Used to save the result of the branch join that's the result of the
 * multiple transaction global prepare
 */
enum server_client_branch_join_e {
    /** Null value, init value */
    CLIENT_BRANCH_JOIN_NULL,
    /** All the branches completed OK, join OK */
    CLIENT_BRANCH_JOIN_OK,
    /** Some branches completed with errors, join KO */
    CLIENT_BRANCH_JOIN_KO };



/**
 * It's the struct used to keep the status of a client
 */
struct server_client_status_s
{
    /**
     * Client session associated to TCP/IP connection
     */
    lixa_session_t session;
    /**
     * Finite state machine associate to the client session
     */
    server_fsm_t fsm;
    /**
     * Place inside the persistent status (memory mapped records) used to store
     * the status of this client
     */
    uint32_t pers_status_slot_id;
    /**
     * Buffer that must be sent to the client: if NULL, no buffer needs to be
     * sent
     */
    char *output_buffer;
    /**
     * Size of the buffer that must be sent to the client;
     * if @ref output_buffer is NULL, this field is meaningless
     */
    size_t output_buffer_size;
    /**
     * Message that must be sent to the client; don't confuse this internal
     * representation with the serialized buffer that can be sent on the wire
     */
    struct lixa_msg_s output_message;
    /**
     * (verb,step) related to the output_buffer
     */
    struct lixa_msg_verb_step_s last_verb_step;
    /**
     * Result of the branch join that's the result of the
     * multiple transaction global prepare
     */
    enum server_client_branch_join_e   branch_join;
    /**
     * Info necessary to switch the current client to a different thread
     */
    struct thread_status_switch_s switch_thread;
};



/**
 * It is used to code two ordered references to two different memory mapped
 * status files; for example, @ref traverse_and_copy uses this struct as
 * data argument to copy from first to second status file
 */
struct two_status_record_s
{
    /**
     * First reference; @ref traverse_and_copy uses it as the source
     */
    status_record_t *first;
    /**
     * Second reference; @ref traverse_and_copy uses it as the destination
     */
    status_record_t *second;
};



/**
 * The base struct for object @ref status_sync_t
 */
struct status_sync_s
{
    /**
     * Number of sessions have requested state synchronization
     */
    int asked_sync;
    /**
     * Timer started at first state synchronization request
     */
    lixa_timer_t sync_delay;
};



/**
 * The object tells how many sessions have asked state synchronization and
 * the current delay from first synchronization request
 */
typedef struct status_sync_s status_sync_t;



/**
 * It's the struct used as argument to every created thread
 */
struct thread_status_s
{
    /**
     * This id is not the system thread id, but an internal identificator
     * in the server scope (0 = listeners thread, 1..N = manager threads)
     */
    int id;
    /**
     * This is the system identificator of the thread
     */
    pthread_t tid;
    /**
     * This pointer is used as a reference to the array of pipes must be used
     * for communication
     */
    struct thread_pipe_array_s *tpa;
    /**
     * Size of polling array. Note: some slots may be unused because array
     * reclamation does not happen every time a client disconnects
     */
    nfds_t poll_size;
    /**
     * Elements used for polling file descriptors. Note: some slots may be
     * unused because array reclamation does not happen every time a client
     * disconnects
     */
    struct pollfd *poll_array;
    /**
     * Number of clients active for this manager
     * Note: MUST BE active_clients < poll_size because poll array contains
     *       at least che control pipe
     */
    size_t active_clients;
    /**
     * Array of active and not active clients
     */
    struct server_client_status_s *client_array;
    /**
     * Minimum number of microseconds should elapse between two successive
     * synchronizations of the state file
     * (copied from @ref server_config_s)
     */
    long min_elapsed_sync_time;
    /**
     * Maximum number of microseconds should not be exceeded between two
     * successive synchronizations of the state file
     * (copied from @ref server_config_s)
     */
    long max_elapsed_sync_time;
    /**
     * How much and when state synchronization was asked
     */
    status_sync_t status_sync;
    /**
     * Filename of the first status file
     */
    gchar *status1_filename;
    /**
     * Filename of the second status file
     */
    gchar *status2_filename;
    /**
     * First instance of memory mapped file accessed as an array
     */
    status_record_t *status1;
    /**
     * Second instance of memory mapped file accessed as an array
     */
    status_record_t *status2;
    /**
     * Current status file: it MUST be one of the previous two (@ref status1,
     * @ref status2)
     */
    status_record_t *curr_status;
    /**
     * A (sorted) tree containing all the updated records: these are the
     * records must be copied from one status file to the other one
     */
    GTree           *updated_records;
    /**
     * The number of the updated records; the field avoids the usage of
     * g_tree_nnode that requires complete tree traversal
     */
    int              number_of_updated_records;
    /**
     * Object used to manage the state and its persistence on the disks
     */
    lixa_state_t     state;
    /**
     * Reference to the GLOBAL recovery table: this data structure is
     * internally protected by a mutex to allow concurrency
     */
    srvr_rcvr_tbl_t *recovery_table;
    /**
     * @brief Reference to the GLOBAL transaction table
     */
    server_trans_tbl_t *trans_table;
    /**
     * Maintenance mode (boolean value): only privileged clients can connect
     * to the server; this status is used to perform special maintenance
     * activities
     */
    int mmode;
    /**
     * Exception reported by the thread (after exit)
     */
    int excp;
    /**
     * Return code reported by the thread (after exit)
     */
    int ret_cod;
    /**
     * Errno reported by the thread (after exit)
     */
    int last_errno;
    /**
     * Type of shutdown must be performed
     */
    enum shutdown_type_e shutdown_type;
#ifdef _CRASH
    /**
     * Counter used for crash simulation feature
     */
    long                          *crash_count;
#endif
};



/**
 * It contains the argument passed to all the thread
 */
struct thread_status_array_s
{
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct thread_status_s *array;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    
    /**
     * Initialize a struct of type @ref thread_status_switch_s
     * @param[in,out] tss reference to the struct must be initialized
     */
    inline static void thread_status_switch_init(
        struct thread_status_switch_s *tss)
    {
        tss->id = 0;
        tss->buffer_size = 0;
        tss->buffer = NULL;
    }


    
    /**
     * Initialize a struct of type @ref server_client_status_s
     * @param[in,out] scs reference to the struct must be initialized
     */
    void server_client_status_init(struct server_client_status_s *scs);

    

    /**
     * This is a callback function used for traversing and synchronizing all
     * the records stored in the tree structure
     * @param[in] key of the traversed node, it is casted to
     *               uintptr_t because it's the index inside a status record
     *               array
     * @param[in] value UNUSED
     * @param[in,out] data reference to the status record array (@ref
     *                    status_record_t)
     * @return TRUE (!LIXA_RC_OK) only in an error happens
     */
    gboolean traverse_and_sync(gpointer key, gpointer value, gpointer data);


    
    /**
     * This is a callback function used for traversing and copying blocks
     * stored in the tree structure
     * @param[in] key of the traversed node, it is casted to
     *               uintptr_t because it's the index inside a status record
     *               array
     * @param[in] value UNUSED
     * @param[in] data reference to the special struct
     *                (@ref two_status_record_s) used to store the pointers to
     *                two memory mapped files
     * @return FALSE
     */
    gboolean traverse_and_copy(gpointer key, gpointer value, gpointer data);


    
    /**
     * Initialize a block as an header (first block of a chain)
     * @param[in] srd reference to the record must be initialized
     * @param[in] fd file descriptor of the session associated to this header
     *              record
     * @return a standardized return code
     */
    int payload_header_init(struct status_record_data_s *srd, int fd);


    
    /**
     * Store last (verb,step) value inside the payload header; oldest value
     * is erased if necessary
     * @param[in,out] ts reference to thread status
     * @param[in] block_id id of the block that contains the header chain block
     * @param[in] vs reference to a (verb,step) value
     * @return a standardized return code
     */
    int payload_header_store_verb_step(struct thread_status_s *ts,
                                       uint32_t block_id,
                                       const struct lixa_msg_verb_step_s *vs);

    

    /**
     * Release a chain of records allocated inside the status record
     * memory mapped array. It must start from an header initialized with
     * @ref payload_header_init
     * @param[in,out] ts reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param[in] block_id id of the block that contains the header chain block
     * @return a standardized return code
     */
    int payload_chain_release(struct thread_status_s *ts, uint32_t block_id);

    

    /**
     * Allocate a chain of records inside the status record memory mapped
     * array. The allocation is atomically: all or none of the records are
     * allocated at exit
     * @param[in,out] ts reference to thread status
     * @param[in] block_id id of the block that contains the header chain block
     * @param[in] size number of blocks that must be allocated
     * @return a reason code
     */
    int payload_chain_allocate(struct thread_status_s *ts, uint32_t block_id,
                               int size);


    
    /**
     * Load status records from status file
     * @param[out] sr pointer to the mapped file
     * @param[in] status_file the name of the status file to be loaded
     * @param[in] updated_records set of record has been updated since last
     *                           synchronization
     * @param[out] number_of_updated_records is reset to 0 by this function
     * @param[in] readonly boolean value, must the state file be opened in
     *            read-only mode to avoid file updates
     * @return a standardized return code
     */
    int status_record_load(status_record_t **sr,
                           const char *status_file,
                           GTree **updated_records,
                           int *number_of_updated_records,
                           int readonly);



    /**
     * Create and format a state file (status record)
     * @param[in] status_file_name the name of the state file to be created
     * @param[out] fd file descriptor of the (created and) opened file
     * @return a standardized return code
     */
    int status_record_create_file(const char *status_file_name,
                                  int *fd);
    

    
    /**
     * Check status integrity
     * @param[in] sr memory mapped status file
     * @return LIXA_RC_OK if the status file is OK, an error otherwise
     */
    int status_record_check_integrity(status_record_t *sr);

    

    /**
     * Travel and display the free block chain and used block chain.
     * Debugging function, typically not used, but available for future usages
     * @param[in] sr memory mapped status file
     */
    void status_record_display_chains(const status_record_t *sr);


    
    /**
     * Insert a new element in the used slot list
     * @param[in,out] ts reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param[out] slot the index of the found free slot
     * @return a standardized return code
     */
    int status_record_insert(struct thread_status_s *ts,
                             uint32_t *slot);

    

    /**
     * Remove an element from the used slot list
     * @param[in,out] ts reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param[in] slot the index of the slot must be released
     * @return a standardized return code
     *
     */
    int status_record_delete(struct thread_status_s *ts,
                             uint32_t slot);


    
    /**
     * Mark a record for update
     * @param[in,out] sr reference to the record must be marked for update
     * @param[in] index position of the record in the status file (first = 0)
     * @param[in,out] updated_records is the tree containing all the modified
     *                records (blocks) since last synch
     * @param[in,out] number_of_updated_records is equal to the tree size
     */
    void status_record_update(status_record_t *sr,
                              uintptr_t index,
                              GTree *updated_records,
                              int *number_of_updated_records);


    
    /**
     * Prepare a record for synchronization: counter is changed from odd to
     * even, digest is computed
     * @param[in,out] sr reference to the record must be marked for update
     * @return a reason code
     */
    int status_record_sync(status_record_t *sr);


    
    /**
     * Copy status record mapped file from source to target
     * @param[out] dest mapped file that will receive status
     * @param[in] src mapped file that will supply status
     * @param[in] ts reference to thread status
     * @return a reason code
     */
    int status_record_copy(status_record_t *dest, const status_record_t *src,
                           struct thread_status_s *ts);


    
    /**
     * This is a convenience function used as comparison call back function
     * for GTree
     * @param[in] a pointer to first arg
     * @param[in] b pointer to second arg
     * @return a<b => -1, a>b => +1, a=b =>0
     */
    int size_t_compare_func(gconstpointer a, gconstpointer b);

    

    /**
     * Initialize a @ref status_sync_t object
     * @param[out] ssy object to be initialize
     */
    static inline void status_sync_init(status_sync_t *ssy)
    {
        ssy->asked_sync = 0;
    }


    
    /**
     * Ask a synchronization of the state file
     * @param[in,out] ssy object to update
     */
    static inline void status_sync_ask_sync(status_sync_t *ssy)
    {
        if (ssy->asked_sync == 0)
            lixa_timer_start(&ssy->sync_delay);
        ssy->asked_sync++;
        LIXA_TRACE(("status_sync_ask_sync: # of synchronization requested: "
                    "%d\n", ssy->asked_sync));
    }

    

    /**
     * Retrieve the number of sessions that asked state synchronization
     * @param[in] ssy object to query
     * @return the number of sessions that asked state synchronization
     */
    static inline int status_sync_get_asked(const status_sync_t *ssy)
    {
        return ssy->asked_sync;
    }

    

    /**
     * Retrieve the current delay between first asked synchronization and now
     * @param[in,out] ssy object to query and update
     * @return the delay expressed in microseconds
     */
    static inline long status_sync_get_sync_delay(status_sync_t *ssy)
    {
        if (ssy->asked_sync == 0)
            return 0;
        lixa_timer_stop(&ssy->sync_delay);
        return lixa_timer_get_diff(&ssy->sync_delay);
    }


    
#ifdef _CRASH
    /**
     * Get a writable reference to crash_count property
     * @param[in] ts struct reference
     * @return a writable reference to the number of times the crash point
     * was traversed
     */
    static inline long *thread_status_get_crash_count(
        struct thread_status_s *ts) {
        return ts->crash_count;
    }
#endif /* _CRASH */



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif /* SERVER_STATUS_H */
