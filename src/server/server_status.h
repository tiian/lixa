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
#ifndef SERVER_STATUS_H
# define SERVER_STATUS_H



#include <config.h>



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



#include <lixa_common_status.h>
#include <lixa_config.h>
#include <lixa_xml_msg.h>
#include <xa.h>
#include <srvr_rcvr_tbl.h>



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
 * Maximum number of blocks in a chain used by a session
 */
#define CHAIN_MAX_SIZE 20

/**
 * The data record is an header
 */
#define DATA_PAYLOAD_TYPE_HEADER  1
/**
 * The data record is a resource manager block
 */
#define DATA_PAYLOAD_TYPE_RSRMGR  2

/**
 * Maximum lenght (null char terminator included) of resource manager name
 */
#define PAYLOAD_RSRMGR_NAME_MAX   30

/**
 * Number of (verb,step) values are stored by the server
 */
#define PAYLOAD_HEADER_VERB_STEP  5



/**
 * It contains the configuration common to any thread
 */
struct thread_pipe_s {
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
struct thread_pipe_array_s {
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
 * It's the struct used to keep the status of a client
 */
struct server_client_status_s {
    /**
     * Place inside the persistent status (memory mapped records) used to store
     * the status of this client
     */
    uint32_t                      pers_status_slot_id;
    /**
     * Buffer must be sent to the client: if NULL, no buffer needs to be sent
     */ 
    char                         *output_buffer;
    /**
     * Size of the buffer must be sent to the client; if @ref output_buffer is
     * NULL, this field is meaningless
     */
    size_t                        output_buffer_size;
    /**
     * (verb,step) related to the output_buffer
     */
    struct lixa_msg_verb_step_s   last_verb_step;
    /**
     * Boolean value: is the client sending the first message?
     */
    int                           first_message;
};



/**
 * This is the control record type and is used for first record only
 */
struct status_record_ctrl_s {
    /**
     * It's the signature of the first 4 bytes and can be used
     * by "file" utility to discover a file is a lixa status file
     */
    uint32_t        magic_number;
    /**
     * Used to distinguish status files produced and used by different
     * versions of the software
     */
    uint32_t        level;
    /**
     * Timestamp (seconds and microseconds) of the last sync operation
     */
    struct timeval  last_sync;
    /**
     * The total number of blocks kept by the status file. The control block
     * itself (block_id = 0) is computed.
     */
    uint32_t        number_of_blocks;
    /**
     * First record of the used blocks chain (0 means the chain is empty)
     */
    uint32_t        first_used_block;
    /**
     * First record of the free blocks chain (0 means the chain is empty)
     */
    uint32_t        first_free_block;
};



/**
 * This struct is used as header to concatenate other records; the header
 * must be defined for every client session. Different type of records are
 * not mandatory
 */
struct payload_header_s {
    /**
     * Number of elements in the block array
     */
    int                           n;
    /**
     * Array of "pointers" to the blocks of this logical concatenation
     */
    uint32_t                      block_array[CHAIN_MAX_SIZE];
    /**
     * Timestamp of the client's arrival time
     */
    struct timeval                arrival_time;
    /**
     * TCP/IP coordinates of the local socket
     */
    struct sockaddr_in            local_sock_addr;
    /**
     * TCP/IP coordinates of the peer socket
     */
    struct sockaddr_in            peer_sock_addr;
    /**
     * Hex format of the MD5 digest of lixac_conf file & profile
     */
    md5_digest_hex_t              config_digest;
    /**
     * Logical JOB associated to the transaction
     */
    lixa_job_t                    job;
    /**
     * Sequence of last (verb,step) stored for the client.
     * This array is used as a circular buffer and position 0 contains ever
     * the last (verb,step)
     */
    struct lixa_msg_verb_step_s   last_verb_step[PAYLOAD_HEADER_VERB_STEP];
    /**
     * Status of the control thread is managing the transaction
     */
    struct common_status_conthr_s state;
};

    

/**
 * This struct is the used to store the status of a resource manager
 */
struct payload_rsrmgr_s {
    /**
     * Resource Manager ID
     */
    int                             rmid;
    /**
     * Resource manager status
     */
    struct common_status_rsrmgr_s   state;
    /**
     * Name of the resource manager as defined in lixac_conf.xml
     */
    char                            name[PAYLOAD_RSRMGR_NAME_MAX];
    /**
     * Name of the resource manager as returned by XA switch data structure
     */
    char                            xa_name[RMNAMESZ];
    /**
     * xa_info string configured for xa_open
     */
    char                            xa_open_info[MAXINFOSIZE];
    /**
     * flags value as passed to xa_open
     */
    long                            xa_open_flags;
    /**
     * rc value as returned from xa_open
     */
    int                             xa_open_rc;
    /**
     * flags value as passed to xa_start
     */
    long                            xa_start_flags;
    /**
     * rc value as returned from xa_start
     */
    int                             xa_start_rc;
    /**
     * flags value as passed to xa_end
     */
    long                            xa_end_flags;
    /**
     * rc value as returned from xa_end
     */
    int                             xa_end_rc;
    /**
     * flags value as passed to xa_prepare
     */
    long                            xa_prepare_flags;
    /**
     * rc value as returned from xa_prepare
     */
    int                             xa_prepare_rc;
    /**
     * flags value as passed to xa_commit
     */
    long                            xa_commit_flags;
    /**
     * rc value as returned from xa_commit
     */
    int                             xa_commit_rc;
    /**
     * flags value as passed to xa_rollback
     */
    long                            xa_rollback_flags;
    /**
     * rc value as returned from xa_rollback
     */
    int                             xa_rollback_rc;
};


    
/**
 * This struct is used to separate scaffolding from payload in data records;
 * all the payload data are kept by this structure
 */
struct status_record_data_payload_s {
    /**
     * Type of payload of this record
     */
    int type;
    /**
     * The data stored by the records depend from the value of the field type
     */
    union {
        /**
         * Payload used for header
         */
        struct payload_header_s ph;
        /**
         * Payload used for resource manager
         */
        struct payload_rsrmgr_s rm;
    };
};



/**
 * This is the data record type and is used for all but first records
 */
struct status_record_data_s {
    /**
     * Index of the first record in the chain (0 means the record is the last
     * in the chain)
     */
    uint32_t next_block;
    /**
     * It contains the interesting data, the persistent status of the client
     */
    struct status_record_data_payload_s pld;
};



/**
 * This union is used to store and retrieve a record from the status file
 */
union status_record_u {
    /**
     * This type of record applies to first record only: it's the control
     * block
     */
    struct status_record_ctrl_s ctrl;
    /**
     * This type of record applies to all but first record: it's the data block
     */
    struct status_record_data_s data;
};



/**
 * This struct is the basic type of a status record: it contains two control
 * fields and one payload field.
 * THE POSITION OF THE FIELDS MUST NOT BE CHANGED INSIDE THE RECORD 
 */
struct status_record_s {
    /**
     * This field is incremented twice between a couple of sync:
     * if the value is even, the record is synched
     * if the value is odd, the record is modified
     * This is the lifetime:
     * - increment once for first update after synch
     * - increment before compute the record digest and successive synch
     */
    uint32_t               counter;
    /**
     * This field contains useful control or data information
     */
    union status_record_u  sr;
    /**
     * This field contains the digest of the previous fields
     */
    md5_digest_t           digest;
};



/**
 * Number of bytes must be signed to check integrity
 */
#define STATUS_RECORD_CHECKSUM_SIZE (sizeof(status_record_t) -  \
                                     sizeof(md5_digest_t))



/**
 * It's defined as a type because it's used in an object oriented fashion
 */
typedef struct status_record_s status_record_t;



/**
 * It is used to code two ordered references to two different memory mapped
 * status files; for example, @ref traverse_and_copy uses this struct as
 * data argument to copy from first to second status file
 */
struct two_status_record_s {
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
 * It's the struct used as argument to every created thread
 */
struct thread_status_s {
    /**
     * This id is not the system thread id, but an internal identificator
     * in the server scope (0 = listeners thread, 1..N = manager threads)
     */
    int                            id;
    /**
     * This is the system identificator of the thread
     */
    pthread_t                      tid;
    /**
     * This pointer is used as a reference to the array of pipes must be used
     * for communication
     */
    struct thread_pipe_array_s    *tpa;
    /**
     * Size of polling array. Note: some slots may be unused because array
     * reclamation does not happen every time a client disconnects
     */
    nfds_t                         poll_size;
    /**
     * Elements used for polling file descriptors. Note: some slots may be
     * unused because array reclamation does not happen every time a client
     * disconnects
     */
    struct pollfd                 *poll_array;
    /**
     * Number of clients active for this manager
     * Note: MUST BE active_clients < poll_size because poll array contains
     *       at least che control pipe
     */
    size_t                         active_clients;
    /**
     * Array of active and not active clients
     */
    struct server_client_status_s *client_array;
    /**
     * One or more sessions asked a file status synchronization
     */
    int                            asked_sync;
    /**
     * Filename of the first status file
     */
    gchar                         *status1_filename;
    /**
     * Filename of the second status file
     */
    gchar                         *status2_filename;
    /**
     * First instance of memory mapped file accessed as an array
     */
    status_record_t               *status1;
    /**
     * Second instance of memory mapped file accessed as an array
     */
    status_record_t               *status2;
    /**
     * Current status file: it MUST be one of the previous two (@ref status1,
     * @ref status2)
     */
    status_record_t               *curr_status;
    /**
     * A (sorted) tree containing all the updated records: these are the
     * records must be copied from one status file to the other one
     */
    GTree                         *updated_records;
    /**
     * Reference to the GLOBAL recovery table: this data structure is internally
     * protected by a mutex to allow concurrency
     */
    srvr_rcvr_tbl_t               *recovery_table;
    /**
     * Exception reported by the thread (after exit)
     */
    int                            excp;
    /**
     * Return code reported by the thread (after exit)
     */
    int                            ret_cod;
    /**
     * Errno reported by the thread (after exit)
     */
    int                            last_errno;
};



/**
 * It contains the argument passed to all the thread
 */
struct thread_status_array_s {
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
     * Initialize a struct of type @ref server_client_status_s
     * @param scs IN/OUT reference to the struct must be initialized
     */
    inline static void server_client_status_init(
        struct server_client_status_s *scs) {
        scs->output_buffer = NULL;
        scs->output_buffer_size = 0;
        scs->last_verb_step.verb = 0;
        scs->last_verb_step.step = 0;
        scs->first_message = TRUE;
    }

    

    /**
     * This is a convenience function used as delete call back function for
     * GTree
     * @param key IN the key of the traversed node, it's the key must be
     *               deleted from GTree
     * @param value IN useless for updated_records use case
     * @param data IN references the GTree object itself
     * @return FALSE because TRUE would break tree traversal
     */
    gboolean traverse_and_delete(gpointer key, gpointer value, gpointer data);

    

    /**
     * This is a callback function used for traversing and synchronizing all
     * the records stored in the tree structure
     * @param key IN the key of the traversed node, it is casted to
     *               uintptr_t because it's the index inside a status record
     *               array
     * @param value IN unused
     * @param data IN/OUT reference to the status record array (@ref
     *                    status_record_t)
     * @return TRUE (!LIXA_RC_OK) only in an error happens
     */
    gboolean traverse_and_sync(gpointer key, gpointer value, gpointer data);



    /**
     * This is a callback function used for traversing and copying blocks
     * stored in the tree structure
     * @param key IN the key of the traversed node, it is casted to
     *               uintptr_t because it's the index inside a status record
     *               array
     * @param value IN unused
     * @param data IN reference to the special struct
     *                (@ref two_status_record_s) used to store the pointers to
     *                two memory mapped files
     * @return FALSE
     */
    gboolean traverse_and_copy(gpointer key, gpointer value, gpointer data);


        
    /**
     * Initialize a block as an header (first block of a chain)
     * @param srd IN reference to the record must be initialized
     * @param fd IN file descriptor of the session associated to this header
     *              record
     * @return a standardized return code
     */
    int payload_header_init(struct status_record_data_s *srd, int fd);
    


    /**
     * Store last (verb,step) value inside the payload header; oldest value
     * is erased if necessary
     * @param ts IN/OUT a reference to thread status
     * @param block_id IN id of the block contains the header chain block
     * @param vs IN reference to a (verb,step) value
     * @return a standardized return code
     */
    int payload_header_store_verb_step(struct thread_status_s *ts,
                                       uint32_t block_id,
                                       const struct lixa_msg_verb_step_s *vs);

    
    
    /**
     * Release a chain of records allocated inside the status record
     * memory mapped array. It must start from an header initialized with
     * @ref payload_header_init
     * @param ts IN/OUT a reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param block_id IN id of the block contains the header chain block
     * @return a standardized return code
     */
    int payload_chain_release(struct thread_status_s *ts, uint32_t block_id);
    



    /**
     * Allocate a chain of records inside the status record memory mapped
     * array. The allocation is atomically: all or none of the records are
     * allocated at exit
     * @param ts IN/OUT reference to thread status
     * @param block_id IN id of the block contains the header chain block
     * @param size IN number of blocks must be allocated
     * @return a reason code
     */
    int payload_chain_allocate(struct thread_status_s *ts, uint32_t block_id,
                               int size);
    
    
    
    /**
     * Load status records from status file
     * @param sr OUT pointer to the mapped file
     * @param status_file IN the name of the status file to be loaded
     * @param updated_records IN set of record has been updated since last
     *                           synchronization
     * @param readonly IN the status file must be opened in read-only mode to
     *                    avoid updates to the file
     * @return a standardized return code
     */
    int status_record_load(status_record_t **sr,
                           const char *status_file,
                           GTree *updated_records,
                           int readonly);



    /**
     * Check status integrity
     * @param sr IN memory mapped status file
     * @return LIXA_RC_OK if the status file is OK, an error otherwise
     */
    int status_record_check_integrity(status_record_t *sr);



    /**
     * Travel and display the free block chain and used block chain
     * @param sr IN memory mapped status file
     */
    void status_record_display_chains(const status_record_t *sr);


    
    /**
     * Insert a new element in the used slot list
     * @param ts IN/OUT a reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param slot OUT the index of the found free slot
     * @return a standardized return code
     */
    int status_record_insert(struct thread_status_s *ts,
                             uint32_t *slot);



    /**
     * Remove an element from the used slot list
     * @param ts IN/OUT a reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param slot IN the index of the slot must be released
     * @return a standardized return code
     *
     */
    int status_record_delete(struct thread_status_s *ts,
                             uint32_t slot);



    /**
     * Mark a record for update
     * @param sr IN/OUT reference to the record must be marked for update
     * @param index IN position of the record in the status file (first = 0)
     * @param updated_records IN/OUT the tree containing all the modified
     *                               records (blocks) since last synch
     */
    static inline void status_record_update(status_record_t *sr,
                                            uintptr_t index,
                                            GTree *updated_records) {
        if (!(sr->counter & 0x1)) {
            sr->counter++;
            g_tree_insert(updated_records, (gpointer)index, NULL);
            LIXA_TRACE(("status_record_update: inserted "
                        "index " UINTPTR_T_FORMAT " (counter="
                        UINT32_T_FORMAT", address=%p) in updated records tree "
                        "(number of nodes now is %d)\n",
                        index, sr->counter, sr,
                        g_tree_nnodes(updated_records)));
        }
    }



    /**
     * Prepare a record for synchronization: counter is changed from odd to
     * even, digest is computed
     * @param sr IN/OUT reference to the record must be marked for update
     * @return a reason code
     */
    int status_record_sync(status_record_t *sr);



    /**
     * Copy status record mapped file from source to target
     * @param dest OUT the mapped file will receive status
     * @param src IN the mapped file will supply status
     * @param ts IN reference to thread status
     * @return a reason code
     */
    int status_record_copy(status_record_t *dest, const status_record_t *src,
                           struct thread_status_s *ts);
                       

    
    /**
     * This is a convenience function used as comparison call back function
     * for GTree
     * @param a IN pointer to first arg
     * @param b IN pointer to second arg
     * @return a<b => -1, a>b => +1, a=b =>0
     */
    int size_t_compare_func(gconstpointer a, gconstpointer b);



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
