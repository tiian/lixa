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
#ifndef SERVER_STATUS_H
#define SERVER_STATUS_H



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
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



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
 * Number of bytes necessary to store an MD5 digest
 */
#define MD5_DIGEST_LENGTH   16



/**
 * This type is used to store an MD5 digest inside a status file record
 */
typedef guint8 md5_digest_t[MD5_DIGEST_LENGTH];



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
    uint32_t pers_status_slot_id;
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
    /**
     * Status file name; this is a reference to the string allocated in
     * configuration struct. This reference must be updated every time the
     * server boots up
     */
};



/**
 * This struct is used as header to concatenate other records; the header
 * must be defined for every client session. Different type of records are
 * not mandatory
 */
struct payload_header {
    /**
     * Number of elements in the block array
     */
    int                n;
    /**
     * Array of "pointers" to the blocks of this logical concatenation
     */
    uint32_t           block_array[CHAIN_MAX_SIZE];
    /**
     * Timestamp of the client's arrival time
     */
    struct timeval     arrival_time;
    /**
     * TCP/IP coordinates of the local socket
     */
    struct sockaddr_in local_sock_addr;
    /**
     * TCP/IP coordinates of the peer socket
     */
    struct sockaddr_in peer_sock_addr;
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
        struct payload_header ph;
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
     * This field contains usefull control or data information
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
     * Initialize a block as an header (first block of a chain)
     * @param srd IN reference to the record must be initialized
     * @param fd IN file descriptor of the session associated to this header
     *              record
     * @return a standardized return code
     */
    int payload_header_init(struct status_record_data_s *srd, int fd);
    


    /**
     * Release a chain of records allocated inside the status record
     * memory mapped array. It must start from an header initialized with
     * @ref payload_header_init
     * @param ts IN/OUT a reference to thread status: it's used to retrieve the
     *                  status files and change them when a dynamic resize is
     *                  necessary
     * @param slot IN index of the header chain block
     * @return a standardized return code
     */
    int payload_chain_release(struct thread_status_s *ts, uint32_t slot);
    


    /**
     * Load status records from status file
     * @param sr OUT pointer to the mapped file
     * @param status_file IN the name of the status file to be loaded
     * @param updated_records IN set of record has been updated since last
     *                           synchronization
     * @return a standardized return code
     */
    int status_record_load(status_record_t **sr,
                           const char *status_file,
                           GTree *updated_records);



    /**
     * Check status integrity
     * @param sr IN memory mapped status file
     * @return LIXA_RC_OK if the status file is OK, an error otherwise
     */
    int status_record_check_integrity(status_record_t *sr);


    
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
        if (!(sr->counter%2)) {
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



    /**
     * Initialize a structure of type @ref thread_status_s
     * @param ts OUT reference to the structure must be initialized
     * @param id IN thread id must assigned
     * @param tpa IN reference to the thread pipe array
     */
    void thread_status_init(struct thread_status_s *ts,
                            int id,
                            struct thread_pipe_array_s *tpa);


    
    /**
     * Load the files associated to memory mapped status
     * @param ts IN/OUT pointer to the thread status structure
     * @param status_file_prefix IN the prefix used for status files
     * @return a reason code
     */
    int thread_status_load_files(struct thread_status_s *ts,
                                 const char *status_file_prefix);

    

    /**
     * Synchronize status files: this is the atomic operation necessary to
     * guarantee transactionality property of the system
     * @param ts IN thread status reference
     * @return a reason code
     */
    int thread_status_sync_files(struct thread_status_s *ts);


    
    /**
     * Remove all records from an updated records tree
     * @param ur IN/OUT the reference to updated records GTree structure
     */
    static inline void thread_status_updated_records_clean(GTree *ur) {
        LIXA_TRACE(("thread_status_updated_records_clean: cleaning "
                    "tree allocated at %p\n", ur));
        g_tree_foreach(ur, traverse_and_delete, ur);
    }



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
