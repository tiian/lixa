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
#ifndef SERVER_THREAD_STATUS_H
# define SERVER_THREAD_STATUS_H



#include <config.h>



#include <server_status.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Used to codify how to dump the status file
 */
struct ts_dump_spec_s {
    /**
     * Boolean: TRUE, perform dump; FALSE, do not perform dump
     */
    int        dump;
    /**
     * Boolean: TRUE, dump all the record in physical order
     */
    int        seq;
    /**
     * Boolean: TRUE, dump free blocks following free chains
     */
    int        free;
    /**
     * Boolean: TRUE, dump used blocks following used chains
     */
    int        used;
};



/**
 * Used to codify how to recover the status file at startup
 */
struct ts_recovery_spec_s {
    /**
     * Remove recovery failed transactions from status file
     */
    int        clean_failed;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a structure of type @ref thread_status_s
     * @param ts OUT reference to the structure must be initialized
     * @param id IN thread id must assigned
     * @param tpa IN reference to the thread pipe array
     * @param mmode IN the thread must operate in maintenance mode
     */
    void thread_status_init(struct thread_status_s *ts, int id,
                            struct thread_pipe_array_s *tpa, int mmode);



    /**
     * Release the memory allocated to a thread status object
     * @param ts OUT reference to the structure must be destroyed
     */
    void thread_status_destroy(struct thread_status_s *ts);


    
    /**
     * Dump the content of the thread status files
     * WARNING: THIS FUNCTION IS *** NOT *** THREAD SAFE
     * @param ts IN thread status reference
     * @param tsds IN dump specification
     * @return a reason code
     */
    int thread_status_dump(const struct thread_status_s *ts,
                           const struct ts_dump_spec_s *tsds);



    /**
     * Dump a transaction header record; see @ref thread_status_dump
     * WARNING: THIS FUNCTION IS *** NOT *** THREAD SAFE
     * @param ph IN payload header reference
     * @return a reason code
     */
    int thread_status_dump_header(const struct payload_header_s *ph);



    /**
     * Dump a resource manager record; see @ref thread_status_dump
     * @param rm IN resource manager reference
     * @return a reason code
     */
    int thread_status_dump_rsrmgr(const struct payload_rsrmgr_s *rm);

    
    
    /**
     * Load the files associated to memory mapped status
     * @param ts IN/OUT pointer to the thread status structure
     * @param status_file_prefix IN the prefix used for status files
     * @param tsds IN dump specification (load and dump if activated)
     * @return a reason code
     */
    int thread_status_load_files(struct thread_status_s *ts,
                                 const char *status_file_prefix,
                                 const struct ts_dump_spec_s *tsds);

    

    /**
     * Scan the status file and enqueue recovery pending transactions in
     * the server recovery table
     * @param ts IN/OUT thread status reference
     * @param srt IN/OUT server recovery table reference
     * @return a standardized return code
     */
    int thread_status_recovery(struct thread_status_s *ts,
                               srvr_rcvr_tbl_t *srt);



    /**
     * Free a chain of blocks and returns the new next block
     * @param ts IN/OUT thread status reference
     * @return a standardized reason code
     */
    int thread_status_clean_failed(struct thread_status_s *ts);


    
    /**
     * Check a transaction header block and determines if it's in recovery
     * pending state
     * @param data IN transaction header block must be analyzed
     * @param result OUT boolean value: TRUE if the transaction is in recovery
     *        pending result
     * @return a standardized return code
     */
    int thread_status_check_recovery_pending(
        const struct status_record_data_s *data, int *result);


    
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



#endif /* SERVER_THREAD_STATUS_H */
