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
#ifndef SERVER_THREAD_STATUS_H
# define SERVER_THREAD_STATUS_H



#include <config.h>



#include "lixa_state.h"
#include "server_status.h"



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
     * @param[out] ts reference to the structure must be initialized
     * @param[in] id thread id must assigned
     * @param[in] tpa reference to the thread pipe array
     * @param[in] crash_count reference to the crash counter
     * @param mmode IN the thread must operate in maintenance mode
     */
    void thread_status_init(struct thread_status_s *ts, int id,
                            struct thread_pipe_array_s *tpa, int mmode,
                            long *crash_count);



    /**
     * Release the memory allocated to a thread status object
     * @param[out] ts reference to the structure must be destroyed
     */
    void thread_status_destroy(struct thread_status_s *ts);


    
    /**
     * Dump the content of the thread status files
     * WARNING: THIS FUNCTION IS *** NOT *** THREAD SAFE
     * @param[in] ts thread status reference
     * @param[in] tsds dump specification
     * @return a reason code
     */
    int thread_status_dump(const struct thread_status_s *ts,
                           const struct ts_dump_spec_s *tsds);



    /**
     * Dump a transaction header record; see @ref thread_status_dump
     * WARNING: THIS FUNCTION IS *** NOT *** THREAD SAFE
     * @param[in] ph payload header reference
     * @return a reason code
     */
    int thread_status_dump_header(const struct payload_header_s *ph);



    /**
     * Dump a resource manager record; see @ref thread_status_dump
     * @param[in] rm resource manager reference
     * @return a reason code
     */
    int thread_status_dump_rsrmgr(const struct payload_rsrmgr_s *rm);

    
    
    /**
     * Load the files associated to memory mapped status
     * @param[in,out] ts pointer to the thread status structure
     * @param[in] status_file_prefix the prefix used for status files
     * @param[in] tsds dump specification (load and dump if activated)
     * @return a reason code
     */
    int thread_status_load_files(struct thread_status_s *ts,
                                 const char *status_file_prefix,
                                 const struct ts_dump_spec_s *tsds);

    

    /**
     * Scan the status file and enqueue recovery pending transactions in
     * the server recovery table
     * @param[in,out] ts thread status reference
     * @param[in,out] srt server recovery table reference
     * @return a standardized return code
     */
    int thread_status_recovery(struct thread_status_s *ts,
                               srvr_rcvr_tbl_t *srt);



    /**
     * Free a chain of blocks and returns the new next block
     * @param[in,out] ts thread status reference
     * @return a standardized reason code
     */
    int thread_status_clean_failed(struct thread_status_s *ts);


    
    /**
     * Check a transaction header block and determines if it's in recovery
     * pending state
     * @param[in] ts thread status reference
     * @param[in] data transaction header block must be analyzed
     * @param[out] branch: TRUE if the branch is in recovery pending state
     * @param[out] global: TRUE if all the branches of the global transaction
     *             are in recovery pending state
     * @return a standardized return code
     */
    int thread_status_check_recovery_pending(
        const struct thread_status_s *ts,
        const struct status_record_data_s *data,
        int *branch, int *global);



    /**
     * Set the global recovery pending state for a record
     * @param[in,out] ts thread status reference
     * @param[in] block_id of the record that must be updated
     * @param[in] global_recovery value that must be set
     * @return a standardized return code
     */
    int thread_status_set_global_recovery(struct thread_status_s *ts,
                                          uint32_t block_id,
                                          int global_recovery);

    
    
    /**
     * Synchronize status files: this is the atomic operation necessary to
     * guarantee transactionality property of the system
     * @param[in] ts thread status reference
     * @return a reason code
     */
    int thread_status_sync_files(struct thread_status_s *ts);


    
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
