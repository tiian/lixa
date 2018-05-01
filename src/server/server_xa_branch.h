/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_XA_BRANCH_H
# define SERVER_XA_BRANCH_H



#include "config.h"



#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif



#include "lixa_errors.h"
#include "server_status.h"
#include "server_trans_tbl.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_XA



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    
    /**
     * Add a new branch to the chain of the already existent branches in the
     * same global transaction
     * @param[in,out] ts reference to thread status
     * @param[in] block_id of the new branch
     * @param[in] array with the existent branches
     * @return a reason code
     */
    int server_xa_branch_chain(struct thread_status_s *ts,
                               uint32_t block_id,
                               server_trans_tbl_qry_arr_t *array);
    


    /**
     * Remove a branch form the chain of the already existent branches in the
     * same global transaction
     * @param[in,out] ts reference to thread status
     * @param[in] block_id of the branch that must be unchained
     * @return a reason code
     */
    int server_xa_branch_unchain(struct thread_status_s *ts,
                                 uint32_t block_id);
    


    /**
     * Check if a block_id is part of a chain of branches
     * @param[in] ts reference to thread status
     * @param[in] block_id of the branch
     * @return a boolean value
     */
    static inline int server_xa_branch_is_chained(
        const struct thread_status_s *ts, uint32_t block_id) {
        return
            0 != ts->curr_status[block_id].sr.data.pld.ph.next_branch_block ||
            0 != ts->curr_status[block_id].sr.data.pld.ph.prev_branch_block;
    }

    

    
    /**
     * Check if a block_id is related to a branch that wants replies for events
     * happened in other branches
     * @param[in] ts reference to thread status
     * @param[in] block_id of the branch
     * @return a boolean value
     */
    int server_xa_branch_want_replies(const struct thread_status_s *ts,
                                      uint32_t block_id);
    

    
    /**
     * Retrieve all the block_id(s) related to all the branches chained in the
     * same multiple branch global transaction
     * @param[in] ts reference to thread status
     * @param[in] block_id of the current branch
     * @param[out] number of found items
     * @param[out] items contains a C dynamically allocated array of number
     *             elements; the caller MUST free the array using "free"
     *             standard C function
     * @return a reason code
     */
    int server_xa_branch_list(const struct thread_status_s *ts,
                              uint32_t block_id,
                              uint32_t *number, uint32_t **items);

    
    
    /**
     * Asses the state of all the branches that are participating in the
     * global transaction
     * @param[in,out] ts reference to the current thread status
     * @param[in] block_id of the current branch
     * @param[in] branch_array_size is the number of branches that participate
     *            in the global transaction
     * @param[in] branch_array is the set of branches that participate in the
     *            global transaction (it contains the block ids of the headers
     *            in current state file)
     * @param[in] branch_join is the state of the branch from the client
     *            session perspective
     * @return a reason code
     */
    int server_xa_branch_prepare(struct thread_status_s *ts,
                                 uint32_t block_id,
                                 uint32_t branch_array_size,
                                 const uint32_t *branch_array,
                                 enum server_client_branch_join_e branch_join);



    /**
     * Check the current state of all the branches that have participated in
     * the global transaction and determine what type of global recovery
     * action would be performed during automatic recovery. This function must
     * be called when the global transaction is not able to complete all the
     * branches.
     * @param[in] ts reference to the current thread status
     * @param[in] branch_array_size is the number of branches that participate
     *            in the global transaction
     * @param[in] branch_array is the set of branches that participate in the
     *            global transaction (it contains the block ids of the header
     *            record of every branch)
     * @param[out] global_recovery is the action that would be performed
     *             by client during automatic recovery
     * @return a reason code
     */
    int server_xa_branch_check_recovery(const struct thread_status_s *ts,
                                        uint32_t branch_array_size,
                                        const uint32_t *branch_array,
                                        int *global_recovery);




    /**
     * During state server restart, some branches could be in inconsistent
     * states due to a journaling fault and/or a states server crash; this
     * step guarantees all the branches of the same global transaction will be
     * rolled back or committed
     * @param[in,out] ts reference to the current thread status
     * @param[in] srt reference to server recovery table
     * @return a reason code
     */
    int server_xa_branch_restart_fix(struct thread_status_s *ts,
                                     const srvr_rcvr_tbl_t *srt);



    
    /**
     * Analize all the client sessions that are participating in a branch
     * and adjust the branch_join flag as necessary
     * @param[in,out] ts reference to thread status
     * @param[in] number of items in the array
     * @param[in] items is an array of number elements, every element is a
     *             "slot_id" associated to a client session
     * @return a reason code
     */
    int server_client_branch_join_adjust(struct thread_status_s *ts,
                                         size_t number, const size_t *items);
                                    
    
    
    /**
     * Retrieve all the slot_id(s) (clients) related to all the branches
     * chained in the same multiple branch global transaction of the client
     * specified by slot_id
     * @param[in] ts reference to thread status
     * @param[in] slot_id of the client that's looking for the other chained
     *            clients
     * @param[out] number of found items
     * @param[out] items contains a C dynamically allocated array of number
     *             elements; the caller MUST free the array using "free"
     *             standard C function
     * @return a reason code
     */
    int server_client_branch_join_list(const struct thread_status_s *ts,
                                       size_t slot_id,
                                       size_t *number, size_t **items);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif /* SERVER_XA_BRANCH_H */
