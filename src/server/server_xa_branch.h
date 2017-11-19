/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
     * Retrieve all the block_id related to all the branches chained in the
     * same multiple branches global transaction
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
