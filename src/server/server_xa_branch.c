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
#include <config.h>



#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



#include "lixa_syslog.h"
#include "server_xa_branch.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_XA



int server_xa_branch_chain(struct thread_status_s *ts,
                           uint32_t block_id,
                           server_trans_tbl_qry_arr_t *array)
{
    enum Exception { NOT_CHAINABLE_BRANCH
                     , INTERNAL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_branch_chain\n"));
    TRY {
        guint i;
        int chained = FALSE;
        
        /* loop on all the already chained blocks: only one should have a
           null next_branch_block */
        for (i=0; i<array->len; ++i) {
            uint32_t rmid;
            int chainable = TRUE;
            struct server_trans_tbl_qry_s *record =
                &g_array_index(array, struct server_trans_tbl_qry_s, i);
            uint32_t next_branch_block =
                ts->curr_status[record->block_id
                                ].sr.data.pld.ph.next_branch_block;
            uint32_t prev_branch_block =
                ts->curr_status[record->block_id
                                ].sr.data.pld.ph.prev_branch_block;
            LIXA_TRACE(("server_xa_branch_chain: item=%u, block_id=%u, "
                        "next_branch_block=%u, prev_branch_block=%u\n",
                        i, record->block_id, next_branch_block,
                        prev_branch_block));
            /* check if the branch has already called xa_prepare; loop all
               rmids*/
            for (rmid=0; rmid<ts->curr_status[record->block_id
                                              ].sr.data.pld.ph.n; ++rmid) {
                status_record_t *sr;
                uint32_t slot =
                    ts->curr_status[block_id].sr.data.pld.ph.block_array[rmid];
                sr = ts->curr_status + slot;
                if (XA_STATE_S0 != sr->sr.data.pld.rm.state.xa_s_state &&
                    XA_STATE_S1 != sr->sr.data.pld.rm.state.xa_s_state &&
                    XA_STATE_S2 != sr->sr.data.pld.rm.state.xa_s_state) {
                    chainable = FALSE;
                }
                LIXA_TRACE(("server_xa_branch_chain: rmid=" UINT32_T_FORMAT
                            ", xa_s_state=%d, branch is chainable: %d\n",
                            rmid, sr->sr.data.pld.rm.state.xa_s_state,
                            chainable));
            } /* for (rmid=0; ... */

            /* this branch is not chainable to previous ones because it's
             * too late from the XA protocol perspective */
            if (!chainable) {
                lixa_ser_xid_t lsx;
                if (lixa_xid_serialize(
                        &ts->curr_status[block_id].sr.data.pld.ph.state.xid,
                        lsx)) {
                    LIXA_TRACE(("server_xa_branch_chain: client is asking to "
                                "branch an existing global transaction, but "
                                "another branch (xid='%s') of the same global "
                                "transaction has already started the XA "
                                "prepare phase\n", lsx));
                    syslog(LOG_WARNING, LIXA_SYSLOG_LXD032N, lsx);
                } /* if (lixa_xid_serialize(... */
                THROW(NOT_CHAINABLE_BRANCH);
            } /* if (!chainable) */
            
            if (0 == next_branch_block) {
                if (!chained) {
                    /* this is the point to connect to */
                    status_record_update(ts->curr_status + record->block_id,
                                         record->block_id,
                                         ts->updated_records);
                    ts->curr_status[record->block_id
                                    ].sr.data.pld.ph.next_branch_block =
                        block_id;
                    /* this is the block to connect */
                    status_record_update(ts->curr_status + block_id,
                                         block_id, ts->updated_records);
                    ts->curr_status[block_id
                                    ].sr.data.pld.ph.prev_branch_block =
                        record->block_id;
                    chained = TRUE;
                    LIXA_TRACE(("server_xa_branch_chain: chained to "
                                "block_id=%u\n", block_id));
                } else {
                    /* this is an internal error and it should never happen */
                    LIXA_TRACE(("server_xa_branch_chain: two records "
                                "with next_branch_block=0 have been found. "
                                "This should never happen and it's an "
                                "internal error\n"));
                    THROW(INTERNAL_ERROR);
                }
            } /* if (0 == next_branch_block) */
        } /* for (i=0; i<array->len; ++i) */        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NOT_CHAINABLE_BRANCH:
                ret_cod = LIXA_RC_NOT_CHAINABLE_BRANCH;
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_branch_chain/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_xa_branch_list(const struct thread_status_s *ts,
                          uint32_t block_id,
                          uint32_t *number, uint32_t **items)
{
    enum Exception { INVALID_STATUS
                     , MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_branch_list\n"));
    TRY {
        uint32_t first, i, j;
        /* reset output values */
        *number = 0;
        *items = NULL;
        /* check it's really a chain... */
        if (!server_xa_branch_is_chained(ts, block_id))
            THROW(INVALID_STATUS);
        /* move to the first block in the chain */
        first = block_id;
        while (0 != ts->curr_status[first].sr.data.pld.ph.prev_branch_block)
            first = ts->curr_status[first].sr.data.pld.ph.prev_branch_block;
        /* count all the elements */
        i = first;
        while (i != 0) {
            (*number)++;
            i = ts->curr_status[i].sr.data.pld.ph.next_branch_block;
        }
        /* allocate the output array */
        if (NULL == (*items = malloc(*number * sizeof(uint32_t))))
            THROW(MALLOC_ERROR);
        /* fill the output array */
        i = first;
        j = 0;
        while (i != 0) {
            (*items)[j++] = i;
            i = ts->curr_status[i].sr.data.pld.ph.next_branch_block;
        }
        LIXA_TRACE(("server_xa_branch_list: number=%u\n", *number));
        for (j=0; j<*number; ++j)
            LIXA_TRACE(("server_xa_branch_list: *items[%u]=%u\n",
                        j, (*items)[j]));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_branch_list/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_xa_branch_prepare(struct thread_status_s *ts,
                             uint32_t block_id,
                             uint32_t branch_array_size,
                             const uint32_t *branch_array)
{
    enum Exception { OBJ_CORRUPTED1
                     , OBJ_CORRUPTED2
                     , MULTIBRANCH_PREPARE_FAILED
                     , PREPARE_DELAYED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_branch_prepare: block_id=" UINT32_T_FORMAT "\n",
                block_id));
    TRY {
        uint32_t i, will_commit=0, will_rollback=0, unknown=0;
        /* assess all the branches */
        for (i=0; i<branch_array_size; ++i) {
            struct common_status_conthr_s *state =
                &ts->curr_status[branch_array[i]].sr.data.pld.ph.state;
            struct lixa_msg_verb_step_s *verb_step =
                &ts->curr_status[branch_array[i]
                                 ].sr.data.pld.ph.last_verb_step[0];
            LIXA_TRACE(("server_xa_branch_prepare: i=" UINT32_T_FORMAT ", "
                        "branch_array[i]=" UINT32_T_FORMAT ", last_verb=%d, "
                        "will_commit=%d, will_rollback=%d\n",
                        i, branch_array[i], verb_step->verb,
                        state->will_commit, state->will_rollback));
            /* check the branch reached the prepare verb, but not for
               current branch*/
            if (LIXA_MSG_VERB_PREPARE != verb_step->verb &&
                block_id != branch_array[i]) {
                LIXA_TRACE(("server_xa_branch_prepare: this branch has not "
                            "yet prepared, skipping it\n"));
                unknown++;
                continue;
            }
            /* check the prepared state of the branch */
            if (TRUE == state->will_commit) {
                if (FALSE == state->will_rollback)
                    will_commit++;
                else
                    THROW(OBJ_CORRUPTED1);
            } else {
                if (TRUE == state->will_rollback)
                    will_rollback++;
                else
                    THROW(OBJ_CORRUPTED2);
            } /* if (TRUE == state->will_commit) */
        } /* for (i=0; i<branch_array_size; ++i) */
        LIXA_TRACE(("server_xa_branch_prepare: #will_commit=" UINT32_T_FORMAT
                    ", #will_rollback=" UINT32_T_FORMAT ", #unknown="
                    UINT32_T_FORMAT "\n",
                    will_commit, will_rollback, unknown));
        if (0 < will_rollback) {
            THROW(MULTIBRANCH_PREPARE_FAILED);
        } else if (0 < unknown) {
            THROW(PREPARE_DELAYED);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED1:
            case OBJ_CORRUPTED2:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case MULTIBRANCH_PREPARE_FAILED:
                ret_cod = LIXA_RC_MULTIBRANCH_PREPARE_FAILED;
                break;
            case PREPARE_DELAYED:
                ret_cod = LIXA_RC_PREPARE_DELAYED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_branch_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
