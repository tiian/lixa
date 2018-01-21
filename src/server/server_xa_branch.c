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
#include "config.h"



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
                    /* 2018-01-21 Ch.F.
                     * Unfortunately, no idea how to create a case test that
                     * can reproduce this behavior in a consistent manner.
                     * This piece code will remain untested... */
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



int server_xa_branch_unchain(struct thread_status_s *ts,
                             uint32_t block_id)
{
    enum Exception { BYPASSED_OPERATION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_branch_unchain\n"));
    TRY {
        uint32_t next_branch_block =
            ts->curr_status[block_id].sr.data.pld.ph.next_branch_block;
        uint32_t prev_branch_block =
            ts->curr_status[block_id].sr.data.pld.ph.prev_branch_block;
        if (0 == next_branch_block && 0 == prev_branch_block) {
            LIXA_TRACE(("server_xa_branch_unchain: block_id "
                        UINT32_T_FORMAT " is not chained because prev and "
                        "next blocks are 0\n", block_id));
            THROW(BYPASSED_OPERATION);
        } /* if (0 == next_branch_block && 0 == prev_branch_block) */
        if (0 != prev_branch_block) {
            LIXA_TRACE(("server_xa_branch_unchain: unchaining previous "
                        "block " UINT32_T_FORMAT " from this one ("
                        UINT32_T_FORMAT ")\n", prev_branch_block, block_id));
            status_record_update(ts->curr_status + prev_branch_block,
                                 prev_branch_block, ts->updated_records);
            ts->curr_status[prev_branch_block
                            ].sr.data.pld.ph.next_branch_block =
                next_branch_block;
        } /* if (0 != prev_branch_block) */
        if (0 != next_branch_block) {
            LIXA_TRACE(("server_xa_branch_unchain: unchaining next "
                        "block " UINT32_T_FORMAT " from this one ("
                        UINT32_T_FORMAT ")\n", next_branch_block, block_id));
            status_record_update(ts->curr_status + next_branch_block,
                                 next_branch_block, ts->updated_records);
            ts->curr_status[next_branch_block
                            ].sr.data.pld.ph.prev_branch_block =
                prev_branch_block;
        } /* if (0 != prev_branch_block) */
        /* resetting current block_id */
        ts->curr_status[block_id].sr.data.pld.ph.next_branch_block = 0;
        ts->curr_status[block_id].sr.data.pld.ph.prev_branch_block = 0;
        status_record_update(ts->curr_status + block_id,
                             block_id, ts->updated_records);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BYPASSED_OPERATION:
                ret_cod = LIXA_RC_BYPASSED_OPERATION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_branch_unchain/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_xa_branch_want_replies(const struct thread_status_s *ts,
                                  uint32_t block_id)
{
    enum Exception { NOT_CHAINED
                     , NONE } excp;
    int ret_cod = FALSE;
    
    LIXA_TRACE(("server_xa_branch_want_replies\n"));
    TRY {
        const struct lixa_msg_verb_step_s *vs = NULL;
        
        /* check it's part of a branch */
        if (!server_xa_branch_is_chained(ts, block_id))
            THROW(NOT_CHAINED);
        vs = &ts->curr_status[block_id].sr.data.pld.ph.last_verb_step[0];
        LIXA_TRACE(("server_xa_branch_want_replies: block_id=" UINT32_T_FORMAT
                    " ,last_verb=%d, last_step=%d\n", block_id, vs->verb,
                    vs->step));
        /* check verb and step */
        switch (vs->verb) {
            case LIXA_MSG_VERB_OPEN:
                if (LIXA_MSG_STEP_INCR == vs->step)
                    ret_cod = TRUE;
                break;
            case LIXA_MSG_VERB_START:
                if (LIXA_MSG_STEP_INCR == vs->step)
                    ret_cod = TRUE;
                break;
            case LIXA_MSG_VERB_END:
                if (LIXA_MSG_STEP_INCR == vs->step)
                    ret_cod = TRUE;
                break;
            case LIXA_MSG_VERB_PREPARE:
                if (LIXA_MSG_STEP_INCR == vs->step ||
                    3*LIXA_MSG_STEP_INCR == vs->step)
                    ret_cod = TRUE;
                break;
            case LIXA_MSG_VERB_QRCVR:
                if (LIXA_MSG_STEP_INCR == vs->step)
                    ret_cod = TRUE;
                break;
            default:
                break;
        } /* switch (vs->verb) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NOT_CHAINED:
            case NONE:
                break;
            default:
                break;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_branch_want_replies/excp=%d/"
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
                     , PREPARE_DELAYED
                     , MULTIBRANCH_PREPARE_FAILED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_branch_prepare: block_id=" UINT32_T_FORMAT "\n",
                block_id));
    TRY {
        uint32_t i, will_commit=0, will_rollback=0, global_recovery=0,
            unknown=0;
        /* assess all the branches */
        for (i=0; i<branch_array_size; ++i) {
            struct common_status_conthr_s *state =
                &ts->curr_status[branch_array[i]].sr.data.pld.ph.state;
            struct lixa_msg_verb_step_s *verb_step =
                &ts->curr_status[branch_array[i]
                                 ].sr.data.pld.ph.last_verb_step[0];
            /* check the branch reached the prepare verb, but not for
               current branch */
            if (LIXA_MSG_VERB_PREPARE != verb_step->verb &&
                block_id != branch_array[i]) {
                LIXA_TRACE(("server_xa_branch_prepare: i=" UINT32_T_FORMAT ", "
                            "branch_array[i]=" UINT32_T_FORMAT ", "
                            "last_verb=%d, this branch has not "
                            "yet prepared, skipping it\n",
                            i, branch_array[i], verb_step->verb));
                unknown++;
                continue;
            }
            LIXA_TRACE(("server_xa_branch_prepare: i=" UINT32_T_FORMAT ", "
                        "branch_array[i]=" UINT32_T_FORMAT ", last_verb=%d, "
                        "will_commit=%d, will_rollback=%d, "
                        "global_recovery=%d\n",
                        i, branch_array[i], verb_step->verb,
                        state->will_commit, state->will_rollback,
                        state->global_recovery));
            if (state->global_recovery)
                global_recovery++;
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
                    UINT32_T_FORMAT ", #global_recovery=" UINT32_T_FORMAT "\n",
                    will_commit, will_rollback, unknown, global_recovery));
        if (0 < unknown) {
            /* at least one not prepared branch */
            THROW(PREPARE_DELAYED);
        } else if ((0 < will_rollback || 0 < global_recovery) &&
                   0 == unknown) {
            /* at least one failed branch or a global_recovery condition has
               been rised, but all the branch has been prepared */
            THROW(MULTIBRANCH_PREPARE_FAILED);
        } /* if (0 < unknown) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED1:
            case OBJ_CORRUPTED2:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case PREPARE_DELAYED:
                ret_cod = LIXA_RC_OPERATION_POSTPONED;
                break;
            case MULTIBRANCH_PREPARE_FAILED:
                ret_cod = LIXA_RC_MULTIBRANCH_PREPARE_FAILED;
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



int server_xa_branch_check_recovery(const struct thread_status_s *ts,
                                    uint32_t branch_array_size,
                                    const uint32_t *branch_array,
                                    int *global_recovery)
{
    enum Exception { INVALID_HEADER_TYPE
                     , SKIP_ANALYSIS
                     , POSSIBLE_IN_FLIGHT_TX
                     , UNPREPARED_RESOURCE_MANAGERS
                     , AT_LEAST_ONE_ROLLBACK
                     , ALL_THE_BRANCHES_PREPARED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_xa_branch_check_recovery\n"));
    TRY {
        uint32_t i, j;
        const struct status_record_data_s *data = NULL;
        const struct payload_rsrmgr_s *rm = NULL;
        /* a bunch of statistic vars */
        int finished = 0, will_commit = 0, will_rollback = 0;
        int prepared_rm = 0, prepared_branches = 0;
        /* reset the condition */
        *global_recovery = XTA_GLOBAL_RECOV_NULL;
        /* loop on all the branches */
        for (i=0; i<branch_array_size; ++i) {
            data = &(ts->curr_status[branch_array[i]].sr.data);
            /* check the record is an header (consistency check) */
            if (DATA_PAYLOAD_TYPE_HEADER != data->pld.type) {
                LIXA_TRACE(("server_xa_branch_check_recovery: "
                            "data->pld.type=%d\n", data->pld.type));
                THROW(INVALID_HEADER_TYPE);
            }
            /* trace a lot of info */
            LIXA_TRACE(("server_xa_branch_check_recovery: branch="
                        UINT32_T_FORMAT ", block_id=" UINT32_T_FORMAT
                        ", last verb=%d, last step=%d, finished=%d, "
                        "txstate=%d, will_commit=%d, will_rollback=%d, "
                        "global_recovery=%d\n",
                        i, branch_array[i],
                        data->pld.ph.last_verb_step[0].verb,
                        data->pld.ph.last_verb_step[0].step,
                        data->pld.ph.state.finished,
                        data->pld.ph.state.txstate,
                        data->pld.ph.state.will_commit,
                        data->pld.ph.state.will_rollback,
                        data->pld.ph.state.global_recovery));
            /* check if the global_recovery has been already set for this
               branch */
            if (XTA_GLOBAL_RECOV_NULL != data->pld.ph.state.global_recovery) {
                LIXA_TRACE(("server_xa_branch_check_recovery: global_recovery "
                            "is already set for this branch, skipping "
                            "analysis...\n"));
                /* stop here the analysis, it has already been done! */
                THROW(SKIP_ANALYSIS);
            } /* if (XTA_GLOBAL_RECOV_NULL != data->pld.ph.state... */
            /* check the last verb */
            switch (data->pld.ph.last_verb_step[0].verb) {
                case LIXA_MSG_VERB_START:
                case LIXA_MSG_VERB_END:
                    LIXA_TRACE(("server_xa_branch_check_recovery: this branch "
                                "started but didn't prepare, there can be a "
                                "possigle 'in flight' transaction, forcing "
                                "rollback\n"));
                    *global_recovery = XTA_GLOBAL_RECOV_FORCE_ROLLBACK;
                    THROW(POSSIBLE_IN_FLIGHT_TX);
                    break;
                default: /* nothing to do */
                    break;
            } /* switch (data->pld.ph.last_verb_step[0].verb) */
            if (data->pld.ph.state.finished)
                finished++;
            if (data->pld.ph.state.will_commit)
                will_commit++;
            if (data->pld.ph.state.will_rollback)
                will_rollback++;
            /* reset the number of prepared resource managers */
            prepared_rm = 0;
            /* loop on all the resource managers used by the branch */
            for (j=0; j<data->pld.ph.n; ++j) {
                rm = &(ts->curr_status[
                           data->pld.ph.block_array[j]].sr.data.pld.rm);
                LIXA_TRACE(("server_xa_branch_check_recovery: rmid=%d, "
                            "name='%s', xa_s_state=%d\n",
                            rm->rmid, rm->name, rm->state.xa_s_state));
                if (XA_STATE_S3 == rm->state.xa_s_state)
                    prepared_rm++;
            } /* for (j=0; j<data->pld.n; ++j) */
            if (0 < prepared_rm && prepared_rm < data->pld.ph.n) {
                LIXA_TRACE(("server_xa_branch_check_recovery: the number of "
                            "prepared resource managers (%d) is different "
                            "than the total number of resource managers (%d), "
                            "forcing rollback...",
                            prepared_rm, data->pld.ph.n));
                *global_recovery = XTA_GLOBAL_RECOV_FORCE_ROLLBACK;
                THROW(UNPREPARED_RESOURCE_MANAGERS);
            } else if (prepared_rm == data->pld.ph.n) {
                LIXA_TRACE(("server_xa_branch_check_recovery: all the "
                            "resource managers (%d) have been prepared for "
                            "this branch\n", prepared_rm));
                prepared_branches++;
            } /* if (0 < prepared_rm && prepared_rm < data->pld.ph.n) */
        } /* for (i=0; i<branch_array_size; ++i) */
        /* if at least one branch wanted to rollback, forcing rollback... */
        if (0 < will_rollback) {
            LIXA_TRACE(("server_xa_branch_check_recovery: there's at least "
                        "one (%d) branch that asked for rollback, forcing "
                        "rollback...\n", will_rollback));
            *global_recovery = XTA_GLOBAL_RECOV_FORCE_ROLLBACK;
            THROW(AT_LEAST_ONE_ROLLBACK);
        }
        /* if all the branches prepared, force commit... */
        if (prepared_branches == branch_array_size) {
            LIXA_TRACE(("server_xa_branch_check_recovery: all the resource "
                        "managers in all the branches prepared for commit, "
                        "forcing commit...\n"));
            *global_recovery = XTA_GLOBAL_RECOV_FORCE_COMMIT;
            THROW(ALL_THE_BRANCHES_PREPARED);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_HEADER_TYPE:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case SKIP_ANALYSIS:
            case POSSIBLE_IN_FLIGHT_TX:
            case UNPREPARED_RESOURCE_MANAGERS:
            case AT_LEAST_ONE_ROLLBACK:
            case ALL_THE_BRANCHES_PREPARED:
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        LIXA_TRACE(("server_xa_branch_check_recovery: global_recovery=%d\n",
                    *global_recovery));
    } /* TRY-CATCH */
    LIXA_TRACE(("server_xa_branch_check_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

