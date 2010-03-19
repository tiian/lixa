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
#include <config.h>



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_recovery.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_RECOVERY



int server_recovery(struct thread_status_s *ts,
                    const struct lixa_msg_s *lmi,
                    struct lixa_msg_s *lmo,
                    uint32_t block_id)
{
    enum Exception { PROTOCOL_ERROR
                     , JOB_SET_RAW_ERROR
                     , SERVER_RECOVERY_RESULT_ERROR
                     , GET_BLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_recovery\n"));
    TRY {
        struct srvr_rcvr_tbl_rec_s query, result;
        lixa_job_t query_job, result_job;

        if (LIXA_MSG_VERB_QRCVR != lmi->header.pvs.verb)
            THROW(PROTOCOL_ERROR);

        /* prepare the query object */
        if (LIXA_RC_OK != (ret_cod = lixa_job_set_raw(
                               &query_job,
                               (const char *)lmi->body.open_8.client.job)))
            THROW(JOB_SET_RAW_ERROR);
        query.job = &query_job;
        query.tsid = ts->id;
        result.job = &result_job; /* reserve room for job object */

        /* query the recovery table */
        ret_cod = srvr_rcvr_tbl_get_block(
            ts->recovery_table, &query, &result, FALSE);
        switch (ret_cod) {
            case LIXA_RC_OK:
                if (LIXA_RC_OK != (ret_cod = server_recovery_result(
                                       ts, &result, lmi, lmo)))
                    THROW(SERVER_RECOVERY_RESULT_ERROR);
                break;
            case LIXA_RC_BYPASSED_OPERATION:
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* nothing to do */
                break;
            default:
                THROW(GET_BLOCK_ERROR);
        } /* switch (rc) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case JOB_SET_RAW_ERROR:
                break;
            case SERVER_RECOVERY_RESULT_ERROR:
            case GET_BLOCK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_recovery_result(struct thread_status_s *ts,
                           const struct srvr_rcvr_tbl_rec_s *record,
                           const struct lixa_msg_s *lmi,
                           struct lixa_msg_s *lmo)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_recovery_result\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_recovery_result/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

