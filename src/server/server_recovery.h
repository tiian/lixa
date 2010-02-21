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
#ifndef SERVER_RECOVERY_H
# define SERVER_RECOVERY_H



#include <config.h>



#include <lixa_config.h>



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
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_RECOVERY



/**
 * It should be used to initialize a static @ref srvr_rcvr_tbl_t object
 */
#define SRVR_RCVR_TBL_INIT    { NULL, NULL, 0 }



/**
 * This struct is used as the base record of the more complex
 * @ref srvr_rcvr_tbl_s data structure; the first three fields are the key
 * fields and are used "by reference" because this structure should not be
 * stored and/or persisted: it should only be a message structure
 */
struct srvr_rcvr_tbl_rec_s {
    /**
     * first level key: transactional JOB
     */
    lixa_job_t    *job;
    /**
     * second level key: the LIXA server thread status identificator
     * (see @ref thread_status_array_s )
     */
    guint          tsid;
    /**
     * third level key: the block inside thread status' file
     */
    uint32_t       block_id;
};



/**
 * Server recovery table struct: this object keeps track of the recovery
 * pending information
 */
struct srvr_rcvr_tbl_s {
    /**
     * Mutex used to serialize accesses
     */
    GMutex          *mutex;
    /**
     * First level of the multimensional structure: it's a tree and the key
     * is the "job" string
     */
    GTree           *lvl1_job;
    /**
     * Size of the array of thread status identifiers
     */
    guint            lvl2_tsid_array_size;
};



/**
 * Server recovery table: it's defined as a type because it's used in an
 * object oriented fashion see @ref srvr_rcvr_tbl_s
 */
typedef struct srvr_rcvr_tbl_s srvr_rcvr_tbl_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Comparison function between two struct @see srvr_rcvr_tbl_rec_s
     * @param a IN reference to first recovery table record
     * @param b IN reference to second recovery table record
     * @return negative value if a<b; 0 if a==b; positive value if a>b
     */
    int srvr_rcrv_tbl_key1_job_comp(gconstpointer a, gconstpointer b);


    
    /**
     * Create a new object: initialization must be performed only ONCE
     * @param srt OUT reference to the recovery table object
     * @param tsid_array_size IN size of the array will store thread id
     *        status
     * @return a standardized return code
     */
    int srvr_rcvr_tbl_new(srvr_rcvr_tbl_t *srt, guint tsid_array_size);

    

    /**
     * Insert a record in the recovery table
     * @param srt IN/OUT reference to the recovery table object
     * @param srtr IN reference to the record must be inserted
     * @return a standardized return code
     */
    int srvr_rcvr_tbl_insert(srvr_rcvr_tbl_t *srt,
                             const struct srvr_rcvr_tbl_rec_s *srtr);



    /**
     * Look for server recovery table and extract the first record must be
     * processed from a recovery pending point of view; this function uses a
     * query by example logic
     * @param srt IN/OUT reference to the recovery table object
     * @param srtr IN reference to the search record:
     *             srtr->job = the job is performing recovery;
     *             srtr->tsid = the thread status id is performing recovery;
     *             srtr->block_is unused
     * @param out OUT reference to the output record:
     *             out->job = srtr->job if there is at least one recovery
     *                       pending transaction for this job;
     *             out->job = "" if there are no recovery pending transactions
     *                          for this job;
     *             out->tsid = srtr->tsid if there is at least one recovery
     *                        pending transaction for this job and this thread
     *             out->tsid != 0 if there is at least one recovery pending
     *                           transaction for this job, but for a different
     *                           thread (specified by out->tsid)
     *             out->tsid = 0 if there are no recovery pending transactions
     *                          for this job & thread
     *             out->block_id != 0 it's the block_id of the transaction must
     *                               be recovered for this job & thread
     *             out->block_id == 0 no transaction for this job & thread
     * @return @ref LIXA_RC_OK if a transaction for this job and thread was
     *                         found, block_id was dequeued;
     *         @ref LIXA_RC_BYPASSED_OPERATION if a transaction for this job,
     *                         but for a different thread was found, no block_id
     *                         was dequeued;
     *         @ref LIXA_RC_OBJ_NOT_FOUND if a transaction for this job was
     *                         NOT found, no block_id was dequeued;
     *         others return code for errors
     */
    int srvr_rcrv_tbl_get_block(srvr_rcvr_tbl_t *srt,
                                const struct srvr_rcvr_tbl_rec_s *srtr,
                                struct srvr_rcvr_tbl_rec_s *out);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SERVER_RECOVERY_H */
