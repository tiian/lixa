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
#ifndef SRVR_RCVR_TBL_H
# define SRVR_RCVR_TBL_H



#include <config.h>



#include <lixa_config.h>
#include <lixa_xml_msg.h>



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
     * @param srtr IN reference to the search record:<br>
     *             srtr->job = the job is performing recovery;<br>
     *             srtr->tsid = the thread status id is performing recovery;<br>
     *             srtr->block_id unused<br>
     *             <b>NOTE:</b> srtr->job must point to a valid job structure
     * @param out IN/OUT reference to the output record:<br>
     *             out->job = srtr->job if there is at least one recovery
     *                       pending transaction for this job;<br>
     *             out->job = "" if there are no recovery pending transactions
     *                          for this job;<br>
     *             out->tsid = srtr->tsid if there is at least one recovery
     *                        pending transaction for this job and this
     *                        thread;<br>
     *             out->tsid != 0 if there is at least one recovery pending
     *                           transaction for this job, but for a different
     *                           thread (specified by out->tsid);<br>
     *             out->tsid = 0 if there are no recovery pending transactions
     *                          for this job & thread;<br>
     *             out->block_id != 0 it's the block_id of the transaction must
     *                               be recovered for this job & thread;<br>
     *             out->block_id == 0 no transaction for this job & thread;<br>
     *             <b>NOTE:</b> out->job must point to a valid job structure
     * @param browse IN boolean value: if TRUE, don't remove the found info
     *               from the recovery table, browse it only
     * @return @ref LIXA_RC_OK if a transaction for this job and thread was
     *                         found, block_id was dequeued
     *                         if browse == FALSE;<br>
     *         @ref LIXA_RC_BYPASSED_OPERATION if a transaction for this job,
     *                         but for a different thread was found, no block_id
     *                         was dequeued;<br>
     *         @ref LIXA_RC_OBJ_NOT_FOUND if a transaction for this job was
     *                         NOT found, no block_id was dequeued;<br>
     *         a different codes mean error
     */
    int srvr_rcvr_tbl_get_block(srvr_rcvr_tbl_t *srt,
                                const struct srvr_rcvr_tbl_rec_s *srtr,
                                struct srvr_rcvr_tbl_rec_s *out,
                                int browse);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* SRVR_RCVR_TBL_H */
