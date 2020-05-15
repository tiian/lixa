/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXA_XA_H
# define LIXA_XA_H



#include "config.h"



#include "lixa_trace.h"
#include "client_status.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_XA



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    /**
     * Close all the resource managers necessary for the transaction
     * @param[in] ccc : client config collection
     * @param[in] cs : reference to the status of the calling client
     * @param[out] txrc : return code prepared for tx_close call
     * @return a reason code
     */
    int lixa_xa_close(client_config_coll_t *ccc, client_status_t *cs,
                      int *txrc);


    
    /**
     * Commit work performed on behalf of the transaction manager
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] xid transaction ID to commit
     * @param[out] txrc return code prepared for tx_commit/tx_rollback call
     * @param[in] one_phase_commit boolean value:
     *            TRUE = the T.M. is performing a ONE phase commit
     *            FALSE = the T.M. is performing a standard TWO phase commit
     * @return a reason code
     */
    int lixa_xa_commit(client_config_coll_t *ccc, client_status_t *cs,
                       const XID *xid, int *txrc, int one_phase_commit);


    /**
     * End work performed on behalf of the transaction manager
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] xid transaction ID to detach
     * @param[out] txrc return code prepared for tx_commit/tx_rollback call
     * @param[in] commit boolean value: <BR>
     *                  TRUE = xa_end will be followed by xa_commit <BR>
     *                  FALSE = xa_end will be followed by xa_rollback
     * @param[in] xa_end_flags flags to send to xa_end
     * @return a reason code
     */
    int lixa_xa_end(client_config_coll_t *ccc, client_status_t *cs,
                    const XID *xid, int *txrc, int commit, int xa_end_flags);


    
    /**
     * This function is not directly called by the TX layer, but it's an
     * helper function for @ref lixa_xa_commit and @ref lixa_xa_rollback; this
     * is the original LIXA version that supports XTA implementation and does
     * not need the multi xids extension implemented by TC TX (see
     * @ref lixa_xa_forget_multi)
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] xid transaction ID to forget
     * @param[in] finished boolean value: TRUE, the transaction can be
     *                    finished; FALSE, a blocking error marked the
     *                    transaction as not finished
     * @return a reason code
     */
    int lixa_xa_forget(client_config_coll_t *ccc, client_status_t *cs,
                       const XID *xid, int finished);

    
    
    /**
     * This function is not directly called by the TX layer, but it's an
     * helper function for @ref lixa_xa_commit and @ref lixa_xa_rollback; this
     * is a specific version of @ref lixa_xa_prepare designed to support the
     * TC TX extension provided by Globetom; XTA implementation does not need
     * multiple xids prepare
     * @param[in] cs reference to the status of the calling client
     * @param[in] xida
     * @param[in] finished boolean value: TRUE, the transaction can be
     *                    finished; FALSE, a blocking error marked the
     *                    transaction as not finished
     * @return a reason code
     */
    int lixa_xa_forget_multi(client_status_t *cs, GArray *xida, int finished);


    /**
     * Open all the resource managers necessary for the transaction
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[out] txrc return code prepared for tx_open call
     * @param[in] next_txstate the txstate will be reached by the control
     *                         thread after executing this function
     * @param[in] mmode the operation is performed inside a maintenance
     *                  session
     * @return a reason code
     */
    int lixa_xa_open(client_config_coll_t *ccc, client_status_t *cs,
                     int *txrc, int next_txstate, int mmode);

    

    /**
     * Prepare work performed on behalf of the transaction manager; this is the
     * original LIXA version that supports XTA implementation and does not
     * need the multi xids extension implemented by TC TX (see
     * @ref lixa_xa_prepare_multi)
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] xid transaction ID to prepare
     * @param[in] non_block boolean value: <br>
     *            TRUE = xa_prepare will not block the caller <br>
     *            FALSE = xa_prepare will block the caller <br>
     *            the option is used only for multiple branch transactions
     * @param[out] txrc return code prepared for tx_commit/tx_rollback call
     * @param[out] commit boolean value: <br>
     *                  TRUE = xa_prepare will be followed by xa_commit <br>
     *                  FALSE = xa_prepare will be followed by xa_rollback
     *                          (one resource manager is not able to prepare
     *                          for commit and the transaction must be backed
     *                          out)
     * @return a reason code
     */
    int lixa_xa_prepare(client_config_coll_t *ccc, client_status_t *cs,
                        const XID *xid, int non_block, int *txrc, int *commit);

    

    
    /**
     * Wait all the branches prepared: this step is necessary before commit
     * can be performed in a multiple branches global transaction
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @return a reason code
     */
    int lixa_xa_prepare_wait_branches(client_config_coll_t *ccc,
                                      client_status_t *cs);


    
    /**
     * Prepare work performed on behalf of the transaction manager; this is a
     * specific version of @ref lixa_xa_prepare designed to support the
     * TC TX extension provided by Globetom; XTA implementation does not need
     * multiple xids prepare
     * @param[in] cs : reference to the status of the calling client
     * @param[in] xida array of XID to prepare
     * @param[out] txrc : return code prepared for tx_commit/tx_rollback call
     * @param[out] commit : boolean value: <br>
     *                  TRUE = xa_prepare will be followed by xa_commit <br>
     *                  FALSE = xa_prepare will be followed by xa_rollback
     *                          (one resource manager is not able to prepare
     *                          for commit and the transaction must be backed
     *                          out)
     * @param[out] xid the final XID to commit on
     * @return a reason code
     */
    int lixa_xa_prepare_multi(client_status_t *cs,
                              GArray *xida, int *txrc, int *commit, XID *xid);


    /**
     * Roll back work performed on behalf of the transaction manager
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] xid transaction ID to prepare
     * @param[out] txrc return code prepared for tx_commit/tx_rollback call
     * @param[in] tx_commit the function is called from tx_commit (TRUE) or
     *                  from tx_rollback (FALSE)
     * @return a reason code
     */
    int lixa_xa_rollback(client_config_coll_t *ccc, client_status_t *cs,
                         const XID *xid, int *txrc, int tx_commit);


    /**
     * Send xa_start to all the resource manager does not support dynamic
     * registration
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[out] txrc return code prepared for tx_open call
     * @param[in] xid transaction id of the new transaction
     * @param[in] txstate the current txstate of the control thread
     * @param[in] next_txstate the txstate will be reached by the control
     *            thread after executing this function without errors
     * @param[out] dupid_or_proto boolean flag: TRUE if one or more resource
     *                       managers returned XAER_DUPID or XAER_PROTO
     * @param[in] xa_start_flags the flags to send to xa_start
     * @return a reason code
     */
    int lixa_xa_start(client_config_coll_t *ccc, client_status_t *cs,
                      int *txrc, const XID *xid,
                      int txstate, int next_txstate, int *dupid_or_proto,
                      int xa_start_flags);

    

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */


#endif /* LIXA_XA_H */
