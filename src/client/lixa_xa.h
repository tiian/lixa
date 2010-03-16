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
#ifndef LIXA_XA_H
# define LIXA_XA_H



#include <config.h>



#include <lixa_trace.h>
#include <client_status.h>



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
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_close call
     * @return a reason code
     */
    int lixa_xa_close(client_status_t *cs, int *txrc);
    
    
    
    /**
     * Commit work performed on behalf of the transaction manager
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_commit/tx_rollback call
     * @param one_phase_commit IN boolean value:
     *            TRUE = the T.M. is performing a ONE phase commit
     *            FALSE = the T.M. is performing a standard TWO phase commit
     * @return a reason code
     */
    int lixa_xa_commit(client_status_t *cs, int *txrc, int one_phase_commit);
    
    
    
    /**
     * End work performed on behalf of the transaction manager
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_commit/tx_rollback call
     * @param commit IN boolean value:
     *                  TRUE = xa_end will be followed by xa_commit
     *                  FALSE = xa_end will be followed by xa_rollback
     * @return a reason code
     */
    int lixa_xa_end(client_status_t *cs, int *txrc, int commit);
    
    
    
    /**
     * Open all the resource managers necessary for the transaction
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_open call
     * @param next_txstate IN the txstate will be reached by the control thread
     *                        after executing this function
     * @return a reason code
     */
    int lixa_xa_open(client_status_t *cs, int *txrc, int next_txstate);
    


    /**
     * End work performed on behalf of the transaction manager
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_commit/tx_rollback call
     * @param commit OUT boolean value:
     *                  TRUE = xa_prepare will be followed by xa_commit
     *                  FALSE = xa_prepare will be followed by xa_rollback
     *                          (one resource manager is not able to prepare
     *                          for commit and the transaction must be backed
     *                          out)
     * @return a reason code
     */
    int lixa_xa_prepare(client_status_t *cs, int *txrc, int *commit);
    
    
    
    /**
     * Roll back work performed on behalf of the transaction manager
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_commit/tx_rollback call
     * @return a reason code
     */
    int lixa_xa_rollback(client_status_t *cs, int *txrc);
    
    
    
    /**
     * Send xa_start to all the resource manager does not support dynamic
     * registration
     * @param cs IN reference to the status of the calling client
     * @param txrc OUT return code prepared for tx_open call
     * @param xid IN transaction id of the new transaction
     * @param next_txstate IN the txstate will be reached by the control thread
     *                        after executing this function
     * @return a reason code
     */
    int lixa_xa_start(client_status_t *cs, int *txrc, XID *xid,
                      int next_txstate);



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
