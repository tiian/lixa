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
     * @param rwrm OUT number of resource manager with read/write transaction
     * @return a reason code
     */
    int lixa_xa_end(client_status_t *cs, int *txrc, int commit, int *rwrm);
    
    
    
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
