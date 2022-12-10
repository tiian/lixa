/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_TX_H
# define LIXA_TX_H

#include <config.h>

#ifdef HAVE_GLIB_H
#include <glib.h>
#endif

#include <tx.h>
#include <lixa_trace.h>

/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_TX

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    
    /**
     * This function implements the real logic underlaying @ref tx_close
     * X/Open function
     * @param[out] txrc tx_* return code
     * @return a return code
     */
    int lixa_tx_close(int *txrc);


    
    /**
     * This function can be used to clean-up memory after a TX_FAIL condition;
     * the primary usage is related to memory leak control and detection.
     * The normal behavior when a TX_FAIL return code happens is a (small)
     * memory leak to avoid the same thread of control uses the library again.
     */
    void lixa_tx_close_cleanup(void);


    
    /**
     * This function implements the real logic underlaying @ref tx_commit
     * X/Open function
     * @param[out] txrc tx_* return code
     * @param[out] begin_new is a TRUE (boolean) value if a new transaction
     *        must be started with @ref lixa_tx_begin
     * @return a return code
     */
    int lixa_tx_commit(int *txrc, int *begin_new);


    
    /**
     * This function implements the real logic underlaying @ref tx_begin
     * X/Open function
     * @param[out] txrc tx_* return code
     * @param[in] xid previously created XID if joining a transaction
     * @param[in] flags passed to @ref lixa_xa_start and xa_start
     * @return a return code
     */
    int lixa_tx_begin(int *txrc, XID *xid, int flags);


    
    /**
     * This function implements the real logic underlying @ref tx_end non
     * standard function.
     * @param[out] txrc tx_* return code
     * @param[in] flags that will be passed to @ref lixa_xa_end and xa_end
     * @return
     */
    int lixa_tx_end(int *txrc, int flags);


    
    /**
     * This function implements the real logic underlaying @ref tx_info
     * X/Open function
     * @param[out] txrc tx_* return code
     * @param[out] info TXINFO structure
     * @return a return code
     */
    int lixa_tx_info(int *txrc, TXINFO *info);


    
    /**
     * This function implements the real logic underlaying @ref tx_open
     * X/Open function
     * @param[out] txrc tx_* return code
     * @param[in] mmode boolean value: TRUE if the function is called from a
     *                 maintentance client instead of a normal Application
     *                 Program
     * @return a return code
     */
    int lixa_tx_open(int *txrc, int mmode);


    
    /**
     * This function implements the real logic underlaying @ref tx_rollback
     * X/Open function
     * @param[out] txrc tx_* return code
     * @param[out] begin_new is a TRUE (boolean) value if a new transaction
     *        must be started with @ref lixa_tx_begin
     * @return a return code
     */
    int lixa_tx_rollback(int *txrc, int *begin_new);


    
    /**
     * This function implements the real logic underlaying
     * @ref tx_set_commit_return X/Open function
     * @param[out] txrc tx_* return code
     * @param[in] when_return commit_return characteristic
     * @return a return code
     */
    int lixa_tx_set_commit_return(int *txrc, COMMIT_RETURN when_return);


    
    /**
     * This function implements the real logic underlaying
     * @ref tx_set_transaction_control X/Open function
     * @param[out] txrc tx_* return code
     * @param[in] control transaction control characteristic
     * @return a return code
     */
    int lixa_tx_set_transaction_control(int *txrc,
                                        TRANSACTION_CONTROL control);


    
    /**
     * This function implements the real logic underlaying
     * @ref tx_set_transaction_timeout X/Open function
     * @param[out] txrc tx_* return code
     * @param[in] timeout transaction timeout (seconds)
     * @return a return code
     */
    int lixa_tx_set_transaction_timeout(int *txrc,
                                        TRANSACTION_TIMEOUT timeout);


    
    /**
     * <b>Note:</b> tx_recover is <b>not</b> a standard function, and this
     * function is directly called from the client program.
     * Perform manual recovery: query resource managers, analyze answers,
     * commit/rollback
     * @param[in] report print a report of the prepared and in-doubt
     *                  transactions
     * @param[in] commit prepared and in-doubt transactions
     * @param[in] rollback prepared and in-doubt transactions
     * @param[in] bbqc bypass branch qualifier check (TRUE/FALSE)
     * @param[in] bfic bypass format id qualifier check (TRUE/FALSE)
     * @param[in] utf use TMENDRSCAN flag for last xa_recover call (TRUE/FALSE)
     * @param[in] xid transaction to commit/rollback
     * @param[in] xid_file (file) list of transaction(s) to commit/rollback
     * @return a standardized return code
     */
    int lixa_tx_recover(int report, int commit, int rollback, int bbqc,
                        int bfic, int utf, const char *xid,
                        const char *xid_file);


    
    /**
     * @brief Retrieves information about all existing transactions
     * <b>Note:</b> This is a non standard function.
     * @param[in,out] xida a tree populated with all the transactions known by
     *                the server
     * @param[in] maint
     * @param[in] report flag indicating if the information should be reported
     * @param[in] unique flag indicating if only unique transactions should be
     *            listed
     * @return a standardised return code
     */
    int lixa_tx_tpm(GArray *xida, int maint, int report, int unique);


    
    /**
     * This function is used to clean-up the environment when a function
     * ends with TX_FAIL and the calling program can not call other functions
     * but tx_open. The primary usage is avoiding memory leaks.
     */
    void lixa_tx_cleanup(void);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif /* LIXA_TX_H */
