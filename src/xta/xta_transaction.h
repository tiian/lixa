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
#ifndef XTA_TRANSACTION_H
# define XTA_TRANSACTION_H



/* LIXA includes */
#include "lixa_trace.h"
/* XTA includes */
#include "xta_resource.h"
#include "xta_last_operation.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_XTA



/**
 * XTA Transaction data type
 */
typedef struct {
    XTA_LAST_OPERATION_PROPERTIES;
    int dummy;
} xta_transaction_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new Transaction object
     * @return a new transaction object or NULL in the event of an error
     *         occurred
     */
    xta_transaction_t *xta_transaction_new(void);



    /**
     * Delete a Transaction object
     * @param[in] transaction object to delete
     */
    void xta_transaction_delete(xta_transaction_t *transaction);



    /**
     * Commit the transaction represented by this transaction object
     * @param[in,out] transaction object
     * @return a reason code
     */
    int xta_transaction_commit(xta_transaction_t *transaction);

    
    
    /**
     * Rollback the transaction represented by this transaction object
     * @param[in,out] transaction object
     * @return a reason code
     */
    int xta_transaction_rollback(xta_transaction_t *transaction);

    
    
    /**
     * Enlist the resource specified with the Transaction associated with the
     * Transaction object
     * @param[in,out] transaction object
     * @param[in] resource to associate
     * @return a reason code
     */
    int xta_transaction_enlist_resource(xta_transaction_t *transaction,
                                        const xta_resource_t *resource);
    

    
    /**
     * Suspend the transaction represented by this transaction object; the
     * transaction can be resumed with @ref xta_transaction_resume at a later
     * time
     * @param[in,out] transaction object
     * @param[in] flags can be @ref TMMIGRATE if the Resource Manager supports
     *            transaction migration and @ref TMNOFLAGS otherwise
     * @return a reason code
     */
    int xta_transaction_suspend(xta_transaction_t *transaction,
                                long flags);
    

    
    /**
     * Resume the transaction represented by xid in this transaction object;
     * the transaction has been previously suspended with
     * @ref xta_transaction_suspend
     * @param[in,out] transaction object
     * @param[in] xid identifier of the transaction that must be resumed
     * @return a reason code
     */
    int xta_transaction_resume(xta_transaction_t *transaction,
                               const xta_xid_t *xid);
    

    
    /**
     * Create a new branch of the transaction represented by xid in this
     * transaction object; the global transaction has been previously started
     * @param[in,out] transaction object
     * @param[in] xid identifier of the global transaction that must be
     *            branched
     * @return a reason code
     */
    int xta_transaction_branch(xta_transaction_t *transaction,
                               const xta_xid_t *xid);
    

    
    /**
     * Get the Xid associated with the Transaction object
     * @param[in] transaction object
     * @return a reference to the Transaction Identifier
     */
    const xta_xid_t *xta_transaction_get_xid(
        const xta_transaction_t *transaction);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* XTA_TRANSACTION_H */
