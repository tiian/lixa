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
#ifndef XTA_TRANSACTION_MANAGER_H
# define XTA_TRANSACTION_MANAGER_H



/* LIXA includes */
#include "lixa_trace.h"
#include "client_status.h"
/* XTA includes */
#include "xta_last_operation.h"
#include "xta_transaction.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_XTA



/**
 * XTA Transaction Manager data type
 */
typedef struct {
    /**
     * LIXA client status
     */
    client_status_t      client_status;
    
    XTA_LAST_OPERATION_PROPERTIES;
    int dummy;
} xta_transaction_manager_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new Transaction Manager object
     * @return a new transaction manager object or NULL in the event of an
     *         error occurred
     */
    xta_transaction_manager_t *xta_transaction_manager_new(void);



    /**
     * Delete a Transaction Manager object
     * @param[in] transaction_manager to delete
     */
    void xta_transaction_manager_delete(
        xta_transaction_manager_t *transaction_manager);



    /**
     * Get the XTA Transaction object that represents the transaction context
     * of the calling thread
     * @param[in,out] transaction_manager object
     * @return the pointer to an XTA Transaction object
     */
    xta_transaction_t *
    xta_transaction_manager_get_transaction(
        xta_transaction_manager_t *transaction_manager);

    

    /**
     * Create a new transaction and associate it with the current
     * process/thread
     * @param[in,out] transaction_manager object
     * @return a reason code
     */
    int xta_transaction_manager_begin(
        xta_transaction_manager_t *transaction_manager);



    /**
     * Create a new transaction branch inside an existing global transaction
     * and associate it with the current process/thread
     * @param[in,out] transaction_manager object
     * @param[in] xid transaction identifier object
     * @return a reason code
     */
    int xta_transaction_manager_branch(
        xta_transaction_manager_t *transaction_manager,
        const xta_xid_t *xid);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* XTA_TRANSACTION_MANAGER_H */
