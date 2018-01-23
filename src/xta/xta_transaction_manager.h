/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "xta_transaction.h"
#include "xta_acquired_xa_resource.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_XTA



/**
 * This type is just a redefinition of the legacy LIXA type
 * "client_config_coll_t" to avoid a type with a "strange name" in the API
 */
typedef client_config_coll_t xta_transaction_manager_config_t;



/**
 * XTA Transaction Manager data type
 */
typedef struct xta_transaction_manager_s {
    /**
     * A mutex that's necessary to synchronize some operations in a
     * multithreaded environment
     */
    GMutex                           mutex;
    /**
     * Currently managed Transaction objects,
     * @see xta_transaction_manager_begin
     */
    GHashTable                      *transactions;
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
     * @param[in] this : transaction manager to delete
     */
    void xta_transaction_manager_delete(xta_transaction_manager_t *this);



    /**
     * Get the legacy LIXA object that contains the static global
     * configurations for all the statically defined Resource Managers.
     * This method must be intended as STATIC because it does not access the
     * object state.
     * @return the pointer to the Transaction Manager configuration object
     */
    static inline
    xta_transaction_manager_config_t *
    xta_transaction_manager_get_config(void) {
        return &global_ccc;
    }
    
    

    /**
     * Create a new XA Transaction object, associate it with the current
     * process/thread and returns it to the caller. In the
     * event that the caller thread has already created an XA Transaction, the
     * previously created XA Transaction object is returned
     * @param[in,out] this : transaction manager object
     * @return the pointer to an XTA Transaction object
     */
    xta_transaction_t *xta_transaction_manager_create_transaction(
        xta_transaction_manager_t *this);



    /**
     * Create a new transaction branch inside an existing global transaction
     * and associate it with the current process/thread
     * @param[in,out] this : transaction manager object
     * @param[in] xid transaction identifier object
     * @return a reason code
     */
    int xta_transaction_manager_branch(xta_transaction_manager_t *this,
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
