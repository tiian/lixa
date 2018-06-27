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



/* XTA includes */
#include "xta_transaction.h"
#include "xta_acquired_xa_resource.h"



/**
 * This typedef are necessary to avoid the inclusion of LIXA internals that
 * are unnecessary for the XTA interface, but necessary for XTA implementation.
 * The real type is client_config_coll_t, by XTA is used only as a
 * pointer
 */
typedef void xta_transaction_manager_config_t;



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
     * @param[in] tm : transaction manager to delete
     */
    void xta_transaction_manager_delete(xta_transaction_manager_t *tm);



    /**
     * Get the legacy LIXA object that contains the static global
     * configurations for all the statically defined Resource Managers.
     * This method must be intended as STATIC because it does not access the
     * object state.
     * @return the pointer to the Transaction Manager configuration object
     */
    xta_transaction_manager_config_t *
    xta_transaction_manager_get_config(void);
    
    

    /**
     * Create a new XA Transaction object, associate it with the current
     * process/thread and returns it to the caller. In the
     * event that the caller thread has already created an XA Transaction, the
     * previously created XA Transaction object is returned
     * @param[in,out] tm : transaction manager object
     * @return the pointer to an XTA Transaction object
     */
    xta_transaction_t *xta_transaction_manager_create_transaction(
        xta_transaction_manager_t *tm);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* XTA_TRANSACTION_MANAGER_H */
