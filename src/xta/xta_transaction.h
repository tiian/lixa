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
#include "client_status.h"
/* XTA includes */
#include "xta_xa_resource.h"



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
 * @ref client_config_coll_t to avoid a type with a "strange name" in the API
 */
typedef client_config_coll_t xta_transaction_config_t;



/*
 * This type is a declaration only statement: the real type is defined inside
 * XA Resource header file. Here we just need to store a pointer to
 * a XA Resource inside an XA Transaction.
 */
typedef struct xta_xa_resource_s xta_xa_resource_t;



/**
 * XTA Transaction data type
 */
typedef struct xta_transaction_s {
    /**
     * LIXA client status
     */
    client_status_t                  client_status;
    /**
     * LIXA client configuration (it's a collection). The name of the property
     * contains the previx "local" to strongly distinguish it from the
     * "global" instance that's used by legacy LIXA due to TX specification
     * limitation
     */
    xta_transaction_config_t         local_ccc;
    /**
     * Transaction ID
     */
    xta_xid_t                       *xid;
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
     * @param[in] this : transaction object to delete
     */
    void xta_transaction_delete(xta_transaction_t *this);



    /**
     * Get the legacy LIXA object that contains the configurations for all the
     * define Resource Managers
     * @param[in] this : Transaction object
     * @return the pointer to the Transaction configuration object
     */
    xta_transaction_config_t *xta_transaction_get_config(
        xta_transaction_t *this);
    
    

    /**
     * Enlist the resource specified with the Transaction associated with the
     * Transaction object
     * @param[in,out] this : transaction object
     * @param[in] xa_res : resource to associate
     * @return a reason code
     */
    int xta_transaction_enlist_resource(xta_transaction_t *this,
                                        xta_xa_resource_t *xa_res);
    

    
    /**
     * Prepare the XA Resource Managers and the XA Transaction Manager for a
     * new transactional Unit of Work. From the XA specification point of view,
     * it calls xa_open (for the statically defined XA Resource Managers)
     * and xa_start
     * @param[in,out] this : transaction object
     * @return a reason code
     */
    int xta_transaction_begin(xta_transaction_t *this);

    
    
    /**
     * Commit the transaction represented by this transaction object
     * @param[in,out] this : transaction object
     * @return a reason code
     */
    int xta_transaction_commit(xta_transaction_t *this);

    
    
    /**
     * Rollback the transaction represented by this transaction object
     * @param[in,out] this : transaction object
     * @return a reason code
     */
    int xta_transaction_rollback(xta_transaction_t *this);

    
    
    /**
     * When a resource registers dynamically the digest must be updated because
     * the configuration has been altered. This must be considered a PRIVATE
     * method and should not be used by the Application Program.
     * @param[in,out] this : transaction object
     * @param[in] xrc : XA resource configuration (LIXA legacy structure)
     * @return a reason code
     */
    int xta_transaction_redigest(xta_transaction_t *this,
                                 const xta_xa_resource_config_t *xrc);

    

    /**
     * Suspend the transaction represented by this transaction object; the
     * transaction can be resumed with @ref xta_transaction_resume at a later
     * time
     * @param[in,out] this : transaction object
     * @param[in] flags can be @ref TMMIGRATE if the Resource Manager supports
     *            transaction migration and @ref TMNOFLAGS otherwise
     * @return a reason code
     */
    int xta_transaction_suspend(xta_transaction_t *this,
                                long flags);
    

    
    /**
     * Resume the transaction represented by xid in this transaction object;
     * the transaction has been previously suspended with
     * @ref xta_transaction_suspend
     * @param[in,out] this : transaction object
     * @param[in] xid identifier of the transaction that must be resumed
     * @return a reason code
     */
    int xta_transaction_resume(xta_transaction_t *this,
                               const xta_xid_t *xid);
    

    
    /**
     * Create a new branch of the transaction represented by xid in this
     * transaction object; the global transaction has been previously started
     * @param[in,out] this : transaction object
     * @param[in] xid identifier of the global transaction that must be
     *            branched
     * @return a reason code
     */
    int xta_transaction_branch(xta_transaction_t *this,
                               const xta_xid_t *xid);
    

    
    /**
     * Get the Xid associated with the Transaction object
     * @param[in] this : transaction object
     * @return a reference to the Transaction Identifier
     */
    const xta_xid_t *xta_transaction_get_xid(const xta_transaction_t *this);

    
    
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
