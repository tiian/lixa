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
#ifndef XTA_TRANSACTION_H
# define XTA_TRANSACTION_H



/* XTA includes */
#include "xta_config.h"
#include "xta_xa_resource.h"



/**
 * This typedef are necessary to avoid the inclusion of LIXA internals that
 * are unnecessary for the XTA interface, but necessary for XTA implementation.
 * The real type is client_status_t, by XTA is used only as a
 * pointer
 */
typedef void xta_transaction_client_status_t;



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
     * Boolean value used to store if the open method has been already called
     * or not
     */
    int                              already_opened;
    /**
     * Boolean value used to store if the start method has been called with
     * multiple_branches flag
     */
    int                              multiple_branches;
    /**
     * LIXA client status
     */
    xta_transaction_client_status_t *client_status;
    /**
     * LIXA client configuration (it's a collection). The name of the property
     * contains the prefix "local" to strongly distinguish it from the
     * "global" instance that's used by legacy LIXA due to TX specification
     * limitation
     */
    xta_config_t                    *local_ccc;
    /**
     * Transaction ID
     */
    xta_xid_t                       *xid;
    /**
     * The transaction has already called @ref xta_transaction_commit with
     * non_block flag and must call @ref xta_transaction_commit again to
     * complete it
     */
    int                              commit_suspended;
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
     * @param[in] transact : transaction object to delete
     * @deprecated only @ref xta_transaction_manager_delete should call this
     *             function. If arbitrarily called, segmentation fault due to
     *             double free of the same pointer can happen.
     */
    void xta_transaction_delete(xta_transaction_t *transact) DEPRECATED;



    /**
     * Check if the transaction object can be safely deleted: if there's a
     * transaction in progress, this method returns FALSE
     * @deprecated only @ref xta_transaction_manager_create_transaction should
     *             call this function.
     * @param[in] transact : transaction object to delete
     * @return a boolean value
     */
    int xta_transaction_safe_delete(const xta_transaction_t *transact) DEPRECATED;


    
    /**
     * Get a reference to the configuration that applies to this transaction
     * object, like all the define Resource Managers
     * @param[in] transact : Transaction object
     * @return the pointer to the Transaction configuration object
     */
    xta_config_t *xta_transaction_get_config(xta_transaction_t *transact);
    
    

    /**
     * Enlist the resource specified with the Transaction associated with the
     * Transaction object. Note: XTA guarantees that all the XA functions are
     * called in the same exact order of enlistment.
     * @param[in,out] transact : transaction object
     * @param[in] xa_res : resource to associate
     * @return a reason code
     */
    int xta_transaction_enlist_resource(xta_transaction_t *transact,
                                        xta_xa_resource_t *xa_res);
    

    /**
     * @deprecated
     */
    int xta_transaction_open(xta_transaction_t *transact) DEPRECATED ;
    /*
     * Prepare the XA Resource Managers and the XA Transaction Manager for a
     * new transactional Unit of Work. From the XA specification point of view,
     * it calls xa_open (for the Native XA Resource Managers)
     * @param[in,out] transact : transaction object
     * @return a reason code
     */
    int xta_transaction_open_internal(xta_transaction_t *transact);


    
    /**
     * @deprecated
     */
    int xta_transaction_close(xta_transaction_t *transact) DEPRECATED ;
    /*
     * Shut down the XA Resource Managers and the XA Transaction Manager after
     * transactional Unit of Work completion. From the XA specification point
     * of view, it calls xa_close (for the Native XA Resource Managers)
     * @param[in,out] transact : transaction object
     * @return a reason code
     */
    int xta_transaction_close_internal(xta_transaction_t *transact);
    
    

    /**
     * Explicitly open and close all the enlisted resource to look for
     * recovery pending transaction in the LIXA state server. In normal
     * condition, this is not necessary, because the same happens when
     * @ref xta_transaction_start, @ref xta_transaction_resume and
     * @ref xta_transaction_branch are called
     * @param[in,out] transact : transaction object
     * @return a reason code
     */
    int xta_transaction_recover(xta_transaction_t *transact);

    
    
    /**
     * Start a new XA Transaction. From the XA specification point of
     * view, it calls xa_start (for the Native XA Resource Managers)
     * @param[in,out] transact : transaction object
     * @param[in] multiple_branches : boolean value: <br>
     *            TRUE = the created transaction will span more applications,
     *                   @ref xta_transaction_branch will be called
     *                   subsequently <br>
     *            FALSE = the created transaction will not span more
     *                   applications and @ref xta_transaction_branch will not
     *                   be called for this transaction <br>
     * @return a reason code
     */
    int xta_transaction_start(xta_transaction_t *transact,
                              int multiple_branches);


    
    /**
     * Commit the transaction represented by this transaction object
     * @param[in,out] transact : transaction object
     * @param[in] non_blocking boolean value: <br>
     *            TRUE = xa_prepare will not block the caller <br>
     *            FALSE = xa_prepare will block the caller <br>
     *            the option is used only for multiple branch transactions
     * @return a reason code
     */
    int xta_transaction_commit(xta_transaction_t *transact, int non_blocking);

    
    
    /**
     * Rollback the transaction represented by this transaction object
     * @param[in,out] transact : transaction object
     * @return a reason code
     */
    int xta_transaction_rollback(xta_transaction_t *transact);

    
    
    /**
     * When a resource registers dynamically the digest must be updated because
     * the configuration has been altered. This must be considered a PRIVATE
     * method and should not be used by the Application Program.
     * @param[in,out] transact : transaction object
     * @param[in] xrc : XA resource configuration (LIXA legacy structure)
     * @return a reason code
     */
    int xta_transaction_redigest(xta_transaction_t *transact,
                                 const xta_xa_resource_config_t *xrc);

    

    /**
     * Suspend the transaction represented by this transaction object; the
     * transaction can be resumed with @ref xta_transaction_resume at a later
     * time
     * @param[in,out] transact : transaction object
     * @param[in] flags can be @ref TMMIGRATE if the Resource Manager supports
     *            transaction migration and @ref TMNOFLAGS otherwise
     * @return a reason code
     */
    int xta_transaction_suspend(xta_transaction_t *transact, long flags);
    

    
    /**
     * Resume the transaction represented by xid in this transaction object;
     * the transaction has been previously suspended with
     * @ref xta_transaction_suspend
     * @param[in,out] transact transaction object
     * @param[in] xid_string serialized identifier of the transaction that
     *            must be resumed (see @ref xta_xid_to_string)
     * @param[in] flags can be @ref TMRESUME if the transaction has been
     *            suspended using @ref TMMIGRATE or @ref TMJOIN if the
     *            transaction has been suspended using @ref TMNOFLAGS
     * @return a reason code
     */
    int xta_transaction_resume(xta_transaction_t *transact,
                               const char *xid_string, long flags);
    

    
    /**
     * Create a new branch of the transaction represented by xid in this
     * transaction object; the global transaction has been previously started
     * @param[in,out] transact : transaction object
     * @param[in] xid_string serialized identifier of the global transaction
     *            that must be branched
     * @return a reason code
     */
    int xta_transaction_branch(xta_transaction_t *transact,
                               const char *xid_string);
    

    
    /**
     * Get the Xid associated with the Transaction object
     * @param[in] transact : transaction object
     * @return a reference to the Transaction Identifier
     */
    const xta_xid_t *xta_transaction_get_xid(
        const xta_transaction_t *transact);



    /**
     * Get the flag multiple_branches
     * @param[in] transact : transaction object
     * @return multiple_branches flag
     */     
    int xta_transaction_get_multiple_branches(
        const xta_transaction_t *transact);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* XTA_TRANSACTION_H */
