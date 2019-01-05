/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef XTA_XID_H
# define XTA_XID_H



/* LIXA includes */
#include <xa.h>



/**
 * XTA Transaction data type
 */
typedef struct {
    /**
     * Transaction ID using the standard XA format
     */
    XID          xa_xid;
} xta_xid_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new Transaction Identifier object and generate a new unique XID
     * @param[in] branch_qualifier that must be assigned to the XID
     *            (in ASCII HEX format)
     * @param[in] multiple_branches : boolean value: <br>
     *            TRUE = the created transaction will span more applications,
     *                   @ref xta_transaction_branch will be called
     *                   subsequently <br>
     *            FALSE = the created transaction will not span more
     *                   applications and @ref xta_transaction_branch will not
     *                   be called for this transaction <br>
     * @return a new transaction identifier object or NULL in the event of an
     *         error occurred
     */
    xta_xid_t *xta_xid_new(const char *branch_qualifier,
                           int multiple_branches);



    /**
     * Check if a branch_qualifier is related to a multiple branches
     * transaction or not
     * @param[in] branch_qualifier that must be checked
     * @return a boolean value: TRUE for multiple branches transaction
     */
    int xta_xid_branch_qualifier_is_multibranch(
        const char *branch_qualifier);

    

    /**
     * Set branch_qualifier to be a multiple branches transaction
     * @param[in,out] branch_qualifier that must be checked
     * @return a boolean value: TRUE if branch_qualifier has been changed
     */
    int xta_xid_branch_qualifier_set_multibranch(char *branch_qualifier);

    

    /**
     * Unset branch_qualifier to be a multiple branches transaction
     * @param[in,out] branch_qualifier that must be checked
     * @return a boolean value: TRUE if branch_qualifier has been changed
     */
    int xta_xid_branch_qualifier_unset_multibranch(char *branch_qualifier);

    

    /**
     * Create a new Transaction Identifier object and set XID as passed by the
     * caller
     * @param[in] xid_string a serialized XID, see @ref xta_xid_to_string
     * @return a new transaction identifier object or NULL in the event of an
     *         error occurred
     */
    xta_xid_t *xta_xid_new_from_string(const char *xid_string);


    
    /**
     * Create a new Transaction Identifier object and set XID as passed by the
     * caller. This is considered a private method because the an XTA
     * application should not manage the XID struct directly.
     * @param[in] xid a native XA XID
     * @return a new transaction identifier object or NULL in the event of an
     *         error occurred
     */
    xta_xid_t *xta_xid_new_from_XID(const XID *xid);

    

    /**
     * Create a new Transaction Identifier object duplicating the passed one
     * @param[in] xid is the transaction identifier that must be duplicated
     * @return a new transaction identifier object or NULL in the event of an
     *         error occurred
     */
    xta_xid_t *xta_xid_dup(const xta_xid_t *xid);


    
    
    /**
     * Delete a Transaction Identifier object
     * @param[in] xid object to delete
     */
    void xta_xid_delete(xta_xid_t *xid);



    /**
     * Retrieve the transaction ID in the XA standard format
     * @param[in] xid object
     * @return a reference to the XA representation of the transaction id
     */
    const XID *xta_xid_get_xa_xid(const xta_xid_t *xid);



    /**
     * Convert the transaction ID to an ASCII string
     * @param[in] xid object
     * @return a string that must be released using free() function by the
     *         caller
     */
    char *xta_xid_to_string(const xta_xid_t *xid);



    /**
     * Reset a Transaction identifier object
     * @param[in,out] xid object to delete
     */
    void xta_xid_reset(xta_xid_t *xid);



    /**
     * This is just a wrapper of lixa_xid_get_gtrid
     */
    long xta_xid_get_gtrid(const xta_xid_t *xid, char *gtrid);
    /**
     * This is just a wrapper of lixa_xid_get_bqual
     */
    long xta_xid_get_bqual(const xta_xid_t *xid, char *bqual);
    /**
     * This is just a wrapper of lixa_xid_get_formatID
     */
    long xta_xid_get_formatID(const xta_xid_t *xid);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* XTA_XID_H */
