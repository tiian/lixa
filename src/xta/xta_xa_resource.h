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
#ifndef XTA_XA_RESOURCE_H
# define XTA_XA_RESOURCE_H



#include <config.h>



/* LIXA includes */
#include "client_config.h"
/* XA include */
#include "xa.h"
/* XTA includes */
#include "xta_xid.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_XTA



/**
 * This type is a declaration only statement: the real type is defined inside
 * Transaction header file. Here we just need to store a pointer to
 * a Transaction inside an XA Resource.
 */
typedef struct xta_transaction_s xta_transaction_t;



/**
 * This type is just a redefinition of the legacy LIXA struct
 * "act_rsrmgr_config_s" to avoid a type with a "strange name" in the API
 */
typedef struct act_rsrmgr_config_s xta_xa_resource_config_t;



/**
 * The base "class" xta_xa_resource is just a redefinition of the standard
 * xa_switch_t structure as defined by X/Open
 *
 * Inheritance is emulated using the schema proposed by Martin in this post:
 * http://stackoverflow.com/questions/1114349/struct-inheritance-in-c . <br>
 * Using -fms-extensions the result would be better, but less portable. <br>
 * Using -std=c11 the result does not improve because a "tag" (like a struct
 * is not allowed as explained here:
 * https://gcc.gnu.org/onlinedocs/gcc/Unnamed-Fields.html )
 */
typedef struct xta_xa_resource_s {
    /**
     * Partial description of the XA Resource Manager using legacy LIXA data
     * structures
     */
    struct rsrmgr_config_s           rsrmgr_config;
    /**
     * Complete description of the XA Resource Manager using legacy LIXA data
     * structures
     */
    struct act_rsrmgr_config_s       act_rsrmgr_config;
    /**
     * The object refers to an XA Resource that has been created
     * dynamically: it has not been loaded statically at startup (it's not
     * described inside lixac.conf file
     */
    int                              dynamic;
    /**
     * TRUE if the XA Resource Manager has been defined dynamically using XTA
     * and it has been opened by the Application Program <br>
     * FALSE if the XA Resource Manager must be opened by the Transaction
     * Manager by mean of xa_open (XA standard)
     */
    int                              skip_xa_open;
    /**
     * The reference to the Transaction that registered this resource
     * or NULL
     */
    const xta_transaction_t         *registered_tx;
} xta_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize the common properties of XA Resources
     * @param[in,out] this : XA Resource object
     * @param[in] skip_xa_open FALSE : the XA Resource needs an explicit call
     *                         to xa_open before usage and xa_close after
     *                         usage end <br>
     *                         TRUE : the XA Resource has been opened and will
     *                         be closed by the Application Program
     * @return a reason code
     */
    int xta_xa_resource_init(xta_xa_resource_t *this,
                             int skip_xa_open);
    

    
    /**
     * Clean the properties of a XA Resource; this function must be
     * called after @ref xta_xa_resource_init during object disposition
     * to avoid memory leaks
     * @param[in,out] this : XA Resource object
     */    
    void xta_xa_resource_clean(xta_xa_resource_t *this);



    /**
     * Return a reference (a pointer) to the configuration parameters of the
     * resource.
     * @param[in] this : XA Resource object
     * @return a pointer to the resource configuration record
     */
    const xta_xa_resource_config_t *xta_xa_resource_get_config(
        const xta_xa_resource_t *this);
    


    /**
     * Check if the XA Resource has been created dynamically or not
     * @param[in] this : XA Resource object
     * @return a boolean value
     */
    static inline int xta_xa_resource_is_dynamic(
        const xta_xa_resource_t *this) {
        return this->dynamic; }



    /**
     * This call back method is invoked by a Transaction Manager when an
     * Application Program registers an XA Resource to a Transaction Manager.
     * The TM calls back the XA Resource to notify it has been registered.
     * @param[in,out] this : XA Resource object
     * @param[in] tx : XA Transaction
     * @return a reason code
     */    
    int xta_xa_resource_registered(xta_xa_resource_t *this,
                                   const xta_transaction_t *tx);


    
    /**
     * Starts work on behalf of a transaction branch specified in xid. If
     * @ref TMJOIN is specified, the start applies to joining a transaction
     * previously seen by the resource manager. If @ref TMRESUME is specified,
     * the start applies to resuming a suspended transaction specified in the
     * parameter xid. If neither @ref TMJOIN nor @ref TMRESUME is specified and
     * the transaction specified by xid has previously been seen by the
     * resource manager, the resource manager returns @ref XTA_RC_XAER_DUPID
     * error code.
     * @param[in,out] this : resource object
     * @param[in] xid : transaction identifier object
     * @param[in] flag : one of @ref TMNOFLAGS, @ref TMJOIN, or @ref TMRESUME
     * @return a reason code
     */
    int xta_xa_resource_start(xta_xa_resource_t *this,
                              const xta_xid_t *xid,
                              long flag);
    


    /**
     * Ends the work performed on behalf of a transaction branch. The resource
     * manager disassociates the XA resource from the transaction branch
     * specified and lets the transaction complete.
     * If @ref TMSUSPEND is specified in the flags, the transaction branch is
     * temporarily suspended in an incomplete state. The transaction context
     * is in a suspended state and must be resumed via the start method with
     * @ref TMRESUME specified.
     * If @ref TMFAIL is specified, the portion of work has failed. The
     * resource manager may mark the transaction as rollback-only.
     * If @ref TMSUCCESS is specified, the portion of work has completed
     * successfully.
     * @param[in,out] this : resource object
     * @param[in] xid : transaction identifier object
     * @param[in] flag : one of @ref TMSUCCESS, @ref TMFAIL, or @ref TMSUSPEND
     * @return a reason code
     */
    int xta_xa_resource_end(xta_xa_resource_t *this,
                            const xta_xid_t *xid,
                            long flag);

    

    /**
     * Ask the resource manager to prepare for a transaction commit of the
     * transaction specified in xid.
     * @param[in,out] this : resource object
     * @param[in] xid : transaction identifier object
     * @return a reason code
     */
    int xta_xa_resource_prepare(xta_xa_resource_t *this,
                                const xta_xid_t *xid);

    
    
    /**
     * Commits the global transaction specified by xid.
     * @param[in,out] this : resource object
     * @param[in] xid : transaction identifier object
     * @param[in] one_phase : if true, the resource manager should use a
     *                        one-phase commit protocol to commit the work
     *                        done on behalf of xid
     * @return a reason code
     */
    int xta_xa_resource_commit(xta_xa_resource_t *this,
                               const xta_xid_t *xid,
                               int one_phase);
    
    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* XTA_XA_RESOURCE_H */
