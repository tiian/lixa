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
#ifndef XTA_RESOURCE_H
# define XTA_RESOURCE_H



/* LIXA includes */
#include "lixa_trace.h"
/* XTA includes */
#include "xta_xid.h"
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
} xta_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new Resource object
     * @return a new resource object or NULL in the event of an error occurred
     */
    xta_resource_t *xta_resource_new(void);



    /**
     * Delete a Resource object
     * @param[in] resource object to delete
     */
    void xta_resource_delete(xta_resource_t *resource);



    /**
     * Starts work on behalf of a transaction branch specified in xid. If
     * @ref TMJOIN is specified, the start applies to joining a transaction
     * previously seen by the resource manager. If @ref TMRESUME is specified,
     * the start applies to resuming a suspended transaction specified in the
     * parameter xid. If neither @ref TMJOIN nor @ref TMRESUME is specified and
     * the transaction specified by xid has previously been seen by the
     * resource manager, the resource manager returns @ref XTA_RC_XAER_DUPID
     * error code.
     * @param[in,out] resource object
     * @param[in] xid : transaction identifier object
     * @param[in] flag : one of @ref TMNOFLAGS, @ref TMJOIN, or @ref TMRESUME
     * @return a reason code
     */
    int xta_resource_start(xta_resource_t *resource,
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
     * @param[in,out] resource object
     * @param[in] xid : transaction identifier object
     * @param[in] flag : one of @ref TMSUCCESS, @ref TMFAIL, or @ref TMSUSPEND
     * @return a reason code
     */
    int xta_resource_end(xta_resource_t *resource,
                         const xta_xid_t *xid,
                         long flag);

    

    /**
     * Ask the resource manager to prepare for a transaction commit of the
     * transaction specified in xid.
     * @param[in,out] resource object
     * @param[in] xid : transaction identifier object
     * @return a reason code
     */
    int xta_resource_prepare(xta_resource_t *resource,
        const xta_xid_t *xid);

    
    
    /**
     * Commits the global transaction specified by xid.
     * @param[in,out] resource object
     * @param[in] xid : transaction identifier object
     * @param[in] one_phase : if true, the resource manager should use a
     *                        one-phase commit protocol to commit the work
     *                        done on behalf of xid
     * @return a reason code
     */
    int xta_resource_commit(xta_resource_t *resource,
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



#endif /* XTA_RESOURCE_H */
