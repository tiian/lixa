/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef POSTGRESQL_XA_RESOURCE_H
# define POSTGRESQL_XA_RESOURCE_H



/* PostgreSQL front-end */
#include <libpq-fe.h>
/* XTA includes */
#include "xta_acquired_xa_resource.h"
/* LIXA includes */
#include "lixa_sw.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_XTA



/**
 * XTA PostgreSQL XA Resource data type
 */
typedef struct {
    union {
        xta_xa_resource_t                    xa_resource;
        xta_acquired_xa_resource_t  acquired_xa_resource;
    };
    /**
     * PostgreSQL connection handler
     */
    PGconn                         *connection;
    /**
     * Resource status, backward compatibility struct with legacy XA
     * PostgreSQL Resource Manager
     */
    struct lixa_sw_status_rm_s      lssr;
} xta_postgresql_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new object to represent a PostgreSQL XA resource
     * @param[in,out] connection to PostgreSQL already opened by the
     *                application program
     * @param[in] name : unique identifier of the resource
     * @param[in] open_info : unique description of the connection properties
     *                        like network name/IP address, port, user/schema,
     *                        etc. Only the first @ref MAXINFOSIZE characters
     *                        will be kept.
     * @return a new object or NULL in the event of error
     */
    xta_postgresql_xa_resource_t *xta_postgresql_xa_resource_new(
        PGconn *connection, const char *name, const char *open_info);

    

    /**
     * Delete an object that represent a PostgreSQL XA Resource
     * @param[in] xa_resource : PostgreSQL XA Resource
     */
    void xta_postgresql_xa_resource_delete(
        xta_postgresql_xa_resource_t *xa_resource);
    
    

    /**
     * Initialize the propeties of a PostgreSQL XA resource
     * @param[in,out] xa_resource : PostgreSQL XA Resource object
     * @param[in] connection : to PostgreSQL already opened by the application
     *            program
     * @param[in] name : unique identifier of the resource
     * @param[in] open_info : unique description of the connection properties
     *                        like network name/IP address, port, user/schema,
     *                        etc. Only the first @ref MAXINFOSIZE characters
     *                        will be kept.
     * @return a new object or NULL in the event of error
     */
    int xta_postgresql_xa_resource_init(
        xta_postgresql_xa_resource_t *xa_resource,
        PGconn *connection, const char *name, const char *open_info);

    

    /**
     * Clean the properties of a PostgreSQL XA Resource; this function
     * must be called after @ref xta_postgresql_xa_resource_init during
     * object deletion to avoid memory leaks
     * @param[in,out] xa_resource : PostgreSQL XA Resource object
     */
    void xta_postgresql_xa_resource_clean(
        xta_postgresql_xa_resource_t *xa_resource);

    
    
    /**
     * Close a Postgresql resource manager
     * @param[in,out] context : XTA resource context
     * @param[in] xa_info : null-terminated character string that may contain
     *                      instance-specific information for the resource
     *                      manager
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMNOFLAGS, future usage only
     * @return a XA return code
     */
    int xta_postgresql_xa_close(xta_xa_resource_t *context, char *xa_info,
                                int rmid, long flags);



    /**
     * Commit work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMONEPHASE or @ref TMNOFLAGS
     * @return a XA return code
     */
    int xta_postgresql_xa_commit(xta_xa_resource_t *context, const XID *xid,
                                 int rmid, long flags);



    /**
     * End work performed on behalf of a transaction branch
     * @param[in,out] context XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid an integer assigned by the transaction manager,
     *                 uniquely identifies the called resource manager
     *                 instance within the thread of control
     * @param[in] flags only @ref TMSUCCESS and @ref TMFAIL can be passed to
     *                  a Postgresql resource
     * @return a XA return code
     */
    int xta_postgresql_xa_end(xta_xa_resource_t *context, const XID *xid,
                              int rmid, long flags);



    /**
     * Forget about a heuristically completed transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMNOFLAGS, future usage only
     * @return a XA return code
     */
    int xta_postgresql_xa_forget(xta_xa_resource_t *context, const XID *xid,
                                 int rmid, long flags);



    /**
     * Open a PostgreSQL resource manager
     * @param[in,out] context : XTA resource context
     * @param[in] xa_info : null-terminated character string that may contain
     *                      instance-specific information for the resource
     *                      manager
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMNOFLAGS, future usage only
     * @return a XA return code
     */
    int xta_postgresql_xa_open(xta_xa_resource_t *context, char *xa_info,
                               int rmid, long flags);



    /**
     * Prepare to commit work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMNOFLAGS, future usage only
     * @return a XA return code
     */
    int xta_postgresql_xa_prepare(xta_xa_resource_t *context, const XID *xid,
                                  int rmid, long flags);



    /**
     * Obtain a list of prepared transaction branches from a resource manager
     * @param[in,out] context : XTA resource context
     * @param[in] xids : an array into which the resource manager places XIDs
     *                   for list of transaction branches that are currently
     *                   in a prepared or heuristically completed state
     * @param[in] count : the maximum number of XIDs that fit into that array
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMSTARTRSCAN, @ref TMENDRSCAN, @ref TMNOFLAGS
     * @return a XA return code
     */
    int xta_postgresql_xa_recover(xta_xa_resource_t *context,
                                  XID *xids, long count, int rmid, long flags);



    /**
     * Roll back work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMNOFLAGS, future usage only
     * @return a XA return code
     */
    int xta_postgresql_xa_rollback(xta_xa_resource_t *context, const XID *xid,
                                   int rmid, long flags);



    /**
     * Start work on behalf of a transaction branch
     * @param[in,out] context XTA resource context
     * @param[in] xid transaction identifier, XA spec
     * @param[in] rmid an integer assigned by the transaction manager,
     *                 uniquely identifies the called resource manager
     *                 instance within the thread of control
     * @param[in] flags only @ref TMNOFLAGS can be passed to a Postgresql
     *                  resource
     * @return a XA return code
     */
    int xta_postgresql_xa_start(xta_xa_resource_t *context, const XID *xid,
                                int rmid, long flags);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* POSTGRESQL_XA_RESOURCE_H */
