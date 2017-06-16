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
#ifndef POSTGRESQL_XA_RESOURCE_H
# define POSTGRESQL_XA_RESOURCE_H



#include <config.h>


/* PostgreSQL front-end */
#ifdef HAVE_POSTGRESQL
# include <libpq-fe.h>
#endif
/* XTA includes */
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
} xta_postgresql_xa_resource_t;



/**
 * Interface with XA function pointers
 */
const struct xta_iface_s xta_postgresql_iface;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new object to represent a PostgreSQL XA resource
     * @param[in] connection to PostgreSQL already opened by the application
     *            program
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
     * @param[in] this : PostgreSQL XA Resource
     */
    void xta_postgresql_xa_resource_delete(xta_postgresql_xa_resource_t *this);
    
    

    /**
     * Initialize the propeties of a PostgreSQL XA resource
     * @param[in,out] this : PostgreSQL XA Resource object
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
        xta_postgresql_xa_resource_t *this,
        PGconn *connection, const char *name, const char *open_info);

    

    /**
     * Clean the properties of a PostgreSQL XA Resource; this function
     * must be called after @ref xta_postgresql_xa_resource_init during
     * object deletion to avoid memory leaks
     * @param[in,out] this : PostgreSQL XA Resource object
     */
    void xta_postgresql_xa_resource_clean(xta_postgresql_xa_resource_t *this);

    
    
    /**
     * Open a PostgreSQL resource manager
     * @param[in,out] context : XTA resource context
     * @param[in] xa_info : null-terminated character string that may contain
     *                      instance-specific information for the resource
     *                      manager
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @return a XA return code
     */
    int xta_postgresql_xa_open(xta_xa_resource_t *context, char *xa_info,
                               int rmid);



    /**
     * Close a Postgresql resource manager
     * @param[in,out] context : XTA resource context
     * @param[in] xa_info : null-terminated character string that may contain
     *                      instance-specific information for the resource
     *                      manager
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @return a XA return code
     */
    int xta_postgresql_xa_close(xta_xa_resource_t *context, char *xa_info,
                                int rmid);



    /**
     * Start work on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : only @ref TMNOFLAGS can be passed to a Postgresql
     *                    resource
     * @return a XA return code
     */
    int xta_postgresql_xa_start(xta_xa_resource_t *context, int rmid,
                                long flags);



    /**
     * End work performed on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : only @ref TMSUCCESS can be passed to a Postgresql
     *                    resource
     * @return a XA return code
     */
    int xta_postgresql_xa_end(xta_xa_resource_t *context,
                              int rmid, long flags);



    /**
     * Roll back work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @return a XA return code
     */
    int xta_postgresql_xa_rollback(xta_xa_resource_t *context, int rmid);



    /**
     * Prepare to commit work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @return a XA return code
     */
    int xta_postgresql_xa_prepare(xta_xa_resource_t *context, int rmid);



    /**
     * Commit work done on behalf of a transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @param[in] flags : @ref TMONEPHASE or @ref TMNOFLAGS
     * @return a XA return code
     */
    int xta_postgresql_xa_commit(xta_xa_resource_t *context,
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
     * Forget about a heuristically completed transaction branch
     * @param[in,out] context : XTA resource context
     * @param[in] rmid : an integer assigned by the transaction manager,
     *                   uniquely identifies the called resource manager
     *                   instance within the thread of control
     * @return a XA return code
     */
    int xta_postgresql_xa_forget(xta_xa_resource_t *context, int rmid);



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
