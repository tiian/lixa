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
#ifndef MYSQL_XA_RESOURCE_H
# define MYSQL_XA_RESOURCE_H



#include <config.h>


/* MySQL front-end */
#include <mysql.h>
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
 * XTA MySQL XA Resource data type
 */
typedef struct {
    union {
        xta_xa_resource_t           xa_resource;
        xta_acquired_xa_resource_t  acquired_xa_resource;
    };
} xta_mysql_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new object to represent a MySQL XA resource
     * @param[in] connection to MySQL already opened by the application
     *            program
     * @param[in] name : unique identifier of the resource
     * @param[in] open_info : unique description of the connection properties
     *                        like network name/IP address, port, user/schema,
     *                        etc. Only the first @ref MAXINFOSIZE characters
     *                        will be kept.
     * @return a new object or NULL in the event of error
     */
    xta_mysql_xa_resource_t *xta_mysql_xa_resource_new(MYSQL *connection,
                                                       const char *name,
                                                       const char *open_info);

    

    /**
     * Delete an object that represent a MySQL XA Resource
     * @param[in] this : MySQL XA Resource
     */
    void xta_mysql_xa_resource_delete(xta_mysql_xa_resource_t *this);
    
    

    /**
     * Initialize the propeties of a MySQL XA resource
     * @param[in,out] this : MySQL XA Resource object
     * @param[in] connection : to MySQL already opened by the application
     *            program
     * @param[in] name : unique identifier of the resource
     * @param[in] open_info : unique description of the connection properties
     *                        like network name/IP address, port, user/schema,
     *                        etc. Only the first @ref MAXINFOSIZE characters
     *                        will be kept.
     * @return a reason code
     */
    int xta_mysql_xa_resource_init(xta_mysql_xa_resource_t *this,
                                   MYSQL *connection,
                                   const char *name, const char *open_info);



    /**
     * Clean the properties of a MySQL XA Resource; this function
     * must be called after @ref xta_mysql_xa_resource_init during
     * object deletion to avoid memory leaks
     * @param[in,out] this : MySQL XA Resource object
     */
    void xta_mysql_xa_resource_clean(xta_mysql_xa_resource_t *this);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* MYSQL_XA_RESOURCE_H */
