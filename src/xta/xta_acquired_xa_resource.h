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
#ifndef XTA_ACQUIRED_XA_RESOURCE_H
# define XTA_ACQUIRED_XA_RESOURCE_H



#include <config.h>



/* system includes */
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
/* LIXA includes */
#include "lixa_trace.h"
/* XTA includes */
#include "xta_xa_resource.h"
#include "xta_last_operation.h"
/* XA include */
#include "xa.h"



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
    union {
        xta_xa_resource_t           xa_resource;
    };
    gchar      *name;
    char        open_info[MAXINFOSIZE];
} xta_acquired_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize the common properties of Acquired XA Resources
     * @param[in,out] this : Acquired XA Resource object
     * @param[in] name : unique identifier of the resource
     * @param[in] open_info : unique description of the connection properties
     *                        like network name/IP address, port, user/schema,
     *                        etc. Only the first @ref MAXINFOSIZE characters
     *                        will be kept.
     * @return a reason code
     */
    int xta_acquired_xa_resource_init(xta_acquired_xa_resource_t *this,
                                      const char *name,
                                      const char *open_info);
    
    

    /**
     * Clean the common properties of Acquired XA Resources; this function
     * must be called after @ref xta_acquired_xa_resource_init during
     * object deletion to avoid memory leaks
     * @param[in,out] this : Acquired XA Resource object
     */
    void xta_acquired_xa_resource_clean(xta_acquired_xa_resource_t *this);


    
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
