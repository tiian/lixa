/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef XTA_ACQUIRED_XA_RESOURCE_H
# define XTA_ACQUIRED_XA_RESOURCE_H



/* XTA includes */
#include "xta_xa_resource.h"
#include "xta_iface.h"
/* XA include */
#include "xa.h"



/**
 * XTA Transaction data type
 */
typedef struct {
    union {
        xta_xa_resource_t           xa_resource;
    };
    /**
     * XTA interface for XA functions
     */
    const struct xta_iface_s       *iface;
} xta_acquired_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize the common properties of Acquired XA Resources
     * @param[in,out] xa_resource : Acquired XA Resource object
     * @param[in] iface : interface with XA function pointers
     * @param[in] name : unique identifier of the resource
     * @param[in] open_info : unique description of the connection properties
     *                        like network name/IP address, port, user/schema,
     *                        etc. Only the first @ref MAXINFOSIZE characters
     *                        will be kept.
     * @return a reason code
     */
    int xta_acquired_xa_resource_init(xta_acquired_xa_resource_t *xa_resource,
                                      const struct xta_iface_s *iface,
                                      const char *name,
                                      const char *open_info);
    
    

    /**
     * Clean the common properties of Acquired XA Resources; this function
     * must be called after @ref xta_acquired_xa_resource_init during
     * object deletion to avoid memory leaks
     * @param[in,out] xa_resource : Acquired XA Resource object
     */
    void xta_acquired_xa_resource_clean(
        xta_acquired_xa_resource_t *xa_resource);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* XTA_RESOURCE_H */
