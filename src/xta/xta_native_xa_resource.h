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
#ifndef NATIVE_XA_RESOURCE_H
# define NATIVE_XA_RESOURCE_H



#include <config.h>
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
 * XTA Native XA Resource data type
 */
typedef struct {
    union {
        xta_xa_resource_t  xa_resource;
    };
} xta_native_xa_resource_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create a new object to represent a native XA resource
     * @param[in] rmid Resource Manager unique identifier within the thread of
     *                 control
     * @param[in] open_info : a null-terminated character string that may
     *                        contain instance-specific information for the
     *                        resource manager when xa_open must be called
     *                        (use NULL if you want to use the
     *                        value passed by standard config)
     * @param[in] close_info : a null-terminated character string that may
     *                         contain instance-specific information for the
     *                         resource manager when xa_close must be called
     *                         (use NULL if you want to use
     *                         the value passed by standard config
     * @return a new object or NULL in the event of error
     */
    xta_native_xa_resource_t *xta_native_xa_resource_new(
        int rmid, const char *open_info, const char *close_info);


    
    /**
     * Initialize the common properties of Native XA Resources
     * @param[in,out] this : Native XA Resource object
     * @return a reason code
     */
    int xta_native_xa_resource_init(xta_native_xa_resource_t *this);
    
    
    
    /**
     * Delete an object that represent a native XA Resource
     * @param[in] this : native XA Resource
     */
    void xta_native_xa_resource_delete(
        xta_native_xa_resource_t *this);
    
    

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* NATIVE_XA_RESOURCE_H */
