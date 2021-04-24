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
#ifndef NATIVE_XA_RESOURCE_H
# define NATIVE_XA_RESOURCE_H



/* XTA includes */
#include "xta_xa_resource.h"
#include "xta_transaction_manager.h"



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
     * @param[in] name : the name assigned to the Resource Manager just for
     *                   debugging purposes
     * @param[in] switch_file : absolute path of the XA switch file that
     *                          points to xa_ functions for the resource
     *                          manager
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
        const char *name, const char *switch_file,
        const char *open_info, const char *close_info);

    

    /**
     * Create a new object to represent a native XA resource described inside
     * LIXA legacy configuration file "lixac.conf"
     * @param[in] rmid : Resource Manager unique identifier within the thread
     *                   of control. All the values that describe the
     *                   Resource Manager must be extracted from configuration
     *                   file (lixac.flom).
     * @param[in] config : legacy LIXA structure that contains all the
     *                     Resource Managers' configurations
     * @return a new object or NULL in the event of error
     */
    xta_native_xa_resource_t *xta_native_xa_resource_new_by_rmid(
        int rmid, const xta_transaction_manager_config_t *config);

    
    
    /**
     * Initialize the common properties of Native XA Resources
     * @param[in,out] xa_resource : Native XA Resource object
     * @param[in] rmid : Resource Manager unique identifier within the thread
     *                   of control. This identifier is even used to
     *                   control the behavior of the constructor: <BR>
     *                   if (rmid >= 0) : all the values that describe the
     *                   Resource Manager must be extracted from configuration
     *                   file (lixac.flom) and the following options will be
     *                   ignored (use NULL value for convenience) <BR>
     *                   if (rmid < 0) : create a new Resource Manager
     *                   description that's not specified in configuration file
     *                   (lixac.flom) and the following options can't be
     *                   NULL. This behavior is useful to generate
     *                   Native XA Resource dynamically at run time.
     * @param[in] config : legacy LIXA structure that contains all the
     *                     Resource Managers' configurations
     * @param[in] name : the name assigned to the Resource Manager just for
     *                   debugging purposes
     * @param[in] switch_file : absolute path of the XA switch file that
     *                          points to xa_ functions for the resource
     *                          manager
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
     * @return a reason code
     */
    int xta_native_xa_resource_init(
        xta_native_xa_resource_t *xa_resource,
        int rmid, const xta_transaction_manager_config_t *config,
        const char *name, const char *switch_file,
        const char *open_info, const char *close_info);
    
    
    
    /**
     * Delete an object that represent a native XA Resource
     * @param[in] xa_resource : native XA Resource
     */
    void xta_native_xa_resource_delete(xta_native_xa_resource_t *xa_resource);
    


    /**
     * Clean the properties of a Native XA Resource; this function must be
     * called after @ref xta_native_xa_resource_init during object disposition
     * to avoid memory leaks
     * @param[in,out] xa_resource : Native XA Resource object
     */
    void xta_native_xa_resource_clean(xta_native_xa_resource_t *xa_resource);

    

#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* NATIVE_XA_RESOURCE_H */
