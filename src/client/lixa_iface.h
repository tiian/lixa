/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_IFACE_H
# define LIXA_IFACE_H



#include "xa.h"
#include "xta_iface.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_CONFIG



/**
 * Standard XA interface, as in xa.h file
 */
#define LIXA_IFACE_STD     0
/**
 * XTA interface, as in xta_iface.h file
 */
#define LIXA_IFACE_XTA     1



/*
 * This type is a declaration only statement: the real type is defined inside
 * XA Resource header file. Here we just need to store a pointer to
 * a XA Resource inside LIXA interface.
 */
typedef struct xta_xa_resource_s xta_xa_resource_t;



/**
 * This interface is a wrapper used to pass different interfaces in absence
 * of polymorphism
 */
typedef struct lixa_iface_s {
    /**
     * Interface type: <br>
     * @ref LIXA_IFACE_STD : standard XA interface as provided by some
     *                       native Resource Managers; see std field <br>
     * @ref LIXA_IFACE_XTA : XTA style interface as provided by XTA resources;
     *                       see xta field
     */
    int                       type;
    union {
        /**
         * standard XA Resource Manager interface
         */
        struct xa_switch_t   *std;
        /**
         * XTA Resource interface
         */
        const struct xta_iface_s   *xta;
    };
    /**
     * Reference to XTA Resource context: NULL for standard XA Resources
     */
    xta_xa_resource_t        *context;
} lixa_iface_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    

    /**
     * Reset a Resource Manager interface
     * @param[out] iface : reference to the object that must be resetted
     */
    void lixa_iface_reset(lixa_iface_t *iface);
    
    

    /**
     * Set a standard LIXA interface for Resource Manager
     * @param[out] iface : reference to the object that must be set
     * @param[in] std : reference to XA standard function struct
     */
    void lixa_iface_set_std(lixa_iface_t *iface, struct xa_switch_t *std);



    /**
     * Set an XTA interface for the Resource Manager
     * @param[out] iface : reference to the object that must be set
     * @param[in] xta : reference to XTA interface struct
     * @param[in] context : reference to the resource context that will be
     *                      passed to functions
     */
    void lixa_iface_set_xta(lixa_iface_t *iface, const struct xta_iface_s *xta,
                            xta_xa_resource_t *context);



    /**
     * Returns the XA flags for the resource managed by an interface
     * @param[in] iface : reference to the interface object
     * @return XA flags for the resource managed by the interface
     */
    static inline long lixa_iface_get_flags(const lixa_iface_t *iface) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->flags;
        else
            return iface->xta->flags;
    }


    
    /**
     * Returns the XA name for the resource managed by an interface
     * @param[in] iface : reference to the interface object
     * @return XA name for the resource managed by the interface
     */
    static inline const char *lixa_iface_get_name(const lixa_iface_t *iface) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->name;
        else
            return iface->xta->name;
    }

    

    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_open function
     * @param[in] iface : reference to the interface object
     * @param[in] info : open string as documented in XA specification
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_open(lixa_iface_t *iface, char *info,
                                         int rmid, long flags) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->xa_open_entry(info, rmid, flags);
        else
            return iface->xta->xa_open_entry(iface->context, info,
                                             rmid, flags);
    }



    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_close function
     * @param[in] iface : reference to the interface object
     * @param[in] info : close string as documented in XA specification
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_close(lixa_iface_t *iface, char *info,
                                          int rmid, long flags) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->xa_close_entry(info, rmid, flags);
        else
            return iface->xta->xa_close_entry(iface->context, info,
                                              rmid, flags);
    }



    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_start function
     * @param[in] iface : reference to the interface object
     * @param[in] xid : transaction id, XA style
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_start(lixa_iface_t *iface, const XID *xid,
                                          int rmid, long flags) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->xa_start_entry((XID *)xid, rmid, flags);
        else
            return iface->xta->xa_start_entry(iface->context, xid,
                                              rmid, flags);
    }


    
    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_commit function
     * @param[in] iface : reference to the interface object
     * @param[in] xid : transaction id, XA style
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_commit(lixa_iface_t *iface,
        const XID *xid, int rmid, long flags) {
    if (LIXA_IFACE_STD == iface->type)
        return iface->std->xa_commit_entry((XID *)xid,rmid,flags);
        else
            return iface->xta->xa_commit_entry(
                iface->context, xid, rmid, flags);
    }


    
    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_rollback function
     * @param[in] iface : reference to the interface object
     * @param[in] xid : transaction id, XA style
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_rollback(lixa_iface_t *iface,
                                             const XID *xid, int rmid,
                                             long flags) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->xa_rollback_entry((XID *)xid, rmid, flags);
        else
            return iface->xta->xa_rollback_entry(
                iface->context, xid, rmid, flags);
    }


    
    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_recover function
     * @param[in] iface : reference to the interface object
     * @param[out] xids : an array into which the resource manager places
     *                    XIDs for these transactions
     * @param[in] count : maximum number of XIDs that fit into that array
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return >= 0, total number of XIDs it returned in *xids <br>
     *         an XA error code as documented in XA specification
     */
    static inline int lixa_iface_xa_recover(lixa_iface_t *iface,
                                            XID *xids, long count,
                                            int rmid, long flags) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->xa_recover_entry(
                (XID *)xids,count,rmid,flags);
        else
            return iface->xta->xa_recover_entry(
                iface->context,xids,count,rmid,flags);
    }


    
    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_forget function
     * @param[in] iface : reference to the interface object
     * @param[in] xid : transaction id, XA style
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_forget(lixa_iface_t *iface,
        const XID *xid, int rmid, long flags) {
    if (LIXA_IFACE_STD == iface->type)
        return iface->std->xa_forget_entry((XID *)xid, rmid, flags);
        else
            return iface->xta->xa_forget_entry(
                iface->context, xid, rmid, flags);
    }


    
    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_end function
     * @param[in] iface : reference to the interface object
     * @param[in] xid : transaction id, XA style
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_end(lixa_iface_t *iface,
        const XID *xid, int rmid, long flags) {
    if (LIXA_IFACE_STD == iface->type)
        return iface->std->xa_end_entry((XID *)xid, rmid, flags);
        else
            return iface->xta->xa_end_entry(iface->context, xid, rmid, flags);
    }


    
    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_prepare function
     * @param[in] iface : reference to the interface object
     * @param[in] xid : transaction id, XA style
     * @param[in] rmid : resource manager id as documented in XA specification
     * @param[in] flags : as documented in XA specification
     * @return an XA return code as documented in XA specification
     */
    static inline int lixa_iface_xa_prepare(lixa_iface_t *iface,
        const XID *xid, int rmid, long flags) {
    if (LIXA_IFACE_STD == iface->type)
        return iface->std->xa_prepare_entry((XID *)xid, rmid, flags);
        else
            return iface->xta->xa_prepare_entry(
                iface->context, xid, rmid, flags);
    }


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_IFACE_H */
