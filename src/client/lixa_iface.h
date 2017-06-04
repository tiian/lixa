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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
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
     * Returns the flags for the resource managed by an interface
     * @param[in] iface : reference to the interface object
     * @return flags for the resource managed by the interface
     */
    static inline long lixa_iface_get_flags(const lixa_iface_t *iface) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->flags;
        else
            return iface->xta->flags;
    }

    

    /**
     * This function implements some sort of polymorphism and call the correct
     * xa_open function
     * @param[in] iface : reference to the interface object
     */
    static inline int lixa_iface_xa_open(lixa_iface_t *iface, char *info,
                                         int rmid, long flags) {
        if (LIXA_IFACE_STD == iface->type)
            return iface->std->xa_open_entry(info,rmid,flags);
        else
            return iface->xta->xa_open_entry(iface->context,info,rmid);
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
