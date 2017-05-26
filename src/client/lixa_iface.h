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



/**
 * Standard XA interface, as in xa.h file
 */
#define LIXA_IFACE_STD     0
/**
 * XTA interface, as in xta_iface.h file
 */
#define LIXA_IFACE_XTA     1



/**
 * This interface is a wrapper used to pass different interfaces in absence
 * of polymorphism
 */
struct lixa_iface_s {
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
        struct xta_iface_s   *xta;
    };
};



#endif /* LIXA_IFACE_H */
