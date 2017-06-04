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
#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include "lixa_trace.h"
#include "lixa_iface.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_CONFIG



void lixa_iface_reset(lixa_iface_t *iface)
{
    LIXA_TRACE(("lixa_iface_reset\n"));
    memset(iface, 0, sizeof(lixa_iface_t));
    return;
}



void lixa_iface_set_std(lixa_iface_t *iface, struct xa_switch_t *std)
{
    LIXA_TRACE(("lixa_iface_set_std\n"));
    iface->type = LIXA_IFACE_STD;
    iface->std = std;
    iface->context = NULL;
    return;
}



void lixa_iface_set_xta(lixa_iface_t *iface, const struct xta_iface_s *xta,
                        xta_xa_resource_t *context)
{
    LIXA_TRACE(("lixa_iface_set_xta\n"));
    iface->type = LIXA_IFACE_XTA;
    iface->xta = xta;
    iface->context = context;
    return;
}
