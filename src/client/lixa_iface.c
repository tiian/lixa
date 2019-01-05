/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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


