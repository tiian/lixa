/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <config.h>



#include <lixa_trace.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



int ax_reg(int rmid, XID *xid, long flags)
{
    /* @@@ dummy implementation to be fixed: bug 2913849 */
    LIXA_TRACE(("ax_reg: rmid = %d, xid, flags = %ld\n", rmid, flags));
    xid->formatID = -1;
    return TM_OK;
}



int ax_unreg(int rmid, long flags)
{
    /* @@@ dummy implementation to be fixed: bug 2913849 */
    LIXA_TRACE(("ax_unreg: rmid = %d, flags = %ld\n", rmid, flags));
    return TM_OK;
}
