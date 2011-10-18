/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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



#include "xa.h"



/*
 * This is an external implementation of a partial XA interface for
 * MySQL.
 * The intent of this piece of software is allowing user to start using
 * LIXA with any recent version of MySQL.
 * A definitive XA interface should be implemented inside MySQL
 * client library.
 * Wrapping the standard interface, only the static registration can be
 * implemented.
 */



#include <lixa_trace.h>
#include <lixa_common_status.h>
#include <liblixamy.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



/*
 * This is MySQL specific: the xa_switch_t struct supplied is named xapqls
 * for XA static registration
 */
extern struct xa_switch_t xapqls;



/*
 * The function is exported and dynamically retrieved after the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &xapqls;
}
