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



#include "xa.h"



/*
 * This is an external implementation of a partial XA interface for
 * PostgreSQL.
 * The intent of this piece of software is allowing user to start using
 * LIXA with any recent version of PostgreSQL.
 * A definitive XA interface should be implemented inside PostgreSQL
 * client library.
 * Wrapping the standard interface, only the static registration can be
 * implemented.
 */



#include <lixa_trace.h>
#include <lixa_common_status.h>
#include <liblixapq.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA_SWITCH



/*
 * This is PostgreSQL specific: the xa_switch_t struct supplied is named xapqls
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
