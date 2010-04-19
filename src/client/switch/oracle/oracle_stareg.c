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



#include "xa.h"



/*
 * This is Oracle specific: the xa_switch_t struct supplied is named xaosw
 * for XA static registration
 */
extern struct xa_switch_t xaosw;



/*
 * The function is exported and dynamically retrieved afted the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &xaosw;
}
