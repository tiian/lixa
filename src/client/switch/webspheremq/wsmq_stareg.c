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



#include "xa.h"



/*
 * This is IBM WebSphere MQ specific
 */
extern struct xa_switch_t MQRMIXASwitch;



/*
 * The function is exported and dynamically retrieved after the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &MQRMIXASwitch;
}
