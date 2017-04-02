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
 */
#ifndef XTA_H
# define XTA_H



#include "xta_errors.h"
#include "xta_resource.h"
#include "xta_transaction.h"
#include "xta_transaction_manager.h"
#include "xta_xid.h"



/*
 * Constants inherited from XA specification
 */

/**
 * No resource manager features selected
 */
#define TMNOFLAGS 0x00000000L
/**
 * Dissociates caller and marks transaction branch rollback-only
 */
#define TMFAIL 0x20000000L
/**
 * Caller is resuming association with suspended transaction branch
 */
#define TMRESUME 0x08000000L
/**
 * Dissociate caller from transaction branch
 */
#define TMSUCCESS 0x04000000L
/**
 * Caller is suspending, not ending, association
 */
#define TMSUSPEND 0x02000000L
/**
 * Caller is joining existing transaction branch
 */
#define TMJOIN 0x00200000L
/**
 * Caller intends to perform migration
 */
#define TMMIGRATE 0x00100000L



#endif /* XTA_H */
