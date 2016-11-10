/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <stdio.h>
#include <stdlib.h>

#include <tx.h>
#include <lixa_xid.h>

int main(int argc, char *argv[]) {
    TXINFO info1, info2;

    lixa_xid_create_new(&info1.xid);

    char *xid;
    if (tx_xid_serialize(info1, xid) != TX_OK) {
        return 1;
    }
    if (!xid) {
        return 1;
    }

    if (tx_xid_deserialize(&info2, xid) != TX_OK) {
        return 1;
    }
    if (lixa_xid_compare(&(info1.xid), &(info2.xid))) {
        return 1;
    }

    return 0;
}