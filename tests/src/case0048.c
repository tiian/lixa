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

    LIXA_TRACE_INIT;

    lixa_xid_create_new(&info1.xid);

    char *xid = NULL;
    if (tx_xid_serialize(info1, &xid) != TX_OK) {
        fprintf(stderr, "%s| unable to serialize xid\n", argv[0]);
        return 1;
    }

    if (!xid) {
        fprintf(stderr, "%s| null xid\n", argv[0]);
        return 1;
    }

    printf("%s| serialized xid: %s\n", argv[0], xid);

    if (tx_xid_deserialize(&info2, xid) != TX_OK) {
        fprintf(stderr, "%s| unable to deserialize xid\n", argv[0]);
        return 1;
    }
    if (lixa_xid_compare(&(info1.xid), &(info2.xid))) {
        return 1;
    }

    free(xid);

    return 0;
}