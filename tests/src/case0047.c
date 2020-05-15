/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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

/* LIXA private library for MySQL */
#include <liblixamy.h>



/*
 * This case test is specifically designed to test XID serialization and
 * deserialization for MySQL
 */



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_COMMON_XID



int main(int argc, char *argv[])
{
    lixa_my_ser_xid_t lmsx = "'db92e9410ef84504b174da57ec216e3e','1d3fe851993cd58d220e64772e6c21e2',1279875137";
    lixa_my_ser_xid_t lmsx2;
    const char *formatID = "1279875137";
    const char *gtrid_length = "32";
    const char *bqual_length = "32";
    const char *data = "db92e9410ef84504b174da57ec216e3e1d3fe851993cd58d220e64772e6c21e2";
    XID xid;

    LIXA_TRACE_INIT ;
    LIXA_TRACE(("Deserializing: formatID='%s', gtrid_length='%s', "
                "bqual_length='%s', data='%s'\n",
                formatID, gtrid_length, bqual_length, data));
    if (!lixa_my_xid_deserialize(&xid, formatID, gtrid_length, bqual_length,
                                 data)) {
        LIXA_TRACE(("lixa_my_xid_deserialized returned FALSE\n"));
        return 1;
    }
    LIXA_TRACE_HEX_DATA("Deserialized XID is: ",
                        (const byte_t *)&xid, sizeof(xid));
    LIXA_TRACE(("Serializing XID obtained by deserialization\n"));
    if (!lixa_my_xid_serialize(&xid, lmsx2)) {
        LIXA_TRACE(("lixa_my_xid_serialized returned FALSE\n"));
        return 1;
    }
    LIXA_TRACE(("Serialized XID is %s\n", lmsx2));
    LIXA_TRACE(("Serialized XID should be %s\n", lmsx));
    if (strcmp(lmsx2, lmsx)) {
        LIXA_TRACE(("Serialized XIDs differ... this is an error!\n"));
        return 1;
    }
    
    return 0;
}
