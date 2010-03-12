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



#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif



#include <lixa_errors.h>
#include <lixa_common_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_STATUS



uuid_t lixa_xid_global_bqual;



#if MD5_DIGEST_LENGTH != SIZEOF_UUID_T
# error MD5 digest and uuid_t differs in length
#endif



void xid_set_global_bqual(const char *md5_digest_hex)
{
    int i;
    unsigned char *p = (unsigned char *)&lixa_xid_global_bqual;
    char tmp[3];
    tmp[2] = '\0';
    for (i=0; i<MD5_DIGEST_LENGTH; ++i) {
        /* working with words would be more efficient, but there might be some
           alignment issues on any platform if uuid_t is declared as an array
           of bytes (on Linux it's defined as unsigned char uuid_t[16] */
        tmp[0] = md5_digest_hex[i * 2];
        tmp[1] = md5_digest_hex[i * 2 + 1];
        p[i] = (unsigned char)strtoul(tmp, NULL, 16);
    }
}



void xid_create_new(XID *xid)
{
    uuid_t uuid_obj;
    char *gtrid, *bqual;
    
    xid->formatID = LIXA_XID_FORMAT_ID;
    xid->gtrid_length = sizeof(uuid_t);
    xid->bqual_length = sizeof(uuid_t);
    memset(xid->data, 0, XIDDATASIZE); /* this is not necessary, but... */
    uuid_generate(uuid_obj);
    memcpy(xid->data, uuid_obj, sizeof(uuid_t));
    memcpy(xid->data + sizeof(uuid_t), lixa_xid_global_bqual, sizeof(uuid_t));
#ifdef _TRACE
    gtrid = xid_get_gtrid_ascii(xid);
    bqual = xid_get_bqual_ascii(xid);
    if (NULL != gtrid && NULL != bqual)
        LIXA_TRACE(("xid_create_new: gtrid='%s'; bqual='%s'\n", gtrid, bqual));
    if (NULL != bqual) free(bqual);
    if (NULL != gtrid) free(gtrid);
#endif /* _TRACE */
}



char *xid_get_gtrid_ascii(const XID *xid)
{
    char *gtrid;
    if (NULL == (gtrid = (char *)malloc(2*sizeof(uuid_t)+4+1)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data, gtrid);
    return gtrid;
}



char *xid_get_bqual_ascii(const XID *xid)
{
    char *bqual;
    if (NULL == (bqual = (char *)malloc(2*sizeof(uuid_t)+4+1)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data + sizeof(uuid_t), bqual);
    return bqual;
}



char *xid_serialize(const XID *xid)
{
    char *ser;
    if (NULL == (ser = (char *)malloc(2*(2*sizeof(uuid_t)+4)+2)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data, ser);
    uuid_unparse((unsigned char *)xid->data + sizeof(uuid_t),
                 ser + (2*sizeof(uuid_t)+4+1));
    ser[2*sizeof(uuid_t)+4] = LIXA_XID_SEPARATOR;
    return ser;
}



int xid_deserialize(char *ser_xid, XID *xid)
{
    enum Exception { SEPARATOR_NOT_FOUND
                     , UUID_PARSE_ERROR1
                     , UUID_PARSE_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xid_deserialize\n"));
    TRY {
        uuid_t uuid_obj;
        
        /* check separator */
        if (LIXA_XID_SEPARATOR != ser_xid[2*sizeof(uuid_t)+4])
            THROW(SEPARATOR_NOT_FOUND);
        /* initialize the format */
        xid->formatID = LIXA_XID_FORMAT_ID;
        /* separates gtrid from bqual strings */
        ser_xid[2*sizeof(uuid_t)+4] = '\0';
        /* parse & store gtrid */
        LIXA_TRACE(("xid_deserialize: gtrid='%s'\n", ser_xid));
        if (0 != uuid_parse(ser_xid, uuid_obj))
            THROW(UUID_PARSE_ERROR1);
        memcpy(xid->data, uuid_obj, sizeof(uuid_t));
        xid->gtrid_length = sizeof(uuid_t);
        /* parse & store bqual */
        LIXA_TRACE(("xid_deserialize: bqual='%s'\n",
                    ser_xid + (2*sizeof(uuid_t)+4+1)));
        if (0 != uuid_parse(ser_xid + (2*sizeof(uuid_t)+4+1), uuid_obj))
            THROW(UUID_PARSE_ERROR2);
        memcpy(xid->data + sizeof(uuid_t), uuid_obj, sizeof(uuid_t));
        xid->bqual_length = sizeof(uuid_t);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SEPARATOR_NOT_FOUND:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case UUID_PARSE_ERROR1:
            case UUID_PARSE_ERROR2:
                ret_cod = LIXA_RC_UUID_PARSE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore original separator */
        if (SEPARATOR_NOT_FOUND < ret_cod)
            ser_xid[2*sizeof(uuid_t)+4] = LIXA_XID_SEPARATOR;
    } /* TRY-CATCH */
    LIXA_TRACE(("xid_deserialize/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



