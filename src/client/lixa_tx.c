/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif


#include <lixa_tx.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



void xid_create_new(XID *xid)
{
    uuid_t uuid_obj;
    char *gtrid, *bqual;
    
    xid->formatID = LIXA_XID_FORMAT_ID;
    xid->gtrid_length = 2 * sizeof(uuid_t);
    xid->bqual_length = sizeof(uuid_t);
    memset(xid->data, 0, XIDDATASIZE); /* this is not necessary, but... */
    uuid_generate_time(uuid_obj);
    memcpy(xid->data, uuid_obj, sizeof(uuid_t));
    uuid_generate_random(uuid_obj);
    memcpy(xid->data + sizeof(uuid_t), uuid_obj, sizeof(uuid_t));
    uuid_generate(uuid_obj);
    memcpy(xid->data + 2 * sizeof(uuid_t), uuid_obj, sizeof(uuid_t));    
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
    if (NULL == (gtrid = (char *)malloc(2*(2*sizeof(uuid_t)+4)+2)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data, gtrid);
    gtrid[2*sizeof(uuid_t)+4] = '+';
    uuid_unparse((unsigned char *)xid->data + sizeof(uuid_t),
                 gtrid + 2 * sizeof(uuid_t) + 5);
    return gtrid;
}



char *xid_get_bqual_ascii(const XID *xid)
{
    char *bqual;
    if (NULL == (bqual = (char *)malloc(2*sizeof(uuid_t)+5)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data + 2 * sizeof(uuid_t), bqual);
    return bqual;
    
}
