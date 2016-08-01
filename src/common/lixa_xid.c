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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
 */
#include <config.h>



#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_REGEX_H
# include <regex.h>
#endif



#include <lixa_config.h>
#include <lixa_trace.h>
#include <lixa_xid.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_XID



const long LIXA_XID_FORMAT_ID = 0x4c495841;



uuid_t lixa_xid_global_bqual;



#if MD5_DIGEST_LENGTH != SIZEOF_UUID_T
# error MD5 digest and uuid_t differs in length
#endif



void lixa_xid_set_global_bqual(const char *md5_digest_hex)
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



int lixa_xid_bqual_is_global(const XID *xid)
{
    if (xid->bqual_length != MD5_DIGEST_LENGTH)
        return FALSE;
    return !memcmp(xid->data + xid->gtrid_length, &lixa_xid_global_bqual,
                   MD5_DIGEST_LENGTH);
}



void lixa_xid_create_new(XID *xid)
{
    uuid_t uuid_obj;
    
    xid->formatID = LIXA_XID_FORMAT_ID;
    xid->gtrid_length = sizeof(uuid_t);
    xid->bqual_length = sizeof(uuid_t);
    memset(xid->data, 0, XIDDATASIZE); /* this is not necessary, but... */
    uuid_generate(uuid_obj);
    memcpy(xid->data, uuid_obj, sizeof(uuid_t));
    memcpy(xid->data + sizeof(uuid_t), lixa_xid_global_bqual, sizeof(uuid_t));
#ifdef LIXA_DEBUG
    {
    char *gtrid, *bqual;
    gtrid = lixa_xid_get_gtrid_ascii(xid);
    bqual = lixa_xid_get_bqual_ascii(xid);
    if (NULL != gtrid && NULL != bqual)
        LIXA_TRACE(("lixa_xid_create_new: gtrid='%s'; bqual='%s'\n",
                    gtrid, bqual));
    if (NULL != bqual) free(bqual);
    if (NULL != gtrid) free(gtrid);
    }
#endif /* LIXA_DEBUG */
}



char *lixa_xid_get_gtrid_ascii(const XID *xid)
{
    char *gtrid;
    if (NULL == (gtrid = (char *)malloc(2*sizeof(uuid_t)+4+1)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data, gtrid);
    return gtrid;
}



char *lixa_xid_get_bqual_ascii(const XID *xid)
{
    char *bqual;
    if (NULL == (bqual = (char *)malloc(2*sizeof(uuid_t)+4+1)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data + sizeof(uuid_t), bqual);
    return bqual;
}



void lixa_xid_formatid(lixa_ser_xid_t lsx)
{
    /* serialize LIXA formatID */
    sprintf(lsx, "%ld", LIXA_XID_FORMAT_ID); 
}



int lixa_xid_serialize(const XID *xid, lixa_ser_xid_t lsx)
{
    int i = 0, j = 0;
    byte_t *p;
    long len = LIXA_SERIALIZED_LONG_INT /* formatID */ +
        1 /* '.' separator */ +
        xid->gtrid_length*2 /* gtrid */ +
        1 /* '.' separator */ +
        xid->bqual_length*2 /* bqual */ +
        1 /* '\0' terminator */ ;
    /* check the XID can be serialized for PostgreSQL by this routine */
    if (len > sizeof(lixa_ser_xid_t)) {
        LIXA_TRACE(("lixa_xid_serialize: xid can not be serialized "
                    "because it would need %ld bytes instead of "
                    SIZE_T_FORMAT "\n",
                    len, sizeof(lixa_ser_xid_t)));
        return FALSE;
    }
    /* serialize formatID and put the first separator */
    sprintf(lsx, "%ld%c", xid->formatID, LIXA_XID_SEPARATOR);

    /* serialize gtrid */
    j = strlen(lsx);
    p = (byte_t *)&(xid->data[0]);
    for (i=0; i<xid->gtrid_length; ++i) {
        sprintf(lsx+j, "%2.2x", p[i]);
        j += 2;
    }
    /* put the second separator */
    *(lsx+j) = LIXA_XID_SEPARATOR;
    j += 1;
    p = (byte_t *)&(xid->data[xid->gtrid_length]);
    for (i=0; i<xid->bqual_length; ++i) {
        sprintf(lsx+j, "%2.2x", p[i]);
        j += 2;
    }
    *(lsx+j) = '\0';
    
    LIXA_TRACE(("lixa_xid_serialize: '%s'\n", lsx));
    return TRUE;
}



int lixa_xid_deserialize(XID *xid, lixa_ser_xid_t lsx)
{
    enum Exception { REGCOMP_ERROR
                     , REGEXEC_ERROR
                     , SEPARATOR1
                     , INVALID_CHAR1
                     , SEPARATOR2
                     , INVALID_CHAR2
                     , TERMINATOR
                     , NONE } excp;
    int ret_cod = FALSE;
    
    LIXA_TRACE(("lixa_xid_deserialize\n"));
    TRY {
        long i;
        char *p = NULL, *q = NULL;
        char tmp[LIXA_SERIALIZED_LONG_INT+1];
        unsigned long l = 0;
        unsigned int b = 0;
        regex_t preg;
        int reg_error;
        char reg_errbuf[200];

        /* check the string is well formed using a regular expression */
        /* compile regular expression */
        reg_error = regcomp(
            &preg, "^([-]?[[:digit:]])+\\.([[:xdigit:]]{2})*\\.([[:xdigit:]]{2})*$",
            REG_EXTENDED|REG_NOSUB|REG_NEWLINE);
        if (0 != reg_error) {
            regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_xid_deserialize: regcomp returned %d ('%s') "
                        "instead of 0\n", reg_error, reg_errbuf));
            THROW(REGCOMP_ERROR);
        }
        /* check string against regular expression */
        reg_error = regexec(&preg, lsx, 0, NULL, 0);
        if (0 != reg_error) {
            regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_xid_deserialize: regexec returned %d ('%s') "
                        "instead of 0 for string '%s'\n",
                        reg_error, reg_errbuf, lsx));
            regfree(&preg);
            THROW(REGEXEC_ERROR);
        }
        /* free compiled regular expression */
        regfree(&preg);

        /* discover first separator */
        q = strchr(lsx, LIXA_XID_SEPARATOR);
        if (NULL == q) {
            LIXA_TRACE(("lixa_xid_deserialize: '%s' does "
                        "not contain the separator '%c'\n", lsx, 
                        LIXA_XID_SEPARATOR));
            THROW(SEPARATOR1);
        }
        
        i = q-lsx;
        strncpy(tmp, lsx, i);
        tmp[i] = '\0';
        sscanf(tmp, "%ld", &l);
        xid->formatID = l;
        /* prepare cursors */
        p = q+1;
        i = 0;
        tmp[2] = '\0';
        /* deserialize gtrid */
        while (p < lsx+sizeof(lixa_ser_xid_t)-1 && i<XIDDATASIZE) {
            q = p+1;
            if (((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f')) &&
                ((*q >= '0' && *q <= '9') || (*q >= 'a' && *q <= 'f'))) {
                tmp[0] = *p;
                tmp[1] = *q;
                sscanf(tmp, "%x", &b);
                xid->data[i++] = b;
            } else if (*p == LIXA_XID_SEPARATOR) {
                break;
            } else {
                LIXA_TRACE(("lixa_xid_deserialize: '%s' invalid "
                            "characters found in gtrid part\n", p));
                THROW(INVALID_CHAR1);
            }
            p += 2;
        }
        /* check the separator between gtrid and bqual */
        if (*p != LIXA_XID_SEPARATOR) {
            LIXA_TRACE(("lixa_xid_deserialize: '%c' is not the "
                        "separator\n", *p));
            THROW(SEPARATOR2);
        }
        xid->gtrid_length = i;
        p++;
        /* deserialize bqual */
        while (p < lsx+sizeof(lixa_ser_xid_t)-1 && i<XIDDATASIZE) {
            q = p+1;
            if (((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f')) &&
                ((*q >= '0' && *q <= '9') || (*q >= 'a' && *q <= 'f'))) {
                tmp[0] = *p;
                tmp[1] = *q;
                sscanf(tmp, "%x", &b);
                xid->data[i++] = b;
            } else if (*p == '\0') {
                break;
            } else {
                LIXA_TRACE(("lixa_xid_deserialize: '%s' invalid "
                            "characters found in bqual part\n", p));
                THROW(INVALID_CHAR2);
            }
            p += 2;
        }
        /* check the terminator */
        if (*p != '\0') {
            LIXA_TRACE(("lixa_xid_deserialize: '%c' is not the "
                        "terminator\n", *p));
            THROW(TERMINATOR);
        }
        xid->bqual_length = i - xid->gtrid_length;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REGCOMP_ERROR:
            case REGEXEC_ERROR:
            case SEPARATOR1:
            case INVALID_CHAR1:
            case SEPARATOR2:
            case INVALID_CHAR2:
            case TERMINATOR:
                ret_cod = FALSE;
                break;
            case NONE:
                ret_cod = TRUE;
                break;
            default:
                ret_cod = FALSE;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_xid_deserialize/excp=%d/ret_cod=%d\n",
                excp, ret_cod));
    return ret_cod;
}



int lixa_xid_compare(const XID *a, const XID *b)
{
    int result;

    LIXA_TRACE(("lixa_xid_compare: a->formatID=%ld, b->formatID=%ld, "
                "a->gtrid_length=%ld, b->gtrid_length=%ld, "
                "a->bqual_length=%ld, b->bqual_length=%ld\n",
                a->formatID, b->formatID,
                a->gtrid_length, b->gtrid_length,
                a->bqual_length, b->bqual_length));
    if (a->formatID < b->formatID)
        return -1;
    else if (a->formatID > b->formatID)
        return 1;
    if (a->gtrid_length < b->gtrid_length)
        return -1;
    else if (a->gtrid_length > b->gtrid_length)
        return 1;
    if (a->bqual_length < b->bqual_length)
        return -1;
    else if (a->bqual_length > b->bqual_length)
        return 1;
    if (0 != (result = memcmp(a->data, b->data, a->gtrid_length))) {
        LIXA_TRACE(("lixa_xid_compare: a->data is ",
                    (const byte_t *)a->data, a->gtrid_length));
        LIXA_TRACE(("lixa_xid_compare: b->data is ",
                    (const byte_t *)b->data, a->gtrid_length));
        return result;
    }
    if (0 != (result = memcmp(a->data + a->gtrid_length,
                              b->data + b->gtrid_length,
                              a->bqual_length))) {
        LIXA_TRACE(("lixa_xid_compare: a->data + a->gtrid_length is ",
                    (const byte_t *)a->data + a->gtrid_length,
                    a->bqual_length));
        LIXA_TRACE(("lixa_xid_compare: b->data + b->gtrid_length is ",
                    (const byte_t *)b->data + b->gtrid_length,
                    a->bqual_length));
        return result;
    }
    return 0;
}
