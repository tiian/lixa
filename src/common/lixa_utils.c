/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_LIBGEN_H
# include <libgen.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
# include <time.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif



#include "lixa_errors.h"
#include "lixa_inst_conf.h"
#include "lixa_trace.h"
#include "lixa_utils.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_UTILS



/* Even if it's no protected by a mutex, it's save to have it static because
   the system page size is immutable by itself */
size_t LIXA_SYSTEM_PAGE_SIZE;



void lixa_print_version(FILE *stream)
{
    fprintf(stream, "LIXA: a LIbre XA implementation\n"
            "Copyright (c) 2009-2019, Christian Ferrari; "
            "all rights reserved.\n"
            "License: GPL (GNU Public License) version 2\n"
            "Package name: %s; package version: %s\n"
            "Release date: %s;\n"
            "Driver for IBM DB2: %s;\n"
            "Driver for MySQL: %s;\n"
            "Driver for Oracle DBMS: %s;\n"
            "Driver for PostgreSQL: %s;\n"
            "Driver for IBM MQ: client:%s, server=%s;\n"
            "Access http://www.tiian.org/lixa/ to report bugs "
            "and partecipate to the project\n",
            LIXA_PACKAGE_NAME, LIXA_PACKAGE_VERSION,LIXA_RELEASE_DATE,
            lixa_config_have_ibmdb2() ? "yes" : "no",
            lixa_config_have_mysql() ? "yes" : "no",
            lixa_config_have_oracle() ? "yes" : "no",
            lixa_config_have_postgresql() ? "yes" : "no",
            lixa_config_have_webspheremq_etc() ? "yes" : "no",
            lixa_config_have_webspheremq_srv() ? "yes" : "no"
            );
}



int lixa_utils_iso_timestamp(const struct timeval *tv, char *buf,
                             size_t buf_size)
{
    enum Exception { BUFFER_OVERFLOW
                     , LOCALTIME_ERROR
                     , INTERNAL_ERROR1
                     , INTERNAL_ERROR2
                     , INTERNAL_ERROR3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_utils_iso_timestamp\n"));
    TRY {
        struct tm broken_down_time;
        size_t buf_len;
        
        if (buf_size < ISO_TIMESTAMP_BUFFER_SIZE)
            THROW(BUFFER_OVERFLOW);
        if (NULL == localtime_r(&(tv->tv_sec), &broken_down_time))
            THROW(LOCALTIME_ERROR);
        strftime(buf, buf_size, "%FT%T", &broken_down_time);
        if ((buf_len = strlen(buf)) >= buf_size)
            THROW(INTERNAL_ERROR1);
        sprintf(buf + buf_len, ".%6.6d", (int)tv->tv_usec);
        if ((buf_len = strlen(buf)) >= buf_size)
            THROW(INTERNAL_ERROR2);
        strftime(buf + buf_len, buf_size - buf_len, "%z", &broken_down_time);
        if ((buf_len = strlen(buf)) >= buf_size)
            THROW(INTERNAL_ERROR3);
        buf[ISO_TIMESTAMP_BUFFER_SIZE - 1] = '\0';
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_OVERFLOW:
                ret_cod = LIXA_RC_BUFFER_OVERFLOW;
                break;
            case LOCALTIME_ERROR:
                ret_cod = LIXA_RC_LOCALTIME_ERROR;
                break;
            case INTERNAL_ERROR1:
            case INTERNAL_ERROR2:
            case INTERNAL_ERROR3:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_utils_iso_timestamp/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_get_program_name(char *buf, size_t buf_size)
{
    enum Exception { OPEN_ERROR
                     , READ_ERROR
                     , UNSUPPORTED_PLATFORM
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    int fd = LIXA_NULL_FD;
    
    LIXA_TRACE(("lixa_get_program_name\n"));
    TRY {
#ifdef __linux
        char filename[1000];
        char tmp[1000];

        snprintf(filename, sizeof(filename), "/proc/%d/cmdline", getpid());
        filename[sizeof(filename)-1] = '\0';
        if (-1 == (fd = open(filename, O_RDONLY)))
            THROW(OPEN_ERROR);
        memset(buf, 0, buf_size);
        if (0 >= read(fd, tmp, sizeof(tmp)-1))
            THROW(READ_ERROR);
        strncpy(buf, basename(tmp), buf_size-1);
#else
# warning lixa_get_program_name not implemented for this platform!
        THROW(UNSUPPORTED_PLATFORM);
#endif
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_ERROR:
            case READ_ERROR:
            case UNSUPPORTED_PLATFORM:                
                strncpy(buf, DEFAULT_PROGRAM_NAME, buf_size-1);
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* write a safety terminator */
        buf[buf_size-1] = '\0';
        if (LIXA_NULL_FD != fd)
            close(fd);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_get_program_name/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void lixa_micro_sleep(long usec) {
    struct timeval tv;
    LIXA_TRACE(("lixa_micro_sleep: waiting %ld microseconds...\n", usec));
    tv.tv_sec = 0;
    tv.tv_usec = usec;
    select(0, NULL, NULL, NULL, &tv);
}



void lixa_timer_start(lixa_timer_t *lt) {
    if (-1 == gettimeofday(&(lt->begin_time), NULL)) {
        LIXA_TRACE(("lixa_timer_start: gettimeofday returned error "
                    "%d (%s)\n", errno, strerror(errno)));
    }
}



void lixa_timer_stop(lixa_timer_t *lt) {
    if (-1 == gettimeofday(&(lt->end_time), NULL)) {
        LIXA_TRACE(("lixa_timer_stop: gettimeofday returned error "
                    "%d (%s)\n", errno, strerror(errno)));
    }
}



void lixa_session_reset(lixa_session_t *session)
{
    memset(session, 0, sizeof(lixa_session_t));
}
    


int lixa_session_init(lixa_session_t *session, int fd, int client)
{
    enum Exception { GETSOCKNAME_ERROR
                     , GETPEERNAME_ERROR
                     , G_COMPUTE_CHECKSUM_FOR_DATA
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    struct sockaddr_storage sa[2];
    struct sockaddr *sock_sa, *peer_sa;
    socklen_t sock_addrlen = sizeof(struct sockaddr_storage);
    socklen_t peer_addrlen = sizeof(struct sockaddr_storage);
    gchar *checksum = NULL;
    
    LIXA_TRACE(("lixa_session_init\n"));
    TRY {
        /* reset the socket storage area for both socket ends */
        memset(sa, 0, 2*sizeof(struct sockaddr_storage));
        /* the hash must computed in the same order: client, server */
        if (client) {
            sock_sa = (struct sockaddr *)&sa[0];
            peer_sa = (struct sockaddr *)&sa[1];
        } else {
            sock_sa = (struct sockaddr *)&sa[1];
            peer_sa = (struct sockaddr *)&sa[0];
        }
        /* collect the info from the TCP/IP stack */
        if (0 != getsockname(fd, sock_sa, &sock_addrlen))
            THROW(GETSOCKNAME_ERROR);
        if (0 != getpeername(fd, peer_sa, &peer_addrlen))
            THROW(GETPEERNAME_ERROR);
        /* compute checksum */
        if (NULL == (checksum = g_compute_checksum_for_data(
                         G_CHECKSUM_MD5, (guchar *)sa,
                         2*sizeof(struct sockaddr_storage))))
            THROW(G_COMPUTE_CHECKSUM_FOR_DATA);
        /* copy the first part in sid field */
        strncpy(session->sid, checksum, LIXA_SESSION_ID_LENGTH);
        session->sid[LIXA_SESSION_ID_LENGTH-1] = '\0';
        LIXA_TRACE(("lixa_session_init: checksum='%s', sid='%s'\n",
                    checksum, session->sid));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GETSOCKNAME_ERROR:
                ret_cod = LIXA_RC_GETSOCKNAME_ERROR;
                break;
            case GETPEERNAME_ERROR:
                ret_cod = LIXA_RC_GETPEERNAME_ERROR;
                break;
            case G_COMPUTE_CHECKSUM_FOR_DATA:
                ret_cod = LIXA_RC_G_COMPUTE_CHECKSUM_FOR_DATA;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* memory free */
    if (NULL != checksum) {
        g_free(checksum);
        checksum = NULL;
    }
    LIXA_TRACE(("lixa_session_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_session_set_sid(lixa_session_t *session, const char *sid)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_session_set_sid\n"));
    TRY {
        if (NULL == sid)
            THROW(NULL_OBJECT);
        /* reset the destination, copy the value in a safe way */
        memset(&(session->sid), 0, LIXA_SESSION_ID_LENGTH);
        strncpy(session->sid, sid, LIXA_SESSION_ID_LENGTH);
        session->sid[LIXA_SESSION_ID_LENGTH] = '\0';
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_session_set_sid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



char *lixa_str_replace(const char *str, const char *from, const char *to)
{
    /* Increment positions cache size initially by this number. */
    size_t cache_sz_inc = 16;
    /* Thereafter, each time capacity needs to be increased,
     * multiply the increment by this factor. */
    const size_t cache_sz_inc_factor = 3;
    /* But never increment capacity by more than this number. */
    const size_t cache_sz_inc_max = 1048576;

    char *pret, *ret = NULL;
    const char *pstr2, *pstr = str;
    size_t i, count = 0;
#if (__STDC_VERSION__ >= 199901L)
    uintptr_t *pos_cache_tmp, *pos_cache = NULL;
#else
    ptrdiff_t *pos_cache_tmp, *pos_cache = NULL;
#endif
    size_t cache_sz = 0;
    size_t cpylen, orglen, retlen, tolen, fromlen = strlen(from);

    /* Find all matches and cache their positions. */
    while ((pstr2 = strstr(pstr, from)) != NULL) {
        count++;

        /* Increase the cache size when necessary. */
        if (cache_sz < count) {
            cache_sz += cache_sz_inc;
            pos_cache_tmp = realloc(pos_cache, sizeof(*pos_cache) * cache_sz);
            if (pos_cache_tmp == NULL) {
                goto end_repl_str;
            } else pos_cache = pos_cache_tmp;
            cache_sz_inc *= cache_sz_inc_factor;
            if (cache_sz_inc > cache_sz_inc_max) {
                cache_sz_inc = cache_sz_inc_max;
            }
        }
        
        pos_cache[count-1] = pstr2 - str;
        pstr = pstr2 + fromlen;
    }

    orglen = pstr - str + strlen(pstr);

    /* Allocate memory for the post-replacement string. */
    if (count > 0) {
        tolen = strlen(to);
        retlen = orglen + (tolen - fromlen) * count;
    } else
        retlen = orglen;
    ret = malloc(retlen + 1);
    if (ret == NULL) {
        goto end_repl_str;
    }

    if (count == 0) {
        /* If no matches, then just duplicate the string. */
        strcpy(ret, str);
    } else {
        /* Otherwise, duplicate the string whilst performing
         * the replacements using the position cache. */
        pret = ret;
        memcpy(pret, str, pos_cache[0]);
        pret += pos_cache[0];
        for (i = 0; i < count; i++) {
            memcpy(pret, to, tolen);
            pret += tolen;
            pstr = str + pos_cache[i] + fromlen;
            cpylen = (i == count-1 ? orglen : pos_cache[i+1]) - pos_cache[i] -
                fromlen;
            memcpy(pret, pstr, cpylen);
            pret += cpylen;
        }
        ret[retlen] = '\0';
    }
    
  end_repl_str:
    /* Free the cache and return the post-replacement string,
     * which will be NULL in the event of an error. */
    free(pos_cache);
    return ret;
}



const uint32_t lixa_crc32_poly8_lookup[256] =
{
    0, 0x77073096, 0xEE0E612C, 0x990951BA,
    0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3,
    0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988,
    0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
    0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE,
    0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
    0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC,
    0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
    0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172,
    0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,
    0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940,
    0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
    0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116,
    0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,
    0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924,
    0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,
    0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A,
    0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
    0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818,
    0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
    0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E,
    0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,
    0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C,
    0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
    0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2,
    0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,
    0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0,
    0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
    0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086,
    0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
    0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4,
    0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,
    0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A,
    0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,
    0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8,
    0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
    0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE,
    0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,
    0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC,
    0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
    0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252,
    0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
    0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60,
    0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
    0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236,
    0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,
    0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04,
    0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
    0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A,
    0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,
    0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38,
    0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
    0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E,
    0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
    0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C,
    0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
    0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2,
    0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,
    0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0,
    0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
    0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6,
    0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,
    0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94,
    0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D
};



uint32_t lixa_crc32(const uint8_t *buffer, size_t buffer_len)
{
    uint32_t crc = 0xffffffff;
    while (buffer_len-- !=0) crc = lixa_crc32_poly8_lookup[
        ((uint8_t) crc ^ *(buffer++))] ^ (crc >> 8);
    // return (~crc); also works
    return (crc ^ 0xffffffff);
}
