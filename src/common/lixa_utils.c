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
