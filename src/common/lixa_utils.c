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



#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_LIBGEN_H
# include <libgen.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
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



#include <lixa_errors.h>
#include <lixa_inst_conf.h>
#include <lixa_trace.h>
#include <lixa_utils.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_UTILS



void lixa_print_version(FILE *stream)
{
    fprintf(stream, "LIXA: a LIbre XA implementation\n"
            "Copyright (c) 2009-2016, Christian Ferrari; "
            "all rights reserved.\n"
            "License: GPL (GNU Public License) version 2\n"
            "Package name: %s; package version: %s\n"
            "Access http://sourceforge.net/projects/lixa/ to report bugs "
            "and partecipate to the project\n",
            LIXA_PACKAGE_NAME, LIXA_PACKAGE_VERSION);
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



