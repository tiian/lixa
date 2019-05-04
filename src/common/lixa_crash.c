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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <config.h>



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif



#include "lixa_crash.h"
#include "lixa_trace.h"
#include "lixa_syslog.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_UTILS



/**
 * Environment variable used to specify the crash point
 */
#define LIXA_CRASH_POINT_ENV_VAR "LIXA_CRASH_POINT"
/**
 * Environment variable used to specify the crash count
 */
#define LIXA_CRASH_COUNT_ENV_VAR "LIXA_CRASH_COUNT"



/**
 * Flag used to store initialization status of the crash simulation feature
 */
int lixa_crash_initialized = FALSE;
/**
 * Mutex used to sequentialize the access to some properties/methods
 */
static GMutex lixa_crash_mutex;
/**
 * Simulated crash point
 */
uint32_t lixa_crash_point = LIXA_CRASH_POINT_NULL;
/**
 * Simulated crash count threshold
 */
long lixa_crash_count_threshold = 1;



void lixa_crash_init(void)
{
    char *tmp_str;

    if (lixa_crash_initialized)
        return;

    g_mutex_lock(&lixa_crash_mutex);
    tmp_str = getenv(LIXA_CRASH_POINT_ENV_VAR);
    if (NULL != tmp_str) {
        lixa_crash_point = strtoul(tmp_str, NULL, 0);
        LIXA_TRACE(("lixa_crash_init: crash point set to " UINT32_T_FORMAT
                    "\n", lixa_crash_point));
        tmp_str = getenv(LIXA_CRASH_COUNT_ENV_VAR);
        if (NULL != tmp_str) {
            lixa_crash_count_threshold = strtol(tmp_str, NULL, 0);
            LIXA_TRACE(("lixa_crash_init: crash count threshold set to %ld\n",
                        lixa_crash_count_threshold));
        }
    }
    lixa_crash_initialized = TRUE;
    g_mutex_unlock(&lixa_crash_mutex);
    return;
}



void lixa_crash(lixa_word_t crash_point, long *count)
{
    if (crash_point == lixa_crash_point) {
        g_mutex_lock(&lixa_crash_mutex);
        *count = *count + 1;
        if (*count >= lixa_crash_count_threshold) {
            LIXA_TRACE(("lixa_crash: crash threshold reached (%d) for crash "
                        "point "UINT32_T_FORMAT", crashing!\n",
                        *count, crash_point));
            LIXA_SYSLOG((LOG_CRIT, LIXA_SYSLOG_LXG000C, crash_point));
            abort();
        }
        g_mutex_unlock(&lixa_crash_mutex);
    }
    return;
}
