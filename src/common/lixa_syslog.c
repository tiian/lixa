/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_syslog.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_UTILS



void lixa_syslog(int priority, const char *format, ...)
{
    enum Exception { MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    /* a new format to put priority in front of the message */
    char *new_format = NULL;
    va_list args;
    char buffer[2000];
    static const int priority_mask = LOG_EMERG | LOG_ALERT | LOG_CRIT |
        LOG_ERR | LOG_WARNING | LOG_NOTICE | LOG_INFO | LOG_DEBUG;
    
    LIXA_TRACE(("lixa_syslog: priority=%d (0x%x)\n", priority, priority));
    TRY {
        const char *level = NULL;
        int masked_priority = priority_mask & priority;
        if (NULL == (new_format = (char *)malloc(strlen(format)+20)))
            THROW(MALLOC_ERROR);

        va_start(args, format);

        switch (masked_priority) {
            case LOG_EMERG:
                level = "EMERG: ";
                break;
            case LOG_ALERT:
                level = "ALERT: ";
                break;
            case LOG_CRIT:
                level = "CRIT: ";
                break;
            case LOG_ERR:
                level = "ERR: ";
                break;
            case LOG_WARNING:
                level = "WARNING: ";
                break;
            case LOG_NOTICE:
                level = "NOTICE: ";
                break;
            case LOG_INFO:
                level = "INFO: ";
                break;
            case LOG_DEBUG:
                level = "DEBUG: ";
                break;
            default:
                level = "";
        } /* switch (masked_priority) */
        /*
        if (LOG_EMERG & priority)
            level = "EMERG: ";
        else if (LOG_ALERT & priority)
            level = "ALERT: ";
        else if (LOG_CRIT & priority)
            level = "CRIT: ";
        else if (LOG_ERR & priority)
            level = "ERR: ";
        else if (LOG_WARNING & priority)
            level = "WARNING: ";
        else if (LOG_NOTICE & priority)
            level = "NOTICE: ";
        else if (LOG_INFO & priority)
            level = "INFO: ";
        else if (LOG_DEBUG & priority)
            level = "DEBUG: ";
        else
            level = "";
        */
        strcpy(new_format, level);
        strcat(new_format, format);
        strcat(new_format, "\n");

#ifdef HAVE_VSNPRINTF
        vsnprintf(buffer, sizeof(buffer), new_format, args);
        buffer[sizeof(buffer)-1] = '\0';
        lixa_trace(buffer);
#else
# error "vsnprintf is necessary for flom_trace function!"
#endif
        va_end(args);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* memory recovery */
        if (NULL != new_format) {
            free(new_format);
            new_format = NULL;
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_syslog/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}
