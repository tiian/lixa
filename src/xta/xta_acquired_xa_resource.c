/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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



/* system includes */
#ifdef HAVE_STRING_H
# include <string.h>
#endif
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
/* XTA includes */
#include "xta_acquired_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



int xta_acquired_xa_resource_init(xta_acquired_xa_resource_t *this,
                                  const char *name,
                                  const char *open_info)
{
    enum Exception { NULL_OBJECT
                     , OBJ_CORRUPTED
                     , INVALID_OPTION1
                     , INVALID_OPTION2
                     , G_STRDUP_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_acquired_xa_resource_init\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check the object has not already initialized */
        if (NULL != this->name)
            THROW(OBJ_CORRUPTED);
        /* name can't be NULL or empty */
        if (NULL == name || 0 == strlen(name))
            THROW(INVALID_OPTION1);
        /* open_info can't be NULL or empty */
        if (NULL == open_info || 0 == strlen(open_info))
            THROW(INVALID_OPTION2);
        if (strlen(open_info) >= MAXINFOSIZE) {
            LIXA_TRACE(("xta_acquired_xa_resource_init: option open_info "
                        "(" SIZE_T_FORMAT ")"
                        "exceeds MAXINFOSIZE %d\n", strlen(open_info),
                        MAXINFOSIZE));
        }
        /* set object properties */
        if (NULL == (this->name = g_strdup(name)))
            THROW(G_STRDUP_ERROR);
        strncpy(this->open_info, open_info, MAXINFOSIZE);
        this->open_info[MAXINFOSIZE] = '\0';
            
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case INVALID_OPTION1:
            case INVALID_OPTION2:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case G_STRDUP_ERROR:
                ret_cod = LIXA_RC_G_STRDUP_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_acquired_xa_resource_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void xta_acquired_xa_resource_clean(xta_acquired_xa_resource_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_acquired_xa_resource_clean\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        if (NULL != this->name) {
            g_free(this->name);
            this->name = NULL;
        }
        this->open_info[0] = '\0';
        
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
    LIXA_TRACE(("xta_acquired_xa_resource_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}

