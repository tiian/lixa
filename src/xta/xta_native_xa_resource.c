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
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
/* XTA includes */
#include "xta_native_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_native_xa_resource_t *xta_native_xa_resource_new(
    int rmid, const char *name, const char *switch_file,
    const char *open_info, const char *close_info)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , XTA_NATIVE_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_native_xa_resource_t *this = NULL;
    
    LIXA_TRACE(("xta_native_xa_resource_new\n"));
    TRY {
        /* allocate sufficient memory for the object */
        if (NULL == (this = (xta_native_xa_resource_t *)g_try_malloc0(
                         sizeof(xta_native_xa_resource_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* initialize the object */
        if (LIXA_RC_OK != (ret_cod = xta_native_xa_resource_init(
                               this, rmid, name, switch_file,
                               open_info, close_info)))
            THROW(XTA_NATIVE_XA_RESOURCE_INIT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case XTA_NATIVE_XA_RESOURCE_INIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_native_xa_resource_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_native_xa_resource_delete(xta_native_xa_resource_t *this)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_native_xa_resource_delete\n"));
    TRY {
        /* dispose object content */
        xta_native_xa_resource_clean(this);
        /* release memory allocated for the object */
        g_free(this);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_native_xa_resource_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



int xta_native_xa_resource_init(
    xta_native_xa_resource_t *this,
    int rmid, const char *name, const char *switch_file,
    const char *open_info, const char *close_info)
{
    enum Exception { NULL_OBJECT
                     , XA_RESOURCE_INIT_ERROR
                     , INVALID_OPTION1
                     , INVALID_OPTION2
                     , G_STRDUP_ERROR1
                     , G_STRDUP_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_native_xa_resource_init\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* call father initializator */
        if (LIXA_RC_OK != (ret_cod = xta_xa_resource_init(
                               (xta_xa_resource_t *)this, TRUE)))
            THROW(XA_RESOURCE_INIT_ERROR);
        if (rmid < 0) {
            /* rmid < 0: this is a dynamic definition */
            if (NULL == name || NULL == switch_file || NULL == open_info ||
                NULL == close_info)
                THROW(INVALID_OPTION1);
            if (0 == strlen(open_info) || 0 == strlen(close_info))
                THROW(INVALID_OPTION2);
            if (strlen(open_info) >= MAXINFOSIZE) {
                LIXA_TRACE(("xta_native_xa_resource_init: option open_info "
                            "(" SIZE_T_FORMAT ")"
                            "exceeds MAXINFOSIZE %d\n", strlen(open_info),
                            MAXINFOSIZE));
            } /* if (strlen(open_info) >= MAXINFOSIZE) */
            if (strlen(close_info) >= MAXINFOSIZE) {
                LIXA_TRACE(("xta_native_xa_resource_init: option close_info "
                            "(" SIZE_T_FORMAT ")"
                            "exceeds MAXINFOSIZE %d\n", strlen(close_info),
                            MAXINFOSIZE));
            } /* if (strlen(close_info) >= MAXINFOSIZE) */
            /* duplicate resource name */
            if (NULL == (this->xa_resource.rsrmgr_config.name =
                         (xmlChar *)g_strdup(name)))
                THROW(G_STRDUP_ERROR1);
            /* duplicate resource switch_file (path) */
            if (NULL == (this->xa_resource.rsrmgr_config.switch_file =
                         (xmlChar *)g_strdup(switch_file)))
                THROW(G_STRDUP_ERROR2);
            /* copy open_info */
            strncpy(this->xa_resource.rsrmgr_config.xa_open_info, open_info,
                    MAXINFOSIZE);
            this->xa_resource.rsrmgr_config.xa_open_info[MAXINFOSIZE-1] =
                '\0';
            /* copy close_info */
            strncpy(this->xa_resource.rsrmgr_config.xa_close_info, close_info,
                    MAXINFOSIZE);
            this->xa_resource.rsrmgr_config.xa_close_info[MAXINFOSIZE-1] =
                '\0';
            /* restart from here @@@ */
        } else {
            /* rmid >= 0: get the properties from global configuration */
        }
        /* nothing else to initialize here... @@@ */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case XA_RESOURCE_INIT_ERROR:
                break;
            case INVALID_OPTION1:
            case INVALID_OPTION2:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case G_STRDUP_ERROR1:
            case G_STRDUP_ERROR2:
                ret_cod = LIXA_RC_G_STRDUP_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_native_xa_resource_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void xta_native_xa_resource_clean(xta_native_xa_resource_t *this)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_native_xa_resource_clean\n"));
    TRY {
        /* clean "base class" (xta_xa_resource_t) properties */
        xta_xa_resource_clean(
            (xta_xa_resource_t *)this);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_native_xa_resource_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}

