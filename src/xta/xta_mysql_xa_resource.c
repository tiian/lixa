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
#include "liblixamy.h"   /* LIXA wrapper for MySQL */
/* XTA includes */
#include "xta_mysql_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_mysql_xa_resource_t *xta_mysql_xa_resource_new(MYSQL *connection,
                                                   const char *name,
                                                   const char *open_info)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , XTA_MYSQL_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_mysql_xa_resource_t *this = NULL;
    
    LIXA_TRACE(("xta_mysql_xa_resource_new\n"));
    TRY {
        /* allocate sufficient memory for the object */
        if (NULL == (this = (xta_mysql_xa_resource_t *)g_try_malloc0(
                         sizeof(xta_mysql_xa_resource_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* initialize "class" properties */
        if (LIXA_RC_OK != (ret_cod = xta_mysql_xa_resource_init(
                               this, connection, name, open_info)))
            THROW(XTA_MYSQL_XA_RESOURCE_INIT_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case XTA_MYSQL_XA_RESOURCE_INIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_resource_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_mysql_xa_resource_delete(xta_mysql_xa_resource_t *this)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_mysql_xa_resource_delete\n"));
    TRY {
        /* clean the object before releasing */
        xta_mysql_xa_resource_clean(this);
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
    LIXA_TRACE(("xta_mysql_xa_resource_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



int xta_mysql_xa_resource_init(xta_mysql_xa_resource_t *this,
                               MYSQL *connection,
                               const char *name, const char *open_info)
{
    enum Exception { XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_mysql_xa_resource_init\n"));
    TRY {
        /* initialize "base class" (xta_acquired_xa_resource_t) properties */
        if (LIXA_RC_OK != (ret_cod = xta_acquired_xa_resource_init(
                               (xta_acquired_xa_resource_t *)this,
                               name, open_info)))
            THROW(XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR);
        /* initialize MySQL XA function pointers */
        this->xa_resource.act_rsrmgr_config.xa_switch = &xamyls;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_resource_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void xta_mysql_xa_resource_clean(xta_mysql_xa_resource_t *this)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_mysql_xa_resource_clean\n"));
    TRY {
        /* clean MySQL XA function pointers */
        this->xa_resource.act_rsrmgr_config.xa_switch = NULL;
        /* clean "base class" (xta_acquired_xa_resource_t) properties */
        xta_acquired_xa_resource_clean(
            (xta_acquired_xa_resource_t *)this);
        
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
    LIXA_TRACE(("xta_mysql_xa_resource_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}

