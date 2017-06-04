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



const struct xta_iface_s xta_mysql_iface = {
    "XTA MySQL",
    TMNOFLAGS,
    0,
    xta_mysql_xa_open,
    xta_mysql_xa_close,
    xta_mysql_xa_start,
    xta_mysql_xa_end,
    xta_mysql_xa_rollback,
    xta_mysql_xa_prepare,
    xta_mysql_xa_commit,
    xta_mysql_xa_recover,
    xta_mysql_xa_forget
};



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
    enum Exception { NULL_OBJECT
                     , XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_mysql_xa_resource_init\n"));
    TRY {
        if (NULL == connection)
            THROW(NULL_OBJECT);
        /* initialize "base class" (xta_acquired_xa_resource_t) properties */
        if (LIXA_RC_OK != (ret_cod = xta_acquired_xa_resource_init(
                               (xta_acquired_xa_resource_t *)this,
                               &xta_mysql_iface,
                               name, open_info)))
            THROW(XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR);
        /* set connection */
        this->connection = connection;
        /* set resource interface */
        lixa_iface_set_xta(&this->xa_resource.act_rsrmgr_config.lixa_iface,
                           &xta_mysql_iface, (xta_xa_resource_t *)this);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
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
        this->xa_resource.act_rsrmgr_config.lixa_iface.std = NULL;
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



int xta_mysql_xa_open(xta_xa_resource_t *context, char *xa_info, int rmid)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_open\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL != this->connection) {
            LIXA_TRACE(("xta_mysql_xa_open: MySQL connection is already open "
                        "(%p)\n", this->connection));
        } else {
            LIXA_TRACE(("xta_mysql_xa_open: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_mysql_xa_close(char *xa_info, int rmid)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_close\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_mysql_xa_start(const XID *xid, int rmid, long flags)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_start\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_mysql_xa_end(XID *xid, int rmid, long flags)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_end\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    



int xta_mysql_xa_rollback(XID *xid, int rmid)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_rollback\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


int xta_mysql_xa_prepare(XID *xid, int rmid)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_prepare\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


int xta_mysql_xa_commit(XID *xid, int rmid, long flags)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_commit\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


int xta_mysql_xa_recover(XID *xids, long count, int rmid, long flags)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_recover\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_recover/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


int xta_mysql_xa_forget(XID *xid, int rmid)
{
    enum Exception { NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_forget\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = XA_OK;
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_forget/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
