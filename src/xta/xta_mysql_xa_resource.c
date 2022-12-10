/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
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



/**
 * Interface with XA function pointers
 */
const static struct xta_iface_s xta_mysql_iface = {
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
    LIXA_TRACE_STACK();
    return this;
}



void xta_mysql_xa_resource_delete(xta_mysql_xa_resource_t *xa_resource)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_mysql_xa_resource_delete\n"));
    TRY {
        /* clean the object before releasing */
        xta_mysql_xa_resource_clean(xa_resource);
        /* release memory allocated for the object */
        g_free(xa_resource);
        
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
    LIXA_TRACE_STACK();
    return;
}



int xta_mysql_xa_resource_init(xta_mysql_xa_resource_t *xa_resource,
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
                               (xta_acquired_xa_resource_t *)xa_resource,
                               &xta_mysql_iface,
                               name, open_info)))
            THROW(XTA_ACQUIRED_XA_RESOURCE_INIT_ERROR);
        /* set connection */
        xa_resource->connection = connection;
        /* set resource interface */
        lixa_iface_set_xta(
            &xa_resource->xa_resource.act_rsrmgr_config.lixa_iface,
            &xta_mysql_iface, (xta_xa_resource_t *)xa_resource);
        /* reset status */
        memset(&xa_resource->lssr, 0, sizeof(struct lixa_sw_status_rm_s));
        
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



void xta_mysql_xa_resource_clean(xta_mysql_xa_resource_t *xa_resource)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_mysql_xa_resource_clean\n"));
    TRY {
        /* clean MySQL XA function pointers */
        xa_resource->xa_resource.act_rsrmgr_config.lixa_iface.std = NULL;
        /* clean "base class" (xta_acquired_xa_resource_t) properties */
        xta_acquired_xa_resource_clean(
            (xta_acquired_xa_resource_t *)xa_resource);
        
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
    LIXA_TRACE_STACK();
    return;
}



int xta_mysql_xa_open(xta_xa_resource_t *context, char *xa_info,
                      int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_open\n"));
    TRY {
        xta_mysql_xa_resource_t *xa_resource =
            (xta_mysql_xa_resource_t *)context;
        if (NULL != xa_resource->connection) {
            LIXA_TRACE(("xta_mysql_xa_open: MySQL connection is already open "
                        "(%p)\n", xa_resource->connection));
        } else {
            LIXA_TRACE(("xta_mysql_xa_open: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* save the connection state: backward compatibility with switch
         * file based implemetation */
        xa_resource->lssr.rmid = rmid;
        xa_resource->lssr.rm_type = LIXA_SW_STATUS_RM_TYPE_MYSQL;
        xa_resource->lssr.state.R = 1;
        xa_resource->lssr.conn = (gpointer)xa_resource->connection;
        
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int xta_mysql_xa_close(xta_xa_resource_t *context, char *xa_info,
                       int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_close\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL != this->connection) {
            LIXA_TRACE(("xta_mysql_xa_close: MySQL connection (%p) must be "
                        "closed by the Application Program\n",
                        this->connection));
        } else {
            LIXA_TRACE(("xta_mysql_xa_close: MySQL connection is NULL!\n"));
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
    LIXA_TRACE(("xta_mysql_xa_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int xta_mysql_xa_start(xta_xa_resource_t *context,
                       const XID *xid, int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_start\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_start: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_start_core(&this->lssr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int xta_mysql_xa_end(xta_xa_resource_t *context, const XID *xid,
                     int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_end\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_end: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_end_core(&this->lssr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
    



int xta_mysql_xa_rollback(xta_xa_resource_t *context, const XID *xid,
                          int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_rollback\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_rollback: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_rollback_core(&this->lssr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
    


int xta_mysql_xa_prepare(xta_xa_resource_t *context, const XID *xid,
                         int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_prepare\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_prepare: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_prepare_core(&this->lssr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
    


int xta_mysql_xa_commit(xta_xa_resource_t *context, const XID *xid,
                        int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_commit\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_commit: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_commit_core(&this->lssr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
    


int xta_mysql_xa_recover(xta_xa_resource_t *context,
                         XID *xids, long count, int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_recover\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_recover: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_recover_core(&this->lssr, xids, count, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_recover/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
    


int xta_mysql_xa_forget(xta_xa_resource_t *context, const XID *xid,
                        int rmid, long flags)
{
    enum Exception { OBJ_CORRUPTED
                     , NONE } excp;
    int ret_cod = XAER_RMERR;
    
    LIXA_TRACE(("xta_mysql_xa_forget\n"));
    TRY {
        xta_mysql_xa_resource_t *this = (xta_mysql_xa_resource_t *)context;
        if (NULL == this->connection) {
            LIXA_TRACE(("xta_mysql_xa_forget: MySQL connection is NULL!\n"));
            THROW(OBJ_CORRUPTED);
        }
        /* call legacy switch file based code */
        ret_cod = lixa_my_forget_core(&this->lssr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_CORRUPTED:
                break;
            case NONE:
                break;
            default:
                ret_cod = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_mysql_xa_forget/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}
