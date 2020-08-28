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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
/* system includes */
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
/* XTA includes */
#include "xta_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



int xta_xa_resource_init(xta_xa_resource_t *xa_resource,
                         int native)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_init\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT);
        /* set resource description (first part) */
        xa_resource->rsrmgr_config.name = NULL;
        xa_resource->rsrmgr_config.switch_file = NULL;
        xa_resource->rsrmgr_config.xa_open_info[0] = '\0';
        xa_resource->rsrmgr_config.xa_close_info[0] = '\0';
        /* set resource description (second part) */
        xa_resource->act_rsrmgr_config.generic = &xa_resource->rsrmgr_config;
        xa_resource->act_rsrmgr_config.module = NULL;
        lixa_iface_reset(&xa_resource->act_rsrmgr_config.lixa_iface);
        /* set dynamic to TRUE: XTA is typically dynamic with few exceptions */
        xa_resource->dynamic = TRUE;
        xa_resource->enlisted_tx = NULL;
        
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
    LIXA_TRACE(("xta_xa_resource_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



const xta_xa_resource_config_t *xta_xa_resource_get_config(
    const xta_xa_resource_t *xa_resource)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    const xta_xa_resource_config_t *config = NULL;
    
    LIXA_TRACE(("xta_xa_resource_get_config\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT);
        /* duplicate the config structs */
        
        /* return the copy of the config structs */
        config = &xa_resource->act_rsrmgr_config;
        
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
    LIXA_TRACE(("xta_xa_resource_get_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return config;
}



int xta_xa_resource_enlisted(xta_xa_resource_t *xa_resource,
                             const xta_transaction_t *tx)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , RESOURCE_ALREADY_REGISTERED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_enlisted\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT1);
        if (NULL == tx)
            THROW(NULL_OBJECT2);
        if (NULL != xa_resource->enlisted_tx) {
            /* already registered resource, checking the Transaction Manager */
            if (tx != xa_resource->enlisted_tx) {
                LIXA_TRACE(("xta_xa_resource_enlisted: this resource has "
                            "been already registered by another TX: %p\n",
                            xa_resource->enlisted_tx));
                THROW(RESOURCE_ALREADY_REGISTERED);
            } else {
                LIXA_TRACE(("xta_xa_resource_enlisted: this resource has "
                            "been already registered by this TX, "
                            "skipping...\n"));
            } /* if (tm != xa_resource->registered_tx) */
        } else {
            xa_resource->enlisted_tx = tx;
            LIXA_TRACE(("xta_xa_resource_enlisted: this resource is now "
                        "registered by TX %p\n", xa_resource->enlisted_tx));
        } /* if (NULL != xa_resource->registered_tx) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case RESOURCE_ALREADY_REGISTERED:
                ret_cod = LIXA_RC_RESOURCE_ALREADY_REGISTERED;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_xa_resource_enlisted/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



void xta_xa_resource_clean(xta_xa_resource_t *xa_resource)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_clean\n"));
    TRY {
        /* check the object is not null */
        if (NULL == xa_resource)
            THROW(NULL_OBJECT);
        
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
    LIXA_TRACE(("xta_xa_resource_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return;
}



int xta_xa_resource_start(xta_xa_resource_t *xa_resource,
                          const xta_xid_t *xid,
                          long flag)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_start\n"));
    TRY {
        
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
    LIXA_TRACE(("xta_xa_resource_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int xta_xa_resource_end(xta_xa_resource_t *xa_resource,
                        const xta_xid_t *xid,
                        long flag)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_end\n"));
    TRY {
        
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
    LIXA_TRACE(("xta_xa_resource_end/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int xta_xa_resource_prepare(xta_xa_resource_t *xa_resource,
                            const xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_prepare\n"));
    TRY {
        
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
    LIXA_TRACE(("xta_xa_resource_prepare/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int xta_xa_resource_commit(xta_xa_resource_t *xa_resource,
                           const xta_xid_t *xid,
                           int one_phase)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_commit\n"));
    TRY {
        
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
    LIXA_TRACE(("xta_xa_resource_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}

