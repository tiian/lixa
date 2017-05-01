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
#include "xta_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



int xta_xa_resource_init(xta_xa_resource_t *this,
                         int must_be_opened)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_init\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* set resource description (first part) */
        this->rsrmgr_config.name = NULL;
        this->rsrmgr_config.switch_file = NULL;
        this->rsrmgr_config.xa_open_info[0] = '\0';
        this->rsrmgr_config.xa_close_info[0] = '\0';
        /* set resource description (second part) */
        this->act_rsrmgr_config.generic = &this->rsrmgr_config;
        this->act_rsrmgr_config.module = NULL;
        this->act_rsrmgr_config.xa_switch = NULL;
        /* set other object properties */
        this->must_be_opened = must_be_opened;
        /* set no Transaction Manager as an initial state */
        this->registered_tm = NULL;
        
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
    return ret_cod;
}



const xta_xa_resource_config_t *xta_xa_resource_get_config(
    const xta_xa_resource_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    const xta_xa_resource_config_t *config = NULL;
    
    LIXA_TRACE(("xta_xa_resource_get_config\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* duplicate the config structs */
        
        /* return the copy of the config structs */
        config = &this->act_rsrmgr_config;
        
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
    return config;
}



int xta_xa_resource_registered(xta_xa_resource_t *this,
                               const xta_transaction_manager_t *tm)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , RESOURCE_ALREADY_REGISTERED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_registered\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == tm)
            THROW(NULL_OBJECT2);
        if (NULL != this->registered_tm) {
            /* already registered resource, checking the Transaction Manager */
            if (tm != this->registered_tm) {
                LIXA_TRACE(("xta_xa_resource_registered: this resource has "
                            "been already registered by another TM: %p\n",
                            this->registered_tm));
                THROW(RESOURCE_ALREADY_REGISTERED);
            } else {
                LIXA_TRACE(("xta_xa_resource_registered: this resource has "
                            "been already registered by this TM, "
                            "skipping...\n"));
            } /* if (tm != this->registered_tm) */
        } else {
            this->registered_tm = tm;
            LIXA_TRACE(("xta_xa_resource_registered: this resource is now "
                        "registered by TM: %p\n", this->registered_tm));
        } /* if (NULL != this->registered_tm) */
        
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
    LIXA_TRACE(("xta_xa_resource_registered/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



void xta_xa_resource_clean(xta_xa_resource_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_clean\n"));
    TRY {
        /* check the object is not null */
        if (NULL == this)
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
    return;
}



int xta_xa_resource_start(xta_xa_resource_t *this,
                          const xta_xid_t *xid,
                          long flag)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_start\n"));
    TRY {
        /* @@@ implement me */
        
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
    return ret_cod;
}



int xta_xa_resource_end(xta_xa_resource_t *this,
                        const xta_xid_t *xid,
                        long flag)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_end\n"));
    TRY {
        /* @@@ implement me */
        
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
    return ret_cod;
}



int xta_xa_resource_prepare(xta_xa_resource_t *this,
                         const xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_prepare\n"));
    TRY {
        /* @@@ implement me */
        
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
    return ret_cod;
}



int xta_xa_resource_commit(xta_xa_resource_t *this,
                        const xta_xid_t *xid,
                        int one_phase)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xa_resource_commit\n"));
    TRY {
        /* @@@ implement me */
        
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
    return ret_cod;
}

