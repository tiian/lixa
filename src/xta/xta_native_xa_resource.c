/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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



/* system includes */
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "client_conn.h"
/* XTA includes */
#include "xta_native_xa_resource.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_native_xa_resource_t *xta_native_xa_resource_new(
    const char *name, const char *switch_file,
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
                               this, -1, NULL, name, switch_file,
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
        /* recovery step in the event of error */
        if (NONE > excp) {
            if (NULL != this) {
                g_free(this);
                this = NULL;
            }
        } /* if (NONE > excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_native_xa_resource_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



xta_native_xa_resource_t *xta_native_xa_resource_new_by_rmid(
    int rmid, const xta_transaction_manager_config_t *config)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , XTA_NATIVE_XA_RESOURCE_INIT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_native_xa_resource_t *this = NULL;
    
    LIXA_TRACE(("xta_native_xa_resource_new_by_rmid\n"));
    TRY {
        /* allocate sufficient memory for the object */
        if (NULL == (this = (xta_native_xa_resource_t *)g_try_malloc0(
                         sizeof(xta_native_xa_resource_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* initialize the object */
        if (LIXA_RC_OK != (ret_cod = xta_native_xa_resource_init(
                               this, rmid, config, NULL, NULL, NULL, NULL)))
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
    if (NONE > excp) {
        /* memory recovery */
        g_free(this);
        this = NULL;
    }
    LIXA_TRACE(("xta_native_xa_resource_new_by_rmid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_native_xa_resource_delete(xta_native_xa_resource_t *xa_resource)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_native_xa_resource_delete\n"));
    TRY {
        /* dispose object content */
        xta_native_xa_resource_clean(xa_resource);
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
    LIXA_TRACE(("xta_native_xa_resource_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



int xta_native_xa_resource_init(
    xta_native_xa_resource_t *xa_resource,
    int rmid, const xta_transaction_manager_config_t *config, const char *name,
    const char *switch_file, const char *open_info, const char *close_info)
{
    enum Exception { NULL_OBJECT
                     , XA_RESOURCE_INIT_ERROR
                     , INVALID_OPTION
                     , XML_STRDUP_ERROR1
                     , XML_STRDUP_ERROR2
                     , CLIENT_CONFIG_LOAD_SWITCH_FILE_ERROR
                     , OUT_OF_RANGE
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_native_xa_resource_init\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT);
        /* call parent initializator */
        if (LIXA_RC_OK != (ret_cod = xta_xa_resource_init(
                               (xta_xa_resource_t *)xa_resource, TRUE)))
            THROW(XA_RESOURCE_INIT_ERROR);
        if (rmid < 0) {
            /* rmid < 0: this is a dynamic definition */
            xa_resource->xa_resource.dynamic = TRUE;
            if (NULL == name || NULL == switch_file || NULL == open_info ||
                NULL == close_info)
                THROW(INVALID_OPTION);
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
            if (NULL == (xa_resource->xa_resource.rsrmgr_config.name =
                         xmlCharStrdup(name)))
                THROW(XML_STRDUP_ERROR1);
            /* duplicate resource switch_file (path) */
            if (NULL == (xa_resource->xa_resource.rsrmgr_config.switch_file =
                         xmlCharStrdup(switch_file)))
                THROW(XML_STRDUP_ERROR2);
            /* copy open_info */
            strncpy(xa_resource->xa_resource.rsrmgr_config.xa_open_info, open_info,
                    MAXINFOSIZE);
            xa_resource->xa_resource.rsrmgr_config.xa_open_info[MAXINFOSIZE-1] =
                '\0';
            /* copy close_info */
            strncpy(xa_resource->xa_resource.rsrmgr_config.xa_close_info, close_info,
                    MAXINFOSIZE);
            xa_resource->xa_resource.rsrmgr_config.xa_close_info[MAXINFOSIZE-1] =
                '\0';
            /* load the switch file for the resource manager */
            if (LIXA_RC_OK != (ret_cod = client_config_load_switch_file(
                                   &xa_resource->xa_resource.act_rsrmgr_config,
                                   TRUE)))
                THROW(CLIENT_CONFIG_LOAD_SWITCH_FILE_ERROR);
            LIXA_TRACE(("xta_native_xa_resource_init: initialized resource "
                        "name='%s', switch_file='%s', xa_open_info='%s', "
                        "xa_close_info='%s'\n",
                        xa_resource->xa_resource.rsrmgr_config.name,
                        xa_resource->xa_resource.rsrmgr_config.switch_file,
                        xa_resource->xa_resource.rsrmgr_config.xa_open_info,
                        xa_resource->xa_resource.rsrmgr_config.xa_close_info));
        } else {
            struct act_rsrmgr_config_s *act_rsrmgr;
            /* rmid >= 0: get the properties from global configuration that
             * has been loaded by xta_transaction_manager_new() */
            xa_resource->xa_resource.dynamic = FALSE;
            if (rmid >=
                ((client_config_coll_t *)config)->actconf.rsrmgrs->len) {
                LIXA_TRACE(("xta_native_xa_resource_init: rmid=%d is out of "
                            "range [0,%u]\n", rmid,
                            ((client_config_coll_t *)config)->
                            actconf.rsrmgrs->len-1));
                THROW(OUT_OF_RANGE);
            }
            act_rsrmgr = &g_array_index(
                ((client_config_coll_t *)config)->actconf.rsrmgrs,
                struct act_rsrmgr_config_s, rmid);
            /* copy it locally to the resource object */
            xa_resource->xa_resource.act_rsrmgr_config = *act_rsrmgr;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case XA_RESOURCE_INIT_ERROR:
                break;
            case INVALID_OPTION:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case XML_STRDUP_ERROR1:
            case XML_STRDUP_ERROR2:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
                break;
            case CLIENT_CONFIG_LOAD_SWITCH_FILE_ERROR:
                break;
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
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



void xta_native_xa_resource_clean(xta_native_xa_resource_t *xa_resource)
{
    enum Exception { CLIENT_CONFIG_UNLOAD_SWITCH_FILE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_native_xa_resource_clean\n"));
    TRY {
        /* clean properties only for dynamically created resources */
        if (xa_resource->xa_resource.dynamic) {
            /* clean resource name */
            if (NULL != xa_resource->xa_resource.rsrmgr_config.name) {
                g_free(xa_resource->xa_resource.rsrmgr_config.name);
                xa_resource->xa_resource.rsrmgr_config.name = NULL;
            }
            /* clean switch_file */
            if (NULL != xa_resource->xa_resource.rsrmgr_config.switch_file) {
                g_free(xa_resource->xa_resource.rsrmgr_config.switch_file);
                xa_resource->xa_resource.rsrmgr_config.switch_file = NULL;
            }
            /* clean xa_open_info and xa_close_info */
            xa_resource->xa_resource.rsrmgr_config.xa_open_info[0] = '\0';
            xa_resource->xa_resource.rsrmgr_config.xa_close_info[0] = '\0';
            /* clean pointer from complete to partial structure */
            xa_resource->xa_resource.act_rsrmgr_config.generic = NULL;
            /* unload module */
            if (NULL != xa_resource->xa_resource.act_rsrmgr_config.module) {
                if (LIXA_RC_OK != (
                        ret_cod = client_config_unload_switch_file(
                            &xa_resource->xa_resource.act_rsrmgr_config)))
                    THROW(CLIENT_CONFIG_UNLOAD_SWITCH_FILE_ERROR);
            }
        } /* if (xa_resource->dynamic) */
        /* clean "base class" (xta_xa_resource_t) properties */
        xta_xa_resource_clean((xta_xa_resource_t *)xa_resource);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_CONFIG_UNLOAD_SWITCH_FILE_ERROR:
                break;
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

