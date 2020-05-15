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



int xta_acquired_xa_resource_init(xta_acquired_xa_resource_t *xa_resource,
                                  const struct xta_iface_s *iface,
                                  const char *name,
                                  const char *open_info)
{
    enum Exception { NULL_OBJECT1
                     , OBJ_CORRUPTED
                     , NULL_OBJECT2
                     , INVALID_OPTION1
                     , INVALID_OPTION2
                     , XTA_XA_RESOURCE_INIT_ERROR
                     , XML_STRDUP_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_acquired_xa_resource_init\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT1);
        /* check the object has not already been initialized */
        if (NULL != xa_resource->xa_resource.rsrmgr_config.name)
            THROW(OBJ_CORRUPTED);
        /* interface can't be NULL */
        if (NULL == iface)
            THROW(NULL_OBJECT2);
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
        /*
         * call parent initializator
         * Acquired XA Resources don't generally need explicit
         * xa_open, xa_close
         */
        LIXA_TRACE(("xta_acquired_xa_resource_init: name='%s', "
                    "open_info='%s'\n", name, open_info));
        if (LIXA_RC_OK != (ret_cod = xta_xa_resource_init(
                               (xta_xa_resource_t *)xa_resource, FALSE)))
            THROW(XTA_XA_RESOURCE_INIT_ERROR);
        /* set object properties */
        xa_resource->iface = iface;
        if (NULL == (xa_resource->xa_resource.rsrmgr_config.name =
                     xmlCharStrdup(name)))
            THROW(XML_STRDUP_ERROR);
        strncpy(xa_resource->xa_resource.rsrmgr_config.xa_open_info,
                open_info, MAXINFOSIZE);
        xa_resource->xa_resource.rsrmgr_config.xa_open_info[MAXINFOSIZE-1] =
            '\0';
            
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_OPTION1:
            case INVALID_OPTION2:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case XTA_XA_RESOURCE_INIT_ERROR:
                break;
            case XML_STRDUP_ERROR:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
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



void xta_acquired_xa_resource_clean(xta_acquired_xa_resource_t *xa_resource)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_acquired_xa_resource_clean\n"));
    TRY {
        if (NULL == xa_resource)
            THROW(NULL_OBJECT);
        if (NULL != xa_resource->xa_resource.rsrmgr_config.name) {
            g_free(xa_resource->xa_resource.rsrmgr_config.name);
            xa_resource->xa_resource.rsrmgr_config.name = NULL;
        }
        xa_resource->xa_resource.rsrmgr_config.xa_open_info[0] = '\0';
        
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

