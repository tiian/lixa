/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <config.h>



/* system includes */
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_xid.h"
/* XTA includes */
#include "xta_xid.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_xid_t *xta_xid_new(const char *branch_qualifier)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *this = NULL;
    
    LIXA_TRACE(("xta_xid_t\n"));
    TRY {
        uuid_t tmp_bqual;
        
        /* allocate the object */
        if (NULL == (this = (xta_xid_t *)
                     g_try_malloc0(sizeof(xta_xid_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* convert from ASCII HEX to binary hex */
        lixa_xid_set_bqual(branch_qualifier, tmp_bqual);
        /* initialize the object */        
        lixa_xid_create_new(tmp_bqual, &this->xa_xid);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_xid_t/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



xta_xid_t *xta_xid_new_from_string(const char *xid_string)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , LIXA_XID_DESERIALIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *this = NULL;
    
    LIXA_TRACE(("xta_xid_new_from_string\n"));
    TRY {
        /* allocate the object */
        if (NULL == (this = (xta_xid_t *)
                     g_try_malloc0(sizeof(xta_xid_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* deserialize XID */
        if (!lixa_xid_deserialize(&this->xa_xid, xid_string))
            THROW(LIXA_XID_DESERIALIZE_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case LIXA_XID_DESERIALIZE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* clean-up memory in the event of an error */
    if (NONE != excp) {
        g_free(this);
        this = NULL;
    }   
    LIXA_TRACE(("xta_xid_new_from_string/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_xid_delete(xta_xid_t *this)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xid_delete\n"));
    TRY {
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
    LIXA_TRACE(("xta_xid_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



const XID *xta_xid_get_xa_xid(xta_xid_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    XID *xa_xid = NULL;
    
    LIXA_TRACE(("xta_xid_get_xa_xid\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        xa_xid = &this->xa_xid;
        
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
    LIXA_TRACE(("xta_xid_get_xa_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return xa_xid;
}



char *xta_xid_to_string(const xta_xid_t *this)
{
    enum Exception { NULL_OBJECT
                     , MALLOC_ERROR
                     , LIXA_XID_SERIALIZE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    char *string = NULL;
    
    LIXA_TRACE(("xta_xid_to_string\n"));
    TRY {
        lixa_ser_xid_t lsx;
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* allocate the dynamic memory */
        if (NULL == (string = malloc(sizeof(lixa_ser_xid_t))))
            THROW(MALLOC_ERROR);
        if (!lixa_xid_serialize(&this->xa_xid, lsx))
            THROW(LIXA_XID_SERIALIZE_ERROR);
        strncpy(string, lsx, sizeof(lixa_ser_xid_t));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case LIXA_XID_SERIALIZE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_xid_to_string/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return string;
}



void xta_xid_reset(xta_xid_t *this)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xid_reset\n"));
    TRY {
        lixa_xid_reset(&this->xa_xid);
        
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
    LIXA_TRACE(("xta_xid_reset/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



