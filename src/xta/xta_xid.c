/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "lixa_xid.h"
/* XTA includes */
#include "xta_xid.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_xid_t *xta_xid_new(const char *branch_qualifier,
                       int multiple_branches)
{
    enum Exception { STRDUP_ERROR
                     , G_TRY_MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *this = NULL;
    char *tmp_branch_qualifier = NULL;
    
    LIXA_TRACE(("xta_xid_new: branch_qualifier=%s, multiple_branches=%d\n",
                branch_qualifier, multiple_branches));
    TRY {
        uuid_t tmp_bqual;

        /* duplicate branch_qualifier */
        if (NULL == (tmp_branch_qualifier = strdup(branch_qualifier)))
            THROW(STRDUP_ERROR);
        /* allocate the object */
        if (NULL == (this = (xta_xid_t *)
                     g_try_malloc0(sizeof(xta_xid_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* set the first HEX digit of tmp_branch_qualifier according to
           multiple_branches flag */
        if (multiple_branches && !xta_xid_branch_qualifier_is_multibranch(
                tmp_branch_qualifier))
            xta_xid_branch_qualifier_set_multibranch(tmp_branch_qualifier);
        else if (!multiple_branches && xta_xid_branch_qualifier_is_multibranch(
                     tmp_branch_qualifier))
            xta_xid_branch_qualifier_unset_multibranch(tmp_branch_qualifier);
        /* convert from ASCII HEX to binary hex */
        lixa_xid_set_bqual(tmp_branch_qualifier, tmp_bqual);
        /* initialize the object */        
        lixa_xid_create_new(tmp_bqual, &this->xa_xid);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
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
    /* memory recovery */
    if (NULL != tmp_branch_qualifier) {
        free(tmp_branch_qualifier);
        tmp_branch_qualifier = NULL;
    }   
    LIXA_TRACE(("xta_xid_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



int xta_xid_branch_qualifier_is_multibranch(const char *branch_qualifier)
{
    int ret_value = FALSE;
    switch (branch_qualifier[0]) {
        case '0':
        case '2':
        case '4':
        case '6':
        case '8':
        case 'a':
        case 'A':
        case 'c':
        case 'C':
        case 'e':
        case 'E':
            ret_value = TRUE;
            break;
        default:
            break;
    } /* switch (branch_qualifier[0]) */
    LIXA_TRACE(("xta_xid_branch_qualifier_is_multibranch: "
                "branch_qualifier=%s, ret_value=%d\n", branch_qualifier,
                ret_value));
    return ret_value;
}



int xta_xid_branch_qualifier_set_multibranch(char *branch_qualifier)
{
    int ret_value = FALSE;
    LIXA_TRACE(("xta_xid_branch_qualifier_set_multibranch: "
                "branch_qualifier=%s\n", branch_qualifier));
    switch (branch_qualifier[0]) {
        case '0':
            break;
        case '1':
            branch_qualifier[0] = '0';
            ret_value = TRUE;
            break;
        case '2':
            break;
        case '3':
            branch_qualifier[0] = '2';
            ret_value = TRUE;
            break;
        case '4':
            break;
        case '5':
            branch_qualifier[0] = '4';
            ret_value = TRUE;
            break;
        case '6':
            break;
        case '7':
            branch_qualifier[0] = '6';
            ret_value = TRUE;
            break;
        case '8':
            break;
        case '9':
            branch_qualifier[0] = '8';
            ret_value = TRUE;
            break;
        case 'a':
        case 'A':
            break;
        case 'b':
        case 'B':
            branch_qualifier[0] = 'A';
            ret_value = TRUE;
            break;
        case 'c':
        case 'C':
            break;
        case 'd':
        case 'D':
            branch_qualifier[0] = 'C';
            ret_value = TRUE;
            break;
        case 'e':
        case 'E':
            break;
        case 'f':
        case 'F':
            branch_qualifier[0] = 'E';
            ret_value = TRUE;
            break;
        default:
            LIXA_TRACE(("xta_xid_branch_qualifier_set_multibranch: "
                        "branch_qualifier[0]='%c' (%s), this is an INTERNAL "
                        "ERROR\n", branch_qualifier[0], branch_qualifier));
    } /* switch (branch_qualifier[0]) */
    LIXA_TRACE(("xta_xid_branch_qualifier_set_multibranch: "
                "branch_qualifier=%s\n", branch_qualifier));
    return ret_value;
}



int xta_xid_branch_qualifier_unset_multibranch(char *branch_qualifier)
{
    int ret_value = FALSE;
    LIXA_TRACE(("xta_xid_branch_qualifier_unset_multibranch: "
                "branch_qualifier=%s\n", branch_qualifier));
    switch (branch_qualifier[0]) {
        case '0':
            branch_qualifier[0] = '1';
            ret_value = TRUE;
            break;
        case '1':
            break;
        case '2':
            branch_qualifier[0] = '3';
            ret_value = TRUE;
            break;
        case '3':
            break;
        case '4':
            branch_qualifier[0] = '5';
            ret_value = TRUE;
            break;
        case '5':
            break;
        case '6':
            branch_qualifier[0] = '7';
            ret_value = TRUE;
            break;
        case '7':
            break;
        case '8':
            branch_qualifier[0] = '9';
            ret_value = TRUE;
            break;
        case '9':
            break;
        case 'a':
        case 'A':
            branch_qualifier[0] = 'B';
            ret_value = TRUE;
            break;
        case 'b':
        case 'B':
            break;
        case 'c':
        case 'C':
            branch_qualifier[0] = 'D';
            ret_value = TRUE;
            break;
        case 'd':
        case 'D':
            break;
        case 'e':
        case 'E':
            branch_qualifier[0] = 'F';
            ret_value = TRUE;
            break;
        case 'f':
        case 'F':
            break;
        default:
            LIXA_TRACE(("xta_xid_branch_qualifier_unset_multibranch: "
                        "branch_qualifier[0]='%c' (%s), this is an INTERNAL "
                        "ERROR\n", branch_qualifier[0], branch_qualifier));
    } /* switch (branch_qualifier[0]) */
    LIXA_TRACE(("xta_xid_branch_qualifier_unset_multibranch: "
                "branch_qualifier=%s\n", branch_qualifier));
    return ret_value;
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
                ret_cod = LIXA_RC_MALFORMED_XID;
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



xta_xid_t *xta_xid_new_from_XID(const XID *xid)
{
    enum Exception { NULL_OBJECT
                     , G_TRY_MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *this = NULL;
    
    LIXA_TRACE(("xta_xid_new_from_XID\n"));
    TRY {
        /* check the passed xid */
        if (NULL == xid)
            THROW(NULL_OBJECT);
        /* allocate the object */
        if (NULL == (this = (xta_xid_t *)
                     g_try_malloc0(sizeof(xta_xid_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* copy XID */
        memcpy(&this->xa_xid, xid, sizeof(XID));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* clean-up memory in the event of an error */
        if (NONE != excp) {
            g_free(this);
            this = NULL;
        }   
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_xid_new_from_XID/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



xta_xid_t *xta_xid_dup(const xta_xid_t *xid)
{
    enum Exception { NULL_OBJECT
                     , G_TRY_MALLOC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *this = NULL;
    
    LIXA_TRACE(("xta_xid_dup\n"));
    TRY {
        if (NULL == xid)
            THROW(NULL_OBJECT);
        /* allocate the object */
        if (NULL == (this = (xta_xid_t *)
                     g_try_malloc0(sizeof(xta_xid_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* copy da field */
        this->xa_xid = xid->xa_xid;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* clean-up memory in the event of an error */
        if (NONE != excp) {
            g_free(this);
            this = NULL;
        }   
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_xid_dup/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_xid_delete(xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xid_delete\n"));
    TRY {
        g_free(xid);
        
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



const XID *xta_xid_get_xa_xid(const xta_xid_t *xid)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    const XID *xa_xid = NULL;
    
    LIXA_TRACE(("xta_xid_get_xa_xid\n"));
    TRY {
        if (NULL == xid)
            THROW(NULL_OBJECT);
        xa_xid = &xid->xa_xid;
        
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



char *xta_xid_to_string(const xta_xid_t *xid)
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
        if (NULL == xid)
            THROW(NULL_OBJECT);
        /* allocate the dynamic memory */
        if (NULL == (string = malloc(sizeof(lixa_ser_xid_t))))
            THROW(MALLOC_ERROR);
        if (!lixa_xid_serialize(&xid->xa_xid, lsx))
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



void xta_xid_reset(xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_xid_reset\n"));
    TRY {
        lixa_xid_reset(&xid->xa_xid);
        
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



long xta_xid_get_gtrid(const xta_xid_t *xid, char *gtrid) {
    return lixa_xid_get_gtrid(&xid->xa_xid, gtrid);
}



long xta_xid_get_bqual(const xta_xid_t *xid, char *bqual) {
    return lixa_xid_get_bqual(&xid->xa_xid, bqual);
}



long xta_xid_get_formatID(const xta_xid_t *xid) {
    return lixa_xid_get_formatID(&xid->xa_xid);
}
