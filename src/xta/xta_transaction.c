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
#include "client_conn.h"
#include "client_config.h"
#include "lixa_xa.h"
/* XTA includes */
#include "xta_transaction.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_transaction_t *xta_transaction_new(void)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , CLIENT_CONFIG_DUP_ERROR
                     , CLIENT_CONFIG_DISPLAY_ERROR
                     , CLIENT_CONNECT_ERROR
                     , CLIENT_CONFIG_JOB_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_transaction_t *this = NULL;
    
    LIXA_TRACE(("xta_transaction_new\n"));
    TRY {
        /* allocate the object */
        if (NULL == (this = (xta_transaction_t *)
                     g_try_malloc0(sizeof(xta_transaction_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* initialize the LIXA client status */
        client_status_init(&this->client_status);
        client_status_active(&this->client_status);
        /*
         * copy the configuration for the LIXA client from the static one.
         * Static configuration must be initialized by xta_transaction_manager
         */
        if (LIXA_RC_OK != (ret_cod = client_config_dup(
                               &global_ccc, &this->local_ccc)))
            THROW(CLIENT_CONFIG_DUP_ERROR);
        /* display on trace the duplicated configuration */
        if (LIXA_RC_OK != (ret_cod = client_config_display(&this->local_ccc)))
            THROW(CLIENT_CONFIG_DISPLAY_ERROR);
        /* connect to LIXA state server */
        if (LIXA_RC_OK != (ret_cod = client_connect(
                               &this->client_status, &this->local_ccc)))
            THROW(CLIENT_CONNECT_ERROR);
        /* configure the LIXA (transactional) job (if necessary) */
        if (LIXA_RC_OK != (ret_cod = client_config_job(
                               &this->local_ccc,
                               client_status_get_sockfd(
                                   &this->client_status))))
            THROW(CLIENT_CONFIG_JOB_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case CLIENT_CONFIG_DUP_ERROR:
            case CLIENT_CONFIG_DISPLAY_ERROR:
            case CLIENT_CONNECT_ERROR:
            case CLIENT_CONFIG_JOB_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* if something went wrong, release allocated memory and return
           NULL */
        if (excp < NONE) {
            LIXA_TRACE(("xta_transaction_new: an internal error "
                        "occurred, releasing allocated memory...\n"));
            if (excp > G_TRY_MALLOC_ERROR) {
                LIXA_TRACE(("xta_transaction_new: releasing client "
                            "status objects...\n"));
                /* free the memory associated to client status */
                client_status_free(&this->client_status);
            }
            if (excp > CLIENT_CONFIG_DUP_ERROR) {
                LIXA_TRACE(("xta_transaction_new: releasing "
                            "configuration objects...\n"));
                /* free the memory associated to client configuration */
                client_unconfig(&this->local_ccc, FALSE);
            }
            if (excp > G_TRY_MALLOC_ERROR) {
                LIXA_TRACE(("xta_transaction_new: an internal error "
                            "occurred, destroying this object and returning "
                            "NULL\n"));
                /* free the memory associated to this object */
                g_free(this);
                this = NULL;
            }
        } /* if (excp < NONE) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_transaction_delete(xta_transaction_t *this)
{
    enum Exception { CLIENT_UNCONFIG_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_delete: destroying %p...\n", this));
    TRY {
        /* unconfigure and release the memory related to client configuration
           collection */
        if (LIXA_RC_OK != (ret_cod = client_unconfig(&this->local_ccc, FALSE)))
            THROW(CLIENT_UNCONFIG_ERROR);
        
        /* free the memory associated to client status */
        client_status_free(&this->client_status);

        /* release the memory of this object */
        g_free(this);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_UNCONFIG_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



xta_transaction_config_t *xta_transaction_get_config(xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_config_coll_t *ccc = NULL;
    
    LIXA_TRACE(("xta_transaction_get_config\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        ccc = &this->local_ccc;
        
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
    LIXA_TRACE(("xta_transaction_get_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ccc;
}



int xta_transaction_enlist_resource(xta_transaction_t *this,
                                    xta_xa_resource_t *xa_res)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , INVALID_STATUS
                     , NULL_OBJECT3
                     , G_TRY_MALLOC_ERROR
                     , CLIENT_CONFIG_DUP_ERROR
                     , REDIGEST_ERROR
                     , XA_RESOURCE_REGISTERED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    const xta_xa_resource_config_t *config = NULL;
    struct rsrmgr_config_s *rsrmgr = NULL;
    struct act_rsrmgr_config_s act_rsrmgr;
    
    LIXA_TRACE(("xta_transaction_enlist_resource\n"));
    TRY {
        /* check the transaction object is not NULL */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the XA Resource object is not NULL */
        if (NULL == xa_res)
            THROW(NULL_OBJECT2);
        /* check transaction state before going on */
        if (TX_STATE_S0 != client_status_get_txstate(&this->client_status)) {
            LIXA_TRACE(("xta_transaction_enlist_resource: expected client "
                        "status %d, current client status %d\n", TX_STATE_S0,
                        client_status_get_txstate(&this->client_status)));
            THROW(INVALID_STATUS);
        }
        /* if the XA Resource is not dynamic, the following steps are not
         * necessary */
        if (xta_xa_resource_is_dynamic(xa_res)) {
            /* retrieve the configuration related to the XA resource that's
             * registering to this transaction manager */
            if (NULL == (config = xta_xa_resource_get_config(xa_res)))
                THROW(NULL_OBJECT3);
            /* allocate a new record for the Resource Manager description */
            if (NULL == (rsrmgr = g_try_malloc(
                             sizeof(struct rsrmgr_config_s))))
                THROW(G_TRY_MALLOC_ERROR);
            /* duplicate the configuration structs to avoid dependency from the
             * resource object (it's necessary to preserve compatibility with
             * the LIXA legacy non object oriented legacy functions */
            if (LIXA_RC_OK != (ret_cod = client_config_rsrmgr_dup(
                                   config, rsrmgr, &act_rsrmgr)))
                THROW(CLIENT_CONFIG_DUP_ERROR);
            /* append the resource manager to the list of actual configured
               resource managers */
            client_config_append_rsrmgr(&this->local_ccc, rsrmgr,
                                        &act_rsrmgr);
            /* compute again the configuration digest (fingerprint) because
               a new resource has been added */
            if (LIXA_RC_OK != (ret_cod = xta_transaction_redigest(
                                   this, config)))
                THROW(REDIGEST_ERROR);
            /* reset the record pointer */
            rsrmgr = NULL;
        } else {
            LIXA_TRACE(("xta_transaction_enlist_resource: this is a static "
                        "resource, skipping config dup...\n"));
        } /* if (!xta_xa_resource_is_dynamic(xa_res)) */
        /* send a registration message to the XA Resource */
        if (LIXA_RC_OK != (ret_cod = xta_xa_resource_registered(
                               xa_res, this)))
            THROW(XA_RESOURCE_REGISTERED);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
            case NULL_OBJECT3:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case REDIGEST_ERROR:
                break;
            case CLIENT_CONFIG_DUP_ERROR:
                break;
            case XA_RESOURCE_REGISTERED:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recovery memory if necessary */
        if (NULL != rsrmgr) {
            LIXA_TRACE(("xta_transaction_enlist_resource: recoverying "
                        "record memory\n"));
            g_free(rsrmgr);
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_enlist_resource/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_redigest(xta_transaction_t *this,
                             const xta_xa_resource_config_t *xrc)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , NULL_OBJECT3
                     , G_CHECKSUM_NEW_ERROR
                     , G_CHECKSUM_GET_STRING_ERROR
                     , JOB_SET_SOURCE_IP_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    gchar *config_tostring = NULL;
    GChecksum *checksum = NULL;
    
    LIXA_TRACE(("xta_transaction_redigest\n"));
    TRY {
        const gchar *checksum_string = NULL;
        
        /* check the transaction is not NULL */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the configuration is not NULL */
        if (NULL == xrc)
            THROW(NULL_OBJECT2);
        /* retrieve the serialized version of the configuration */
        if (NULL == (config_tostring = client_config_tostring_rsrmgr(xrc)))
            THROW(NULL_OBJECT3);
        /* trace old values */
        LIXA_TRACE(("xta_transaction_redigest: old digest is '%s'\n",
                    this->local_ccc.config_digest));
        LIXA_TRACE(("xta_transaction_redigest: old job id for this "
                    "transaction is '%s'\n",
                    lixa_job_get_raw(this->local_ccc.job)));
        /* create a new checksum */
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR);
        /* use current digest as the initial content */
        g_checksum_update(checksum, (guchar *)this->local_ccc.config_digest,
                          sizeof(md5_digest_hex_t));
        /* append the config of the current resource */
        g_checksum_update(checksum, (guchar *)config_tostring,
                          strlen(config_tostring));
        if (NULL == (checksum_string = g_checksum_get_string(checksum)))
            THROW(G_CHECKSUM_GET_STRING_ERROR);
        /* copy back the new digest */
        strncpy(this->local_ccc.config_digest, (const char *)checksum_string,
                MD5_DIGEST_LENGTH * 2);
        this->local_ccc.config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
        /* re compute LIXA job id for this transaction */
        lixa_job_reset(this->local_ccc.job);
        lixa_job_set_config_digest(this->local_ccc.job,
                                   this->local_ccc.config_digest);
        if (LIXA_RC_OK != (ret_cod = lixa_job_set_source_ip(
                               this->local_ccc.job,
                               client_status_get_sockfd(
                                   &this->client_status))))
            THROW(JOB_SET_SOURCE_IP_ERROR);
        /* trace new values */
        LIXA_TRACE(("xta_transaction_redigest: new digest is '%s'\n",
                    this->local_ccc.config_digest));
        LIXA_TRACE(("xta_transaction_redigest: new job id for this "
                    "transaction is '%s'\n",
                    lixa_job_get_raw(this->local_ccc.job)));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
            case NULL_OBJECT3:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_CHECKSUM_NEW_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
            case G_CHECKSUM_GET_STRING_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_GET_STRING_ERROR;
                break;
            case JOB_SET_SOURCE_IP_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover dynamically allocated memory */
        if (NULL != config_tostring)
            g_free(config_tostring);
        if (NULL != checksum)
            g_checksum_free(checksum);
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_redigest/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


int xta_transaction_begin(xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , LIXA_XA_OPEN_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_begin\n"));
    TRY {
        int next_txstate, txrc;
        
        /* check object */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        if (TX_STATE_S0 != client_status_get_txstate(&this->client_status)) {
            LIXA_TRACE(("xta_transaction_begin: expected client status %d, "
                        "current client status %d\n", TX_STATE_S0,
                        client_status_get_txstate(&this->client_status)));
            THROW(INVALID_STATUS);
        }
        next_txstate = TX_STATE_S1;
        /* open statically defined XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_open(
                               &this->local_ccc, &this->client_status,
                               &txrc, next_txstate, FALSE)))
            THROW(LIXA_XA_OPEN_ERROR);
        /* set new state after RMs are open... */
        client_status_set_txstate(&this->client_status, next_txstate);

        /* @@@ grab logic from lixa_tx_begin */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_OPEN_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_begin/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_commit(xta_transaction_t *transaction)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_commit\n"));
    TRY {
        /* @@@ */
        
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
    LIXA_TRACE(("xta_transaction_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_rollback(xta_transaction_t *transaction)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_rollback\n"));
    TRY {
        /* @@@ */
        
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
    LIXA_TRACE(("xta_transaction_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_suspend(xta_transaction_t *transaction,
                                   long flags)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_suspend\n"));
    TRY {
        /* @@@ */
        
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
    LIXA_TRACE(("xta_transaction_suspend/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_resume(xta_transaction_t *transaction,
                           const xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_resume\n"));
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
    LIXA_TRACE(("xta_transaction_resume/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_branch(xta_transaction_t *transaction,
                           const xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_branch\n"));
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
    LIXA_TRACE(("xta_transaction_branch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



const xta_xid_t *xta_transaction_get_xid(const xta_transaction_t *t)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *xid = NULL;
    
    LIXA_TRACE(("xta_transaction_get_xid\n"));
    TRY {
        /* @@@ */
        
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
    LIXA_TRACE(("xta_transaction_get_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return xid;
}

