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
/* XTA includes */
#include "xta_transaction_manager.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_transaction_manager_t *xta_transaction_manager_new(void)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , CLIENT_CONFIG_ERROR
                     , CLIENT_CONNECT_ERROR
                     , CLIENT_CONFIG_JOB_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_transaction_manager_t *this = NULL;
    
    /* activate tracing */
    LIXA_TRACE_INIT;
    
    LIXA_TRACE(("xta_transaction_manager_new\n"));
    TRY {
        /* allocate the object */
        if (NULL == (this = (xta_transaction_manager_t *)
                     g_try_malloc0(sizeof(xta_transaction_manager_t))))
            THROW(G_TRY_MALLOC_ERROR);
        /* initialize the mutex */
        g_mutex_init(&this->mutex);
        /* initialize the LIXA client status */
        client_status_init(&this->client_status);
        client_status_active(&this->client_status);
        /* configure the LIXA client (if necessary) */
        if (LIXA_RC_OK != (ret_cod = client_config(&this->local_ccc, FALSE)))
            THROW(CLIENT_CONFIG_ERROR);
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
            case CLIENT_CONFIG_ERROR:
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
            LIXA_TRACE(("xta_transaction_manager_new: an internal error "
                        "occurred, releasing allocated memory...\n"));
            if (excp > G_TRY_MALLOC_ERROR) {
                LIXA_TRACE(("xta_transaction_manager_new: releasing client "
                            "status objects...\n"));
                /* free the memory associated to client status */
                client_status_free(&this->client_status);
            }
            if (excp > CLIENT_CONFIG_ERROR) {
                LIXA_TRACE(("xta_transaction_manager_new: releasing "
                            "configuration objects...\n"));
                /* free the memory associated to client configuration */
                client_unconfig(&this->local_ccc, FALSE);
            }
            if (excp > G_TRY_MALLOC_ERROR) {
                LIXA_TRACE(("xta_transaction_manager_new: an internal error "
                            "occurred, destroying this object and returning "
                            "NULL\n"));
                /* free the memory associated to this object */
                g_free(this);
                this = NULL;
            }
        } /* if (excp < NONE) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_manager_new/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return this;
}



void xta_transaction_manager_delete(xta_transaction_manager_t *this)
{
    enum Exception { CLIENT_UNCONFIG_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_manager_delete\n"));
    TRY {
        /* destroy transaction objects if any */
        if (NULL != this->transactions) {
            g_hash_table_destroy(this->transactions);
            this->transactions = NULL;
        } /* if (NULL != this->transactions) */
        
        /* unconfigure and release the memory related to client configuration
           collection */
        if (LIXA_RC_OK != (ret_cod = client_unconfig(&this->local_ccc, FALSE)))
            THROW(CLIENT_UNCONFIG_ERROR);
        
        /* free the memory associated to client status */
        client_status_free(&this->client_status);

        /* clear the synchronization mutex */
        g_mutex_clear(&this->mutex);
        
        /* destroy the object itself */
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
    LIXA_TRACE(("xta_transaction_manager_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return;
}



xta_transaction_manager_config_t *
xta_transaction_manager_get_config(xta_transaction_manager_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_config_coll_t *ccc = NULL;
    
    LIXA_TRACE(("xta_transaction_manager_get_config\n"));
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
    LIXA_TRACE(("xta_transaction_manager_get_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ccc;
}



xta_transaction_t *
xta_transaction_manager_get_transaction(xta_transaction_manager_t *this)
{
    enum Exception { NULL_OBJECT
                     , OBJ_NOT_FOUND1
                     , OBJ_NOT_FOUND2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_transaction_t *t = NULL;
    
    LIXA_TRACE(("xta_transaction_manager_get_transaction\n"));
    TRY {
        GThread *self = g_thread_self();
        
        /* check object reference */
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        LIXA_TRACE(("xta_transaction_manager_get_transaction: "
                    "locking mutex...\n"));
        g_mutex_lock(&this->mutex);

        /* check hash table is allocated */
        if (NULL == this->transactions) {
            LIXA_TRACE(("xta_transaction_manager_get_transaction: there are "
                        "no started transactions\n"));
            THROW(OBJ_NOT_FOUND1);
        }

        /* look for transaction in hash table */
        if (NULL == (t = g_hash_table_lookup(this->transactions, self))) {
            LIXA_TRACE(("xta_transaction_manager_get_transaction: there is "
                        "not a started transactions for this thread\n"));
            THROW(OBJ_NOT_FOUND2);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OBJ_NOT_FOUND1:
            case OBJ_NOT_FOUND2:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* clear mutex */
        if (NULL_OBJECT < excp) {
            g_mutex_unlock(&this->mutex);
            LIXA_TRACE(("xta_transaction_manager_get_transaction: "
                        "mutex unlocked\n"));
        }
            
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_manager_get_transaction/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return t;
}



int xta_transaction_manager_register(xta_transaction_manager_t *this,
                                     xta_xa_resource_t *xa_res)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
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
    
    LIXA_TRACE(("xta_transaction_manager_register\n"));
    TRY {
        /* check the transaction manager object is not NULL */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the XA Resource object is not NULL */
        if (NULL == xa_res)
            THROW(NULL_OBJECT2);
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
            if (LIXA_RC_OK != (ret_cod = client_config_dup(
                                   config, rsrmgr, &act_rsrmgr)))
                THROW(CLIENT_CONFIG_DUP_ERROR);
            /* append the resource manager to the list of actual configured
               resource managers */
            client_config_append_rsrmgr(&this->local_ccc, rsrmgr,
                                        &act_rsrmgr);
            /* compute again the configuration digest (fingerprint) because
               a new resource has been added */
            if (LIXA_RC_OK != (ret_cod = xta_transaction_manager_redigest(
                                   this, config)))
                THROW(REDIGEST_ERROR);
            /* reset the record pointer */
            rsrmgr = NULL;
        } else {
            LIXA_TRACE(("xta_transaction_manager_register: this is a static "
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
            LIXA_TRACE(("xta_transaction_manager_register: recoverying "
                        "record memory\n"));
            g_free(rsrmgr);
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_manager_register/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_manager_redigest(xta_transaction_manager_t *this,
                                     const xta_xa_resource_config_t *xrc)
{
    enum Exception { NULL_OBJECT1
                     , NULL_OBJECT2
                     , NULL_OBJECT3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    gchar *config_tostring = NULL;
    
    LIXA_TRACE(("xta_transaction_manager_redigest\n"));
    TRY {
        /* check the transaction manager is not NULL */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        /* check the configuration is not NULL */
        if (NULL == xrc)
            THROW(NULL_OBJECT2);
        /* retrieve the serialized version of the configuration */
        if (NULL == (config_tostring = client_config_tostring_rsrmgr(xrc)))
            THROW(NULL_OBJECT3);
        /* @@@ implement something similar to lixa_config_digest with the
         * following steps:
         * 1: start with current digest
         * 2: append config_tostring
         * set the config digest again */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
            case NULL_OBJECT3:
                ret_cod = LIXA_RC_NULL_OBJECT;
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
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_manager_redigest/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_manager_begin(xta_transaction_manager_t *this)
{
    enum Exception { NULL_OBJECT1
                     , G_HASH_TABLE_NEW_ERROR
                     , TX_ALREADY_STARTED
                     , NULL_OBJECT2
                     , INTERNAL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_transaction_t *tx = NULL;
    
    LIXA_TRACE(("xta_transaction_manager_begin\n"));
    TRY {
        GThread *self = g_thread_self();
        GThread *lookup = NULL;

        /* check object reference */
        if (NULL == this)
            THROW(NULL_OBJECT1);

        LIXA_TRACE(("xta_transaction_manager_begin: locking mutex...\n"));
        g_mutex_lock(&this->mutex);
        
        /* check if the hash table as been already created */
        if (NULL == this->transactions) {
            if (NULL == (this->transactions = g_hash_table_new_full(
                             NULL, NULL, NULL,
                             (GDestroyNotify)xta_transaction_delete)))
                THROW(G_HASH_TABLE_NEW_ERROR);
        } /* if (NULL == this->transactions) */
        
        /* check if the current thread has already started a transaction */
        if (NULL != (lookup = g_hash_table_lookup(this->transactions, self))) {
            LIXA_TRACE(("xta_transaction_manager_begin: this thread has "
                        "already started a transaction and a second one "
                        "cannot be started, skipping...\n"));
            THROW(TX_ALREADY_STARTED);
        }

        /* allocate a new transaction object */
        if (NULL == (tx = xta_transaction_new()))
            THROW(NULL_OBJECT2);

        /* insert the transaction object in the hash map */
        if (!g_hash_table_insert(this->transactions, self, tx)) {
            THROW(INTERNAL_ERROR);
        }
        
        /* @@@ implement some code related to XID... */

        /* avoid memory recovery for this object that's OK */
        tx = NULL;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_HASH_TABLE_NEW_ERROR:
                ret_cod = LIXA_RC_G_HASH_TABLE_NEW_ERROR;
                break;
            case TX_ALREADY_STARTED:
                ret_cod = LIXA_RC_TX_ALREADY_STARTED;
                break;
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* memory recovery */
        if (NULL != tx)
            xta_transaction_delete(tx);
        /* mutex unlock */
        if (NULL_OBJECT1 < excp) {
            g_mutex_unlock(&this->mutex);
            LIXA_TRACE(("xta_transaction_manager_begin: mutex unlocked\n"));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_manager_begin/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_manager_branch(
    xta_transaction_manager_t *transaction_manager,
    const xta_xid_t *xid)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_manager_branch\n"));
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
    LIXA_TRACE(("xta_transaction_manager_branch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

