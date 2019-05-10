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



/* system includes */
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif



/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
#include "client_conn.h"
#include "client_config.h"
#include "client_status.h"
#include "lixa_xa.h"
#include "lixa_syslog.h"
/* XTA includes */
#include "xta_transaction.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



xta_transaction_t *xta_transaction_new(void)
{
    enum Exception { G_TRY_MALLOC_ERROR1
                     , G_TRY_MALLOC_ERROR2
                     , G_TRY_MALLOC_ERROR3
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
            THROW(G_TRY_MALLOC_ERROR1);
        /* reset already_open flag */
        this->already_opened = FALSE;
        /* reset multiple_branches flag */
        this->multiple_branches = FALSE;
        /* allocate a client_status object */
        if (NULL == (this->client_status =
                     g_try_malloc0(sizeof(client_status_t))))
            THROW(G_TRY_MALLOC_ERROR2);
        /* initialize the LIXA client status */
        client_status_init(this->client_status);
        client_status_active(this->client_status);
        /* allocate a client_config_coll to store a local copy of the
         * global configuration */
        if (NULL == (this->local_ccc =
                     g_try_malloc0(sizeof(client_config_coll_t))))
            THROW(G_TRY_MALLOC_ERROR3);
        /*
         * copy the configuration for the LIXA client from the static one.
         * Static configuration must be initialized by xta_transaction_manager
         */
        if (LIXA_RC_OK != (ret_cod = client_config_dup(
                               &global_ccc, this->local_ccc)))
            THROW(CLIENT_CONFIG_DUP_ERROR);
        /* display on trace the duplicated configuration */
        if (LIXA_RC_OK != (ret_cod = client_config_display(this->local_ccc)))
            THROW(CLIENT_CONFIG_DISPLAY_ERROR);
        /* connect to LIXA state server */
        if (LIXA_RC_OK != (ret_cod = client_connect(
                               this->client_status, this->local_ccc))) {
            LIXA_TRACE(("xta_transaction_new/client_connect: ret_cod=%d "
                        "('%s')\n", ret_cod, lixa_strerror(ret_cod)));
            THROW(CLIENT_CONNECT_ERROR);
        }
        /* configure the LIXA (transactional) job (if necessary) */
        if (LIXA_RC_OK != (ret_cod = client_config_job(
                               this->local_ccc, client_status_get_sockfd(
                                   this->client_status))))
            THROW(CLIENT_CONFIG_JOB_ERROR);
        /* reset the XID reference */
        this->xid = NULL;
        /* reset flag */
        this->commit_suspended = FALSE;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR1:
            case G_TRY_MALLOC_ERROR2:
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
            if (excp > G_TRY_MALLOC_ERROR2) {
                LIXA_TRACE(("xta_transaction_new: releasing client "
                            "status objects...\n"));
                /* free the memory associated to client status */
                client_status_free(this->client_status);
            }
            if (excp > CLIENT_CONFIG_DUP_ERROR) {
                LIXA_TRACE(("xta_transaction_new: releasing "
                            "configuration objects...\n"));
                /* free the memory associated to client configuration */
                client_unconfig(this->local_ccc, FALSE);
            }
            if (excp > G_TRY_MALLOC_ERROR3) {
                LIXA_TRACE(("xta_transaction_new: an internal error "
                            "occurred, destroying local_ccc object and "
                            "returning NULL\n"));
                /* free the memory associated to this object */
                g_free(this->local_ccc);
                this->local_ccc = NULL;
            }
            if (excp > G_TRY_MALLOC_ERROR2) {
                LIXA_TRACE(("xta_transaction_new: an internal error "
                            "occurred, destroying client_status object and "
                            "returning NULL\n"));
                /* free the memory associated to this object */
                g_free(this->client_status);
                this->client_status = NULL;
            }
            if (excp > G_TRY_MALLOC_ERROR1) {
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



void xta_transaction_delete(xta_transaction_t *transact)
{
    enum Exception { CLIENT_UNCONFIG_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_delete: destroying %p...\n", transact));
    TRY {
        /* call close if not already done */
        if (transact->already_opened)
            if (LIXA_RC_OK != (ret_cod = xta_transaction_close_internal(
                                   transact))) {
                LIXA_TRACE(("xta_transaction_delete/"
                            "xta_transaction_close_internal: ret_cod=%d\n",
                            ret_cod));
            }
        /* close the client connection */
        if (LIXA_RC_OK != (ret_cod = client_disconnect(
                               transact->client_status))) {
            LIXA_TRACE(("xta_transaction_delete/client_disconnect: "
                        "rec_cod=%d\n", ret_cod));
        }
        /* unconfigure and release the memory related to client configuration
           collection */
        if (LIXA_RC_OK != (ret_cod = client_unconfig(
                               transact->local_ccc, FALSE)))
            THROW(CLIENT_UNCONFIG_ERROR);
        /* free the memory associated to local_ccc */
        g_free(transact->local_ccc);
        
        /* free the memory associated to client status */
        client_status_free(transact->client_status);
        g_free(transact->client_status);

        /* release the memory of the XID */
        if (NULL != transact->xid)
            xta_xid_delete(transact->xid);
        
        /* release the memory of this object */
        g_free(transact);
        
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



int xta_transaction_safe_delete(const xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , INTERNAL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int result = FALSE;
    
    LIXA_TRACE(("xta_transaction_safe_delete\n"));
    TRY {
        int txstate;
        
        if (NULL == transact)
            THROW(NULL_OBJECT);
        txstate = client_status_get_txstate(transact->client_status);
        switch (txstate) {
            /* these states are safe */
            case TX_STATE_S0:
            case TX_STATE_S1:
                result = TRUE;
                break;
            /* these states should never be used with XTA */
            case TX_STATE_S2:
            case TX_STATE_S4:
                THROW(INVALID_STATUS);
                break;
            /* these states are not safe */
            case TX_STATE_S3:
            case TX_STATE_S5:
                break;
            default:
                THROW(INTERNAL_ERROR);
        } /* switch (txstate) */
        LIXA_TRACE(("xta_transaction_safe_delete: status=%d, result=%d\n",
                    txstate, result));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
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
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_safe_delete/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return result;
}



xta_config_t *xta_transaction_get_config(xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_config_coll_t *ccc = NULL;
    
    LIXA_TRACE(("xta_transaction_get_config\n"));
    TRY {
        if (NULL == transact)
            THROW(NULL_OBJECT);
        ccc = transact->local_ccc;
        
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



int xta_transaction_enlist_resource(xta_transaction_t *transact,
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
        if (NULL == transact)
            THROW(NULL_OBJECT1);
        /* check the XA Resource object is not NULL */
        if (NULL == xa_res)
            THROW(NULL_OBJECT2);
        /* check transaction state before going on */
        if (TX_STATE_S0 != client_status_get_txstate(
                transact->client_status)) {
            LIXA_TRACE(("xta_transaction_enlist_resource: expected client "
                        "status %d, current client status %d\n", TX_STATE_S0,
                        client_status_get_txstate(transact->client_status)));
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
            client_config_append_rsrmgr(transact->local_ccc, rsrmgr,
                                        &act_rsrmgr);
            /* compute again the configuration digest (fingerprint) because
               a new resource has been added */
            if (LIXA_RC_OK != (ret_cod = xta_transaction_redigest(
                                   transact, config)))
                THROW(REDIGEST_ERROR);
            /* reset the record pointer */
            rsrmgr = NULL;
        } else {
            LIXA_TRACE(("xta_transaction_enlist_resource: this is a static "
                        "resource, skipping config dup...\n"));
        } /* if (!xta_xa_resource_is_dynamic(xa_res)) */
        /* send a registration message to the XA Resource */
        if (LIXA_RC_OK != (ret_cod = xta_xa_resource_enlisted(
                               xa_res, transact)))
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



int xta_transaction_redigest(xta_transaction_t *transact,
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
    client_config_coll_t *local_ccc = NULL;
    
    LIXA_TRACE(("xta_transaction_redigest\n"));
    TRY {
        const gchar *checksum_string = NULL;
        
        /* check the transaction is not NULL */
        if (NULL == transact)
            THROW(NULL_OBJECT1);
        local_ccc = (client_config_coll_t *)transact->local_ccc;
        /* check the configuration is not NULL */
        if (NULL == xrc)
            THROW(NULL_OBJECT2);
        /* retrieve the serialized version of the configuration */
        if (NULL == (config_tostring = client_config_tostring_rsrmgr(xrc)))
            THROW(NULL_OBJECT3);
        LIXA_TRACE(("xta_transaction_redigest: config_tostring = '%s'\n",
                    config_tostring));
        /* trace old values */
        LIXA_TRACE(("xta_transaction_redigest: old digest is '%s'\n",
                    local_ccc->config_digest));
        LIXA_TRACE(("xta_transaction_redigest: old job id for this "
                    "transaction is '%s'\n",
                    lixa_job_get_raw(local_ccc->job)));
        /* create a new checksum */
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR);
        /* use current digest as the initial content */
        g_checksum_update(checksum, (guchar *)local_ccc->config_digest,
                          sizeof(md5_digest_hex_t));
        /* append the config of the current resource */
        g_checksum_update(checksum, (guchar *)config_tostring,
                          strlen(config_tostring));
        if (NULL == (checksum_string = g_checksum_get_string(checksum)))
            THROW(G_CHECKSUM_GET_STRING_ERROR);
        /* copy back the new digest */
        strncpy(local_ccc->config_digest,
                (const char *)checksum_string, MD5_DIGEST_LENGTH * 2);
        local_ccc->config_digest[MD5_DIGEST_LENGTH * 2] = '\0';
        /* re compute LIXA job id for this transaction */
        lixa_job_reset(local_ccc->job);
        lixa_job_set_config_digest(local_ccc->job, local_ccc->config_digest);
        if (LIXA_RC_OK != (ret_cod = lixa_job_set_source_ip(
                               local_ccc->job,
                               client_status_get_sockfd(
                                   transact->client_status))))
            THROW(JOB_SET_SOURCE_IP_ERROR);
        /* trace new values */
        LIXA_TRACE(("xta_transaction_redigest: new digest is '%s'\n",
                    local_ccc->config_digest));
        LIXA_TRACE(("xta_transaction_redigest: new job id for this "
                    "transaction is '%s'\n",
                    lixa_job_get_raw(local_ccc->job)));
        
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



int xta_transaction_open(xta_transaction_t *transact)
{
    return xta_transaction_open_internal(transact);
}



int xta_transaction_open_internal(xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , LIXA_XA_OPEN_ERROR
                     , INTERNAL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_open_internal\n"));
    TRY {
        int next_txstate, txrc;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        if (TX_STATE_S0 != client_status_get_txstate(
                transact->client_status)) {
            LIXA_TRACE(("xta_transaction_open_internal: expected client "
                        "status %d, current client status %d\n", TX_STATE_S0,
                        client_status_get_txstate(transact->client_status)));
            THROW(INVALID_STATUS);
        }
        next_txstate = TX_STATE_S1;
        /* open statically defined XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_open(
                               transact->local_ccc, transact->client_status,
                               &txrc, next_txstate, FALSE)))
            THROW(LIXA_XA_OPEN_ERROR);
        transact->already_opened = TRUE;
        /* set new state after RMs are open... */
        client_status_set_txstate(transact->client_status, next_txstate);
        
        /* this should never happen! */
        if (NULL != transact->xid)
            THROW(INTERNAL_ERROR);

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
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_open_internal/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_close(xta_transaction_t *transact)
{
    return xta_transaction_close_internal(transact);
}



int xta_transaction_close_internal(xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , ALREADY_CLOSED
                     , PROTOCOL_ERROR
                     , INVALID_STATUS
                     , LIXA_XA_CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_close_internal\n"));
    TRY {
        int txstate, next_txstate, txrc;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        txstate = client_status_get_txstate(transact->client_status);
        switch (txstate) {
            case TX_STATE_S0:
                LIXA_TRACE(("xta_transaction_close_internal: already "
                            "close, skipping...\n"));
                THROW(ALREADY_CLOSED);
            case TX_STATE_S1:
                break;
            case TX_STATE_S3:
                THROW(PROTOCOL_ERROR);
            case TX_STATE_S5:
                break;
            default:
                THROW(INVALID_STATUS);
        }

        next_txstate = TX_STATE_S0;
        /* close statically defined XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_close(
                               transact->local_ccc, transact->client_status,
                               &txrc)))
            THROW(LIXA_XA_CLOSE_ERROR);
        /* reset already_opened flag */
        transact->already_opened = FALSE;
        /* set new state after RMs are open... */
        client_status_set_txstate(transact->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case ALREADY_CLOSED:
                ret_cod = LIXA_RC_OK;
                break;
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_CLOSE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_close_internal/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_recover(xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , OPEN_INTERNAL
                     , CLOSE_INTERNAL
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_recover\n"));
    TRY {
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT);
        /* open enlisted resources */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_open_internal(transact)))
            THROW(OPEN_INTERNAL);
        /* close enlisted resources */
        if (LIXA_RC_OK != (ret_cod = xta_transaction_close_internal(transact)))
            THROW(CLOSE_INTERNAL);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OPEN_INTERNAL:
            case CLOSE_INTERNAL:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_recover/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_start(xta_transaction_t *transact, int multiple_branches)
{
    enum Exception { NULL_OBJECT1
                     , NON_REUSABLE_TX
                     , OPEN_INTERNAL_ERROR
                     , INVALID_STATUS
                     , NULL_OBJECT2
                     , LIXA_XA_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_start\n"));
    TRY {
        int txstate, next_txstate, dupid_or_proto = FALSE;
        int txrc = 0;
        client_config_coll_t *local_ccc = NULL;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT1);
        local_ccc = (client_config_coll_t *)transact->local_ccc;
        /* multiple branches transaction require a new object */
        if (transact->already_opened && multiple_branches)
            THROW(NON_REUSABLE_TX);
        /* call open if not already done */
        if (!transact->already_opened)
            if (LIXA_RC_OK != (ret_cod = xta_transaction_open_internal(
                                   transact)))
                THROW(OPEN_INTERNAL_ERROR);
        /* check TX state */
        txstate = client_status_get_txstate(transact->client_status);
        if (TX_STATE_S1 != txstate) {
            LIXA_TRACE(("xta_transaction_start: expected client status %d, "
                        "current client status %d\n", TX_STATE_S1, txstate));
            THROW(INVALID_STATUS);
        } else
            next_txstate = TX_STATE_S3;
        /* create a new xid */
        if (NULL == (transact->xid = xta_xid_new(local_ccc->config_digest,
                                                 multiple_branches)))
            THROW(NULL_OBJECT2);
        /* set multiple branches */
        transact->multiple_branches = multiple_branches;
        /* start the transaction in all the XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
                               local_ccc, transact->client_status,
                               &txrc,
                               xta_xid_get_xa_xid(transact->xid), txstate,
                               next_txstate, &dupid_or_proto, TMNOFLAGS)))
            THROW(LIXA_XA_START_ERROR);        
        /* update the TX state */
        client_status_set_txstate(transact->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NON_REUSABLE_TX:
                ret_cod = LIXA_RC_NON_REUSABLE_TX;
                break;
            case OPEN_INTERNAL_ERROR:
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case LIXA_XA_START_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_start/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_commit(xta_transaction_t *transact, int non_blocking)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , GET_BQUAL_ASCII
                     , LIXA_XA_END_ERROR
                     , LIXA_XA_PREPARE_WAIT_BRANCHES_ERROR
                     , COMMIT_SUSPENDED
                     , LIXA_XA_PREPARE_ERROR
                     , LIXA_XA_COMMIT_ERROR
                     , INVALID_STATE1
                     , INVALID_TXRC1
                     , LIXA_XA_ROLLBACK_ERROR
                     , INVALID_STATE2
                     , INVALID_TXRC2
                     , LIXA_XA_FORGET_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    char *bqual = NULL;
    
    LIXA_TRACE(("xta_transaction_commit\n"));
    int txrc = TX_OK;
    TRY {
        int one_phase_commit = FALSE, commit = TRUE, finished = TRUE;
        int txstate, prepare_txrc, next_txstate;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT);
        /* check TX state */
        txstate = client_status_get_txstate(transact->client_status);
        if (TX_STATE_S3 != txstate) {
            LIXA_TRACE(("xta_transaction_commit: expected client status %d, "
                        "current client status %d\n", TX_STATE_S3, txstate));
            THROW(INVALID_STATUS);
        }
        if (transact->commit_suspended) {
            LIXA_TRACE(("xta_transaction_commit: a previously suspended "
                        "commit must be completed\n"));
        } else {
            /* check if One Phase Commit optimization can be applied */
            one_phase_commit = (client_status_could_one_phase(
                                    transact->client_status,
                                    transact->local_ccc));
            if (one_phase_commit) {
                /* check if the transaction has been create with the multiple
                   branches option */
                if (NULL == (bqual = lixa_xid_get_bqual_ascii(
                                 xta_xid_get_xa_xid(transact->xid))))
                    THROW(GET_BQUAL_ASCII);
                if (xta_xid_branch_qualifier_is_multibranch(bqual)) {
                    LIXA_TRACE(("xta_transaction_commit: forcing "
                                "one_phase_commit to FALSE because the branch "
                                "qualifier (%s) of this transaction is "
                                "related to a multiple branches transaction\n",
                                bqual));
                    one_phase_commit = FALSE;
                }
            } /* if (one_phase_commit) */
            /* detach the transaction */
            if (LIXA_RC_OK != (ret_cod = lixa_xa_end(
                                   transact->local_ccc,
                                   transact->client_status,
                                   xta_xid_get_xa_xid(transact->xid), &txrc,
                                   commit, TMSUCCESS))) {
                if (TX_ROLLBACK == txrc)
                    commit = FALSE;
                else
                    THROW(LIXA_XA_END_ERROR);
            } /* if (LIXA_RC_OK != (ret_cod = lixa_xa_end( */
        } /* if (transact->commit_suspended) */
        /* prepare (skip if we are rollbacking) */
        if (commit) {
            /* check if multiple branches prepare must be waited */
            if (transact->commit_suspended) {
                ret_cod = lixa_xa_prepare_wait_branches(
                    transact->local_ccc, transact->client_status);
                if (LIXA_RC_OTHER_BRANCH_ERROR == ret_cod) {
                    /* force rollback */
                    txrc = TX_ROLLBACK;
                    commit = FALSE;
                } else if (LIXA_RC_OK != ret_cod)
                    THROW(LIXA_XA_PREPARE_WAIT_BRANCHES_ERROR);
            } else if (!one_phase_commit) {
            /* bypass xa_prepare if one_phase_commit is TRUE or xa_prepare
             * has been already performed in the previous suspended commit */
                ret_cod = lixa_xa_prepare(
                    transact->local_ccc, transact->client_status,
                    xta_xid_get_xa_xid(transact->xid), non_blocking,
                    &txrc, &commit);
                switch (ret_cod) {
                    case LIXA_RC_OK:
                        break;
                    case LIXA_RC_WOULD_BLOCK:
                        transact->commit_suspended = TRUE;
                        THROW(COMMIT_SUSPENDED);
                        break;
                    case LIXA_RC_OTHER_BRANCH_ERROR: /* force rollback */
                        txrc = TX_ROLLBACK;
                        commit = FALSE;
                        break;
                    default:
                        THROW(LIXA_XA_PREPARE_ERROR);
                } /* switch (ret_cod) */
            } /* if (!one_phase_commit) */
        } /* if (commit) */
        prepare_txrc = txrc;
        /* commit or rollback the transaction */
        if (commit) {
            LIXA_TRACE(("xta_transaction_commit: go on with commit...\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_xa_commit(
                                   transact->local_ccc,
                                   transact->client_status,
                                   xta_xid_get_xa_xid(transact->xid),
                                   &txrc, one_phase_commit)))
                THROW(LIXA_XA_COMMIT_ERROR);
            switch (txrc) {
                case TX_OK:
                case TX_ROLLBACK:
                case TX_ERROR:
                case TX_MIXED:
                case TX_HAZARD:
                    next_txstate = TX_STATE_S1;
                    break;
                case TX_NO_BEGIN:
                case TX_ROLLBACK_NO_BEGIN:
                case TX_MIXED_NO_BEGIN:
                case TX_HAZARD_NO_BEGIN:
                    THROW(INVALID_STATE1);
                    break;
                case TX_FAIL:
                    next_txstate = txstate;
                    finished = FALSE;
                    break;
                default:
                    THROW(INVALID_TXRC1);
            } /* switch */
        } else {
            LIXA_TRACE(("xta_transaction_commit: go on with rollback...\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_xa_rollback(
                                   transact->local_ccc,
                                   transact->client_status,
                                   xta_xid_get_xa_xid(transact->xid),
                                   &txrc, TRUE)))
                THROW(LIXA_XA_ROLLBACK_ERROR);
            if (TX_FAIL == prepare_txrc) {
                LIXA_TRACE(("xta_transaction_commit: txrc=%d, "
                            "prepare_txrc=%d, "
                            "returning TX_FAIL to Application Program\n",
                            txrc, prepare_txrc));
                txrc = TX_FAIL;
            }
            LIXA_TRACE(("xta_transaction_commit: txrc=%d\n", txrc));
            switch (txrc) {
                case TX_OK:
                case TX_ROLLBACK:
                case TX_MIXED:
                case TX_HAZARD:
                    next_txstate = TX_STATE_S1;
                    break;
                case TX_NO_BEGIN:
                case TX_ROLLBACK_NO_BEGIN:
                case TX_MIXED_NO_BEGIN:
                case TX_HAZARD_NO_BEGIN:
                case TX_COMMITTED_NO_BEGIN:
                    THROW(INVALID_STATE2);
                    break;
                case TX_FAIL:
                    next_txstate = txstate;
                    finished = FALSE;
                    break;
                default:
                    THROW(INVALID_TXRC2);
            } /* switch */
        } /* else */

        /* clean Heurstically Completed states... */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_forget(
                               transact->local_ccc, transact->client_status,
                               xta_xid_get_xa_xid(transact->xid), finished)))
            THROW(LIXA_XA_FORGET_ERROR);
        
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(transact->client_status, next_txstate);
        /* reset the transaction id */
        xta_xid_reset(transact->xid);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case GET_BQUAL_ASCII:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case LIXA_XA_END_ERROR:
            case LIXA_XA_PREPARE_WAIT_BRANCHES_ERROR:
            case LIXA_XA_PREPARE_ERROR:
            case COMMIT_SUSPENDED:
            case LIXA_XA_COMMIT_ERROR:
                break;
            case INVALID_STATE1:
            case INVALID_TXRC1:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_ROLLBACK_ERROR:
                break;
            case INVALID_STATE2:
            case INVALID_TXRC2:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_FORGET_ERROR:
                break;
            case NONE:
                switch (txrc) {
                    case TX_OK:
                        ret_cod = LIXA_RC_OK;
                        break;
                    case TX_ROLLBACK:
                        ret_cod = LIXA_RC_TX_ROLLBACK;
                        break;
                    case TX_MIXED:
                        ret_cod = LIXA_RC_TX_MIXED;
                        break;
                    case TX_HAZARD:
                        ret_cod = LIXA_RC_TX_HAZARD;
                        break;
                    default:
                        ret_cod = LIXA_RC_INTERNAL_ERROR;
                } /* switch (txrc) */
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* LIXA backward compatibility, but it might be useless */
        if (TX_FAIL == txrc && NULL != transact->client_status)
            client_status_failed(transact->client_status);
    } /* TRY-CATCH */
    /* memory recovery */
    if (NULL != bqual) {
        free(bqual);
        bqual = NULL;
    }
    LIXA_TRACE(("xta_transaction_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_rollback(xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , LIXA_XA_END_ERROR
                     , LIXA_XA_ROLLBACK_ERROR
                     , INVALID_STATE
                     , INVALID_TXRC
                     , LIXA_XA_FORGET_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_rollback\n"));
    int txrc = TX_OK;
    TRY {
        int txstate, next_txstate, finished = TRUE;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT);
        /* check TX state */
        txstate = client_status_get_txstate(transact->client_status);
        if (TX_STATE_S3 != txstate) {
            LIXA_TRACE(("xta_transaction_commit: expected client status %d, "
                        "current client status %d\n", TX_STATE_S3, txstate));
            THROW(INVALID_STATUS);
        }
        /* detach the transaction */
        ret_cod = lixa_xa_end(transact->local_ccc, transact->client_status,
                              xta_xid_get_xa_xid(transact->xid), &txrc,
                              FALSE, TMSUCCESS);
        if (LIXA_RC_OK != ret_cod && TX_ROLLBACK != txrc)
            THROW(LIXA_XA_END_ERROR);
        /* rollback the transaction */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_rollback(
                               transact->local_ccc, transact->client_status,
                               xta_xid_get_xa_xid(transact->xid), &txrc,
                               FALSE)))
            THROW(LIXA_XA_ROLLBACK_ERROR);
        switch (txrc) {
            case TX_OK:
            case TX_MIXED:
            case TX_HAZARD:
            case TX_COMMITTED:
            case TX_ERROR:
                next_txstate = TX_STATE_S1;
                break;
            case TX_NO_BEGIN:
            case TX_MIXED_NO_BEGIN:
            case TX_HAZARD_NO_BEGIN:
            case TX_COMMITTED_NO_BEGIN:
                THROW(INVALID_STATE);
                break;
            case TX_FAIL:
                next_txstate = txstate;
                finished = FALSE;
                break;
            default:
                THROW(INVALID_TXRC);
        } /* switch */

        /* clean Heurstically Completed states... */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_forget(
                               transact->local_ccc, transact->client_status,
                               xta_xid_get_xa_xid(transact->xid), finished)))
            THROW(LIXA_XA_FORGET_ERROR);
        
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(transact->client_status, next_txstate);
        /* reset the transaction id */
        xta_xid_reset(transact->xid);
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_END_ERROR:
            case LIXA_XA_ROLLBACK_ERROR:
                break;
            case INVALID_STATE:
            case INVALID_TXRC:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_FORGET_ERROR:
                break;
            case NONE:
                switch (txrc) {
                    case TX_OK:
                    case TX_ROLLBACK:
                        ret_cod = LIXA_RC_OK;
                        break;
                    case TX_MIXED:
                        ret_cod = LIXA_RC_TX_MIXED;
                        break;
                    case TX_HAZARD:
                        ret_cod = LIXA_RC_TX_HAZARD;
                        break;
                    default:
                        ret_cod = LIXA_RC_INTERNAL_ERROR;
                } /* switch (txrc) */
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_rollback/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_suspend(xta_transaction_t *transact, long flags)
{
    enum Exception { NULL_OBJECT
                     , PROTOCOL_ERROR
                     , INVALID_STATUS
                     , INVALID_FLAGS
                     , LIXA_XA_END_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_suspend\n"));
    TRY {
        int txstate, next_txstate, txrc;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        txstate = client_status_get_txstate(transact->client_status);
        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
                THROW(PROTOCOL_ERROR);
            case TX_STATE_S3:
                break;
            default:
                THROW(INVALID_STATUS);
        }
        /* check flags */
        if (TMMIGRATE == flags) {
            LIXA_TRACE(("xta_transaction_suspend: flags=TMMIGRATE, "
                        "suspend for resume\n"));
        } else if (TMNOFLAGS == flags) {
            LIXA_TRACE(("xta_transaction_suspend: flags=TMNOFLAGS, "
                        "suspend for join\n"));
        } else
            THROW(INVALID_FLAGS);
        /* add suspend flag */
        flags |= TMSUSPEND;
        /* detach the transaction */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_end(
                               transact->local_ccc, transact->client_status,
                               xta_xid_get_xa_xid(transact->xid), &txrc,
                               FALSE, flags)))
            THROW(LIXA_XA_END_ERROR);
        
        next_txstate = TX_STATE_S5;
        /* set new state after RMs are suspended... */
        client_status_set_txstate(transact->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case INVALID_FLAGS:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case LIXA_XA_END_ERROR:
                break;
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



int xta_transaction_resume(xta_transaction_t *transact,
                           const char *xid_string, long flags)
{
    enum Exception { NULL_OBJECT1
                     , INVALID_STATUS
                     , OPEN_INTERNAL_ERROR
                     , PROTOCOL_ERROR
                     , INVALID_FLAGS
                     , NULL_OBJECT2
                     , LIXA_XA_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_resume\n"));
    TRY {
        int txstate, next_txstate, dupid_or_proto = FALSE;
        int txrc = 0;
        
        /* check object and serialized XID */
        if (NULL == transact || NULL == xid_string)
            THROW(NULL_OBJECT1);
        /* check current XID is not set */
        if (NULL != transact->xid)
            THROW(INVALID_STATUS);
        /* call open if not already done */
        if (!transact->already_opened)
            if (LIXA_RC_OK != (ret_cod = xta_transaction_open_internal(
                                   transact)))
                THROW(OPEN_INTERNAL_ERROR);
        /* check transaction state before going on */
        txstate = client_status_get_txstate(transact->client_status);
        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S3:
                THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S5:
                break;
            default:
                THROW(PROTOCOL_ERROR);
        }
        next_txstate = TX_STATE_S3;
        /* check flags */
        if (TMRESUME == flags) {
            LIXA_TRACE(("xta_transaction_resume: flags=TMRESUME, "
                        "resuming after possible migration\n"));
        } else if (TMJOIN == flags) {
            LIXA_TRACE(("xta_transaction_resume: flags=TMJOIN, "
                        "joining an existent transaction\n"));
        } else
            THROW(INVALID_FLAGS);
        /* deserialize the passed XID */
        if (NULL == (transact->xid = xta_xid_new_from_string(xid_string)))
            THROW(NULL_OBJECT2);
        /* start the transaction in all the XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
                               transact->local_ccc, transact->client_status,
                               &txrc,
                               xta_xid_get_xa_xid(transact->xid), txstate,
                               next_txstate, &dupid_or_proto, flags)))
            THROW(LIXA_XA_START_ERROR);        
        /* update the TX state */
        client_status_set_txstate(transact->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case OPEN_INTERNAL_ERROR:
                break;
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_FLAGS:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case LIXA_XA_START_ERROR:
                break;
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



int xta_transaction_branch(xta_transaction_t *transact, const char *xid_string)
{
    enum Exception { NULL_OBJECT1
                     , OPEN_INTERNAL_ERROR
                     , INVALID_STATUS
                     , XID_DESERIALIZE
                     , GET_BQUAL_ASCII
                     , NON_BRANCHABLE_TX
                     , XID_SERIALIZE
                     , NULL_OBJECT2
                     , LIXA_XA_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int warning = LIXA_RC_OK;
    char *bqual = NULL;
    
    LIXA_TRACE(("xta_transaction_branch\n"));
    TRY {
        XID superior, subordinate;
        lixa_ser_xid_t lsx;
        int txstate, next_txstate, dupid_or_proto = FALSE;
        int txrc = 0;
        
        /* check object */
        if (NULL == transact)
            THROW(NULL_OBJECT1);
        /* call open if not already done */
        if (!transact->already_opened)
            if (LIXA_RC_OK != (ret_cod = xta_transaction_open_internal(
                                   transact)))
                THROW(OPEN_INTERNAL_ERROR);        
        /* check TX state */
        txstate = client_status_get_txstate(transact->client_status);
        if (TX_STATE_S1 != txstate) {
            LIXA_TRACE(("xta_transaction_branch: expected client status %d, "
                        "current client status %d\n", TX_STATE_S1, txstate));
            THROW(INVALID_STATUS);
        } else
            next_txstate = TX_STATE_S3;
        
        /* deserialize superior XID */
        if (!lixa_xid_deserialize(&superior, xid_string)) {
            LIXA_TRACE(("xta_transaction_branch: unable to deserialize "
                        "superior XID '%s'\n", xid_string));
            THROW(XID_DESERIALIZE);
        }
        /* retrieve the branch qualifier to check it's for multiple branches */
        if (NULL == (bqual = lixa_xid_get_bqual_ascii(&superior)))
            THROW(GET_BQUAL_ASCII);
        if (!xta_xid_branch_qualifier_is_multibranch(bqual)) {
            LIXA_TRACE(("xta_transaction_branch: this transaction "
                        "(xid=%s) can't be branched because bqual (%s) is "
                        "related to a non branchable transaction\n",
                        xid_string, bqual));
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXC031E, xid_string, bqual));
            THROW(NON_BRANCHABLE_TX);
        }
        /* generate subordinate XID */
        lixa_xid_branch_new(&superior, &subordinate);
        /* serialize subordiante XID */
        if (!lixa_xid_serialize(&subordinate, lsx)) {
            LIXA_TRACE(("xta_transaction_branch: unable to serialize "
                        "subordinate XID\n"));
            THROW(XID_SERIALIZE);
        }
        /* debug message */
        LIXA_TRACE(("xta_transaction_branch: superior XID is '%s', "
                    "subordinate XID is '%s'\n", xid_string, lsx));        
        /* create a new xid */
        if (NULL == (transact->xid = xta_xid_new_from_XID(&subordinate)))
            THROW(NULL_OBJECT2);
        /* start the transaction in all the XA Resource Managers as a new
         * branch under the scope of an existing global transaction */
        ret_cod = lixa_xa_start(transact->local_ccc, transact->client_status,
                                &txrc,
                                xta_xid_get_xa_xid(transact->xid), txstate,
                                next_txstate, &dupid_or_proto, TMXTABRANCH);
        switch (ret_cod) {
            case LIXA_RC_OK:
                break;
            case LIXA_RC_NO_SUPERIOR_BRANCH:
                /* set the warning condition, but the transaction can go on */
                warning = ret_cod;
                break;
            default:
                THROW(LIXA_XA_START_ERROR);        
        } /* switch (ret_cod) */
        /* update the TX state */
        client_status_set_txstate(transact->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OPEN_INTERNAL_ERROR:
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XID_DESERIALIZE:
            case XID_SERIALIZE:
            case GET_BQUAL_ASCII:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case NON_BRANCHABLE_TX:
                ret_cod = LIXA_RC_NON_BRANCHABLE_TX;
                break;
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case LIXA_XA_START_ERROR:
                break;
            case NONE:
                ret_cod = warning;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* memory recovery */
    if (NULL != bqual) {
        free(bqual);
        bqual = NULL;
    }
    LIXA_TRACE(("xta_transaction_branch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



const xta_xid_t *xta_transaction_get_xid(const xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *xid = NULL;
    
    LIXA_TRACE(("xta_transaction_get_xid\n"));
    TRY {
        if (NULL == transact)
            THROW(NULL_OBJECT);
        xid = transact->xid;
        
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
    LIXA_TRACE(("xta_transaction_get_xid/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return xid;
}



int xta_transaction_get_multiple_branches(
    const xta_transaction_t *transact)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int result = FALSE;
    
    LIXA_TRACE(("xta_transaction_get_multiple_branches\n"));
    TRY {
        if (NULL == transact)
            THROW(NULL_OBJECT);
        result = transact->multiple_branches;
        
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
    LIXA_TRACE(("xta_transaction_get_multiple_branches/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return result;
}

