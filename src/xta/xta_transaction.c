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
        /* reset the XID reference */
        this->xid = NULL;
        
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

        /* release the memory of the XID */
        if (NULL != this->xid)
            xta_xid_delete(this->xid);
        
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
            client_config_append_rsrmgr(&this->local_ccc, rsrmgr, &act_rsrmgr);
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
        if (LIXA_RC_OK != (ret_cod = xta_xa_resource_enlisted(
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
        g_checksum_update(checksum,
                          (guchar *)this->local_ccc.config_digest,
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
    


int xta_transaction_open(xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , LIXA_XA_OPEN_ERROR
                     , INTERNAL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_open\n"));
    TRY {
        int next_txstate, txrc;
        
        /* check object */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        if (TX_STATE_S0 != client_status_get_txstate(&this->client_status)) {
            LIXA_TRACE(("xta_transaction_open: expected client status %d, "
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
        
        /* this should never happen! */
        if (NULL != this->xid)
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
    LIXA_TRACE(("xta_transaction_open/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_close(xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT
                     , PROTOCOL_ERROR
                     , INVALID_STATUS
                     , LIXA_XA_CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_close\n"));
    TRY {
        int txstate, next_txstate, txrc;
        
        /* check object */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        txstate = client_status_get_txstate(&this->client_status);
        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
                break;
            case TX_STATE_S3:
                THROW(PROTOCOL_ERROR);
            case TX_STATE_S5:
                break;
            default:
                THROW(INVALID_STATUS);
        }

        next_txstate = TX_STATE_S1;
        /* close statically defined XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_close(
                               &this->local_ccc, &this->client_status,
                               &txrc)))
            THROW(LIXA_XA_CLOSE_ERROR);
        /* set new state after RMs are open... */
        client_status_set_txstate(&this->client_status, next_txstate);
        txstate = next_txstate;
        next_txstate = TX_STATE_S3;
        
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
            case LIXA_XA_CLOSE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_start(xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT1
                     , INVALID_STATUS
                     , NULL_OBJECT2
                     , LIXA_XA_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xta_transaction_start\n"));
    TRY {
        int txstate, next_txstate, dupid_or_proto = FALSE;
        int txrc = 0;
        
        /* check object */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        
        /* check TX state */
        txstate = client_status_get_txstate(&this->client_status);
        if (TX_STATE_S1 != txstate) {
            LIXA_TRACE(("xta_transaction_start: expected client status %d, "
                        "current client status %d\n", TX_STATE_S1, txstate));
            THROW(INVALID_STATUS);
        } else
            next_txstate = TX_STATE_S3;
        /* create a new xid */
        if (NULL == (this->xid = xta_xid_new(this->local_ccc.config_digest)))
            THROW(NULL_OBJECT2);
        /* start the transaction in all the XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
                               &this->local_ccc, &this->client_status,
                               &txrc,
                               xta_xid_get_xa_xid(this->xid), txstate,
                               next_txstate, &dupid_or_proto, TMNOFLAGS)))
            THROW(LIXA_XA_START_ERROR);        
        /* update the TX state */
        client_status_set_txstate(&this->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
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



int xta_transaction_commit(xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT
                     , INVALID_STATUS
                     , LIXA_XA_END_ERROR
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
    
    LIXA_TRACE(("xta_transaction_commit\n"));
    int txrc = TX_OK;
    TRY {
        int one_phase_commit = FALSE, commit = TRUE, finished = TRUE;
        int txstate, prepare_txrc, next_txstate;
        
        /* check object */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check TX state */
        txstate = client_status_get_txstate(&this->client_status);
        if (TX_STATE_S3 != txstate) {
            LIXA_TRACE(("xta_transaction_commit: expected client status %d, "
                        "current client status %d\n", TX_STATE_S3, txstate));
            THROW(INVALID_STATUS);
        }
        /* check if One Phase Commit optimization can be applied */
        one_phase_commit = client_status_could_one_phase(
            &this->client_status, &this->local_ccc);
        /* detach the transaction */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_end(
                               &this->local_ccc, &this->client_status,
                               xta_xid_get_xa_xid(this->xid), &txrc,
                               commit, TMSUCCESS))) {
            if (TX_ROLLBACK == txrc)
                commit = FALSE;
            else
                THROW(LIXA_XA_END_ERROR);
        }
        /* prepare (skip if we are rollbacking) */
        if (commit) {
            /* bypass xa_prepare if one_phase_commit is TRUE */
            if (!one_phase_commit &&
                LIXA_RC_OK != (ret_cod = lixa_xa_prepare(
                                   &this->local_ccc, &this->client_status,
                                   xta_xid_get_xa_xid(this->xid),
                                   &txrc, &commit)))
                THROW(LIXA_XA_PREPARE_ERROR);
            LIXA_TRACE(("xta_transaction_commit: lixa_xa_prepare returned "
                        "txrc=%d\n", txrc));
        }
        prepare_txrc = txrc;
        /* commit or rollback the transaction */
        if (commit) {
            LIXA_TRACE(("xta_transaction_commit: go on with commit...\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_xa_commit(
                                   &this->local_ccc, &this->client_status,
                                   xta_xid_get_xa_xid(this->xid),
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
                                   &this->local_ccc, &this->client_status,
                                   xta_xid_get_xa_xid(this->xid),
                                   &txrc, TRUE)))
                THROW(LIXA_XA_ROLLBACK_ERROR);
            if (TX_FAIL == prepare_txrc) {
                LIXA_TRACE(("xta_transaction_commit: txrc=%d, "
                            "prepare_txrc=%d, "
                            "returning TX_FAIL to Application Program\n",
                            txrc, prepare_txrc));
                txrc = TX_FAIL;
            }
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
                               &this->local_ccc, &this->client_status,
                               xta_xid_get_xa_xid(this->xid), finished)))
            THROW(LIXA_XA_FORGET_ERROR);
        
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(&this->client_status, next_txstate);
        /* reset the transaction id */
        xta_xid_reset(this->xid);
        
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
            case LIXA_XA_PREPARE_ERROR:
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
        if (TX_FAIL == txrc && NULL != &this->client_status)
            client_status_failed(&this->client_status);
    } /* TRY-CATCH */
    LIXA_TRACE(("xta_transaction_commit/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int xta_transaction_rollback(xta_transaction_t *this)
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
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check TX state */
        txstate = client_status_get_txstate(&this->client_status);
        if (TX_STATE_S3 != txstate) {
            LIXA_TRACE(("xta_transaction_commit: expected client status %d, "
                        "current client status %d\n", TX_STATE_S3, txstate));
            THROW(INVALID_STATUS);
        }
        /* detach the transaction */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_end(
                               &this->local_ccc, &this->client_status,
                               xta_xid_get_xa_xid(this->xid), &txrc,
                               FALSE, TMSUCCESS)))
            THROW(LIXA_XA_END_ERROR);
        /* rollback the transaction */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_rollback(
                               &this->local_ccc, &this->client_status,
                               xta_xid_get_xa_xid(this->xid), &txrc, FALSE)))
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
                               &this->local_ccc, &this->client_status,
                               xta_xid_get_xa_xid(this->xid), finished)))
            THROW(LIXA_XA_FORGET_ERROR);
        
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(&this->client_status, next_txstate);
        /* reset the transaction id */
        xta_xid_reset(this->xid);
                
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



int xta_transaction_suspend(xta_transaction_t *this, long flags)
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
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check transaction state before going on */
        txstate = client_status_get_txstate(&this->client_status);
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
                               &this->local_ccc, &this->client_status,
                               xta_xid_get_xa_xid(this->xid), &txrc,
                               FALSE, flags)))
            THROW(LIXA_XA_END_ERROR);
        
        next_txstate = TX_STATE_S5;
        /* set new state after RMs are suspended... */
        client_status_set_txstate(&this->client_status, next_txstate);
        
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



int xta_transaction_resume(xta_transaction_t *this,
                           const char *xid_string, long flags)
{
    enum Exception { NULL_OBJECT1
                     , INVALID_STATUS
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
        if (NULL == this || NULL == xid_string)
            THROW(NULL_OBJECT1);
        /* check current XID is not set */
        if (NULL != this->xid)
            THROW(INVALID_STATUS);
        /* check transaction state before going on */
        txstate = client_status_get_txstate(&this->client_status);
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
        if (NULL == (this->xid = xta_xid_new_from_string(xid_string)))
            THROW(NULL_OBJECT2);
        /* start the transaction in all the XA Resource Managers */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
                               &this->local_ccc, &this->client_status,
                               &txrc,
                               xta_xid_get_xa_xid(this->xid), txstate,
                               next_txstate, &dupid_or_proto, flags)))
            THROW(LIXA_XA_START_ERROR);        
        /* update the TX state */
        client_status_set_txstate(&this->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
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



int xta_transaction_branch(xta_transaction_t *this, const char *xid_string)
{
    enum Exception { NULL_OBJECT1
                     , INVALID_STATUS
                     , XID_DESERIALIZE
                     , XID_SERIALIZE
                     , NULL_OBJECT2
                     , LIXA_XA_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int warning = LIXA_RC_OK;
    
    LIXA_TRACE(("xta_transaction_branch\n"));
    TRY {
        XID superior, subordinate;
        lixa_ser_xid_t lsx;
        int txstate, next_txstate, dupid_or_proto = FALSE;
        int txrc = 0;
        
        /* check object */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        
        /* check TX state */
        txstate = client_status_get_txstate(&this->client_status);
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
        if (NULL == (this->xid = xta_xid_new_from_XID(&subordinate)))
            THROW(NULL_OBJECT2);
        /* start the transaction in all the XA Resource Managers as a new
         * branch under the scope of an existing global transaction */
        ret_cod = lixa_xa_start(&this->local_ccc, &this->client_status,
                                &txrc,
                                xta_xid_get_xa_xid(this->xid), txstate,
                                next_txstate, &dupid_or_proto, TMXTABRANCH);
        switch (ret_cod) {
            case LIXA_RC_OK:
                break;
            case LIXA_RC_ERROR_FROM_SERVER_OFFSET+
                LIXA_RC_NO_SUPERIOR_BRANCH:
                /* set the warning condition, but the transaction can go on */
                warning = ret_cod;
                break;
            default:
                THROW(LIXA_XA_START_ERROR);        
        } /* switch (ret_cod) */
        /* update the TX state */
        client_status_set_txstate(&this->client_status, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XID_DESERIALIZE:
            case XID_SERIALIZE:
                ret_cod = LIXA_RC_MALFORMED_XID;
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
    LIXA_TRACE(("xta_transaction_branch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



const xta_xid_t *xta_transaction_get_xid(const xta_transaction_t *this)
{
    enum Exception { NULL_OBJECT
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    xta_xid_t *xid = NULL;
    
    LIXA_TRACE(("xta_transaction_get_xid\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        xid = this->xid;
        
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

