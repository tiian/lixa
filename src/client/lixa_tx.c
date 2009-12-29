/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <config.h>



#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_tx.h>
#include <lixa_xa.h>
#include <lixa_xml_msg.h>
#include <client_conn.h>
#include <client_config.h>
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



int lixa_tx_begin(int *txrc)
{
    enum Exception { STATUS_NOT_FOUND
                     , COLL_GET_CS_ERROR
                     , PROTOCOL_ERROR
                     , INVALID_STATUS1
                     , LIXA_XA_START_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int tmp_txrc = TX_OK;
    
    *txrc = TX_FAIL;
    
    LIXA_TRACE_INIT;
    LIXA_TRACE(("lixa_tx_begin\n"));
    TRY {
        int txstate, next_txstate;
        client_status_t *cs;
        XID xid;
        
        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                THROW(STATUS_NOT_FOUND);
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S1:
                next_txstate = TX_STATE_S3;
                break;
            case TX_STATE_S2:
                next_txstate = TX_STATE_S4;
                break;
            case TX_STATE_S0:
            case TX_STATE_S3:
            case TX_STATE_S4:
                THROW(PROTOCOL_ERROR);                
            default:
                THROW(INVALID_STATUS1);
        }
        LIXA_TRACE(("lixa_tx_begin: txstate = S%d, "
                    "next_txstate = S%d\n", txstate, next_txstate));

        /* generate the transction id */
        xid_create_new(&xid);
        client_status_set_xid(cs, &xid);
        
        /* the real logic must be put here */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
                               cs, txrc, &xid, next_txstate)))
            THROW(LIXA_XA_START_ERROR);
        
        /* @@@ go on from here... */
        
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(cs, next_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case STATUS_NOT_FOUND:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case COLL_GET_CS_ERROR:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS1:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_START_ERROR:
                break;
            case NONE:
                *txrc = tmp_txrc;
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_begin/TX_*=%d/excp=%d/"
                "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}



int lixa_tx_close(int *txrc)
{
    enum Exception { COLL_GET_CS_ERROR
                     , PROTOCOL_ERROR
                     , INVALID_STATUS
                     , LIXA_XA_CLOSE_ERROR
                     , CLIENT_DISCONNECT_ERROR
                     , CLIENT_CONFIG_UNLOAD_SWITCH_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int tmp_txrc = TX_OK;
    
    *txrc = TX_FAIL;
    
    LIXA_TRACE_INIT;
    LIXA_TRACE(("lixa_tx_close\n"));
    TRY {
        int txstate;
        client_status_t *cs;
        
        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                *txrc = TX_OK;
                /* break intentionally missed */
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
            case TX_STATE_S2:
                break;
            case TX_STATE_S3:
            case TX_STATE_S4:
                THROW(PROTOCOL_ERROR);                
            default:
                THROW(INVALID_STATUS);
        }
        
        /* the real logic must be put here */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_close(cs, &tmp_txrc)))
            THROW(LIXA_XA_CLOSE_ERROR);
            
        if (LIXA_RC_OK != (ret_cod = client_disconnect(&global_csc)))
            THROW(CLIENT_DISCONNECT_ERROR);

        /* @@@ due to a suspected memory leak inside glib discovered with
           valgrind, the modules are not unloaded: only process exit will
           unload them...
        if (LIXA_RC_OK != (ret_cod = client_config_unload_switch(&global_ccc)))
            THROW(CLIENT_CONFIG_UNLOAD_SWITCH_ERROR);
        */
            
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(cs, TX_STATE_S0);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_ERROR:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_CLOSE_ERROR:
                *txrc = tmp_txrc;
                break;
            case CLIENT_DISCONNECT_ERROR:
            case CLIENT_CONFIG_UNLOAD_SWITCH_ERROR:
                *txrc = TX_ERROR;
                break;
            case NONE:
                *txrc = tmp_txrc;
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_close/TX_*=%d/excp=%d/"
                "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}



int lixa_tx_open(int *txrc)
{
    enum Exception { CLIENT_STATUS_COLL_GET_CS_ERROR
                     , CLIENT_STATUS_COLL_REGISTER_ERROR
                     , CLIENT_CONFIG_ERROR
                     , CLIENT_CONNECT_ERROR
                     , COLL_GET_CS_ERROR
                     , LIXA_XA_OPEN_ERROR
                     , ALREADY_OPENED
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int tmp_txrc;
    
    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_TRACE(("lixa_tx_open\n"));    
    TRY {
        int txstate, next_txstate, pos = 0;
        client_status_t *cs;

        /* check if the thread is already registered and
         * retrieve a reference to the status of the current thread */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* already registered, nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND: /* first time, it must be registered */
                /* register this thread in library status */
                if (LIXA_RC_OK != (ret_cod = client_status_coll_register(
                                       &global_csc, &pos)))
                    THROW(CLIENT_STATUS_COLL_REGISTER_ERROR);
                cs = client_status_coll_get_status(&global_csc, pos);
                break;
            default:
                THROW(CLIENT_STATUS_COLL_GET_CS_ERROR);
        }
        
        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);
        if (txstate == TX_STATE_S0) {
            if (LIXA_RC_OK != (ret_cod = client_config(&global_ccc)))
                THROW(CLIENT_CONFIG_ERROR);
            if (LIXA_RC_OK != (ret_cod =
                               client_connect(&global_csc, &global_ccc)))
                THROW(CLIENT_CONNECT_ERROR);

            next_txstate = TX_STATE_S1;
            
            /* the real logic must be put here */
            if (LIXA_RC_OK != (ret_cod = lixa_xa_open(
                                   cs, &tmp_txrc, next_txstate)))
                THROW(LIXA_XA_OPEN_ERROR);
            
            /* set new state after RMs are open... */
            client_status_set_txstate(cs, next_txstate);
        } else { /* already opened, nothing to do */
            LIXA_TRACE(("lixa_tx_open: already opened (txstate = %d), "
                        "bypassing...\n", txstate));    
            THROW(ALREADY_OPENED);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_STATUS_COLL_GET_CS_ERROR:
            case CLIENT_STATUS_COLL_REGISTER_ERROR:
                break;
            case CLIENT_CONFIG_ERROR:
                break;
            case CLIENT_CONNECT_ERROR:
                break;
            case COLL_GET_CS_ERROR:
            case LIXA_XA_OPEN_ERROR:
                break;
            case ALREADY_OPENED:
                ret_cod = LIXA_RC_BYPASSED_OPERATION;
                *txrc = TX_OK;
                break;
            case NONE:
                *txrc = tmp_txrc;
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_open/TX_*=%d/excp=%d/"
                "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}



int lixa_tx_set_commit_return(int *txrc, COMMIT_RETURN when_return)
{
    enum Exception { COLL_GET_CS_ERROR
                     , PROTOCOL_ERROR
                     , INVALID_STATUS
                     , UNSUPPORTED_OPTION
                     , INVALID_OPTION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_TRACE(("lixa_tx_set_commit_return\n"));
    TRY {
        int txstate;
        client_status_t *cs;
        
        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succed -> protocol
                   error */
                THROW(PROTOCOL_ERROR);
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
                THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S2:
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default:
                THROW(INVALID_STATUS);
        }
                
        /* LIXA support only TX_COMMIT_COMPLETED; it does not support
         * TX_COMMIT_DECISION_LOGGED. This behavior does not violate standard
         * (see page 29 of "DTP: The TX (Transaction Demarcation) Specification
         */
        switch (when_return) {
            case TX_COMMIT_COMPLETED:
                /* this is the default and only supported characteristic */
                break;
            case TX_COMMIT_DECISION_LOGGED:
                THROW(UNSUPPORTED_OPTION);
                break;
            default:
                THROW(INVALID_OPTION);
                break;
        } /* switch (when_return) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_ERROR:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case UNSUPPORTED_OPTION:
                *txrc = TX_NOT_SUPPORTED;
                ret_cod = LIXA_RC_UNSUPPORTED_OPTION;
                break;
            case INVALID_OPTION:
                *txrc = TX_EINVAL;
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NONE:
                *txrc = TX_OK;
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_set_commit_return/TX_*=%d/excp=%d/"
                "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}



int lixa_tx_set_transaction_control(int *txrc,
                                    TRANSACTION_CONTROL control)
{
    enum Exception { COLL_GET_CS_ERROR
                     , PROTOCOL_ERROR
                     , INVALID_STATUS
                     , INTERNAL_ERROR1
                     , INTERNAL_ERROR2
                     , INVALID_OPTION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_TRACE(("lixa_tx_set_transaction_control\n"));
    TRY {
        int txstate, new_txstate;
        client_status_t *cs;
        
        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succed -> protocol
                   error */
                THROW(PROTOCOL_ERROR);
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
                THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S2:
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default:
                THROW(INVALID_STATUS);
        }
                
        new_txstate = txstate;
        switch (control) {
            case TX_UNCHAINED:
                switch (txstate) {
                    case TX_STATE_S1:
                        break;
                    case TX_STATE_S2:
                        new_txstate = TX_STATE_S1;
                        break;
                    case TX_STATE_S3:
                        break;
                    case TX_STATE_S4:
                        new_txstate = TX_STATE_S3;
                        break;
                    default:
                        THROW(INTERNAL_ERROR1);
                } /* switch (txstate) */
                break;
            case TX_CHAINED:
                switch (txstate) {
                    case TX_STATE_S1:
                        new_txstate = TX_STATE_S2;
                        break;
                    case TX_STATE_S2:
                        break;
                    case TX_STATE_S3:
                        new_txstate = TX_STATE_S4;
                        break;
                    case TX_STATE_S4:
                        break;
                    default:
                        THROW(INTERNAL_ERROR2);
                } /* switch (txstate) */
                break;
            default:
                THROW(INVALID_OPTION);
        } /* switch (when_return) */
        
        LIXA_TRACE(("lixa_tx_set_transaction_control: old status = S%d, "
                    "new status = S%d\n", txstate, new_txstate));
        /* set new state... */
        client_status_set_txstate(cs, new_txstate);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_ERROR:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case INTERNAL_ERROR1:
            case INTERNAL_ERROR2:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case INVALID_OPTION:
                *txrc = TX_EINVAL;
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NONE:
                *txrc = TX_OK;
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_set_transaction_control/TX_*=%d/excp=%d/"
                "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}



