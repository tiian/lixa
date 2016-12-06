/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
 */

#include <config.h>

#ifdef HAVE_ASSERT_H

# include <assert.h>

#endif
#ifdef HAVE_STDIO_H

# include <stdio.h>

#endif
#ifdef HAVE_STRING_H

# include <string.h>

#endif

#include <lixa_crash.h>
#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xid.h>
#include <lixa_tx.h>
#include <lixa_xa.h>
#include <lixa_xml_msg.h>
#include <client_conn.h>
#include <client_config.h>
#include <client_recovery.h>
#include <client_status.h>
#include <client_tpm.h>

/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX

int lixa_tx_begin(int *txrc, XID *xid)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        STATUS_NOT_FOUND,
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR1,
        INVALID_STATUS,
        OUTSIDE_ERROR,
        PROTOCOL_ERROR2,
        LIXA_XA_START_ERROR1,
        LIXA_XA_START_ERROR2,
        GETTIMEOFDAY_ERROR,
        XID_SERIALIZE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_begin\n"));
    TRY {
        int txstate, next_txstate, dupid_or_proto = FALSE;
        TRANSACTION_TIMEOUT timeout;
        guint i;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND: THROW(STATUS_NOT_FOUND);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

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
            case TX_STATE_S4: THROW(PROTOCOL_ERROR1);
            default: THROW(INVALID_STATUS);
        }
        LIXA_TRACE(("lixa_tx_begin: txstate = S%d, "
            "next_txstate = S%d\n", txstate, next_txstate));

        /* check there are no local transactions started from any resource
           manager */
        for (i = 0; i < global_ccc.actconf.rsrmgrs->len; ++i) {
            struct client_status_rsrmgr_s *csr = &g_array_index(
                cs->rmstatus,
                struct client_status_rsrmgr_s, i);
            if (csr->common.dynamic &&
                XA_STATE_D3 == csr->common.xa_td_state) {
                LIXA_TRACE(("lixa_tx_begin: resource manager # %u uses "
                    "dynamic registration and is in state %d "
                    "(Registered with NULLXID)\n",
                    i, csr->common.xa_td_state));
                THROW(OUTSIDE_ERROR);
            } else if (csr->common.dynamic &&
                       XA_STATE_D0 != csr->common.xa_td_state) {
                LIXA_TRACE(("lixa_tx_begin: resource manager # %u uses "
                    "dynamic registration and is in state %d\n",
                    i, csr->common.xa_td_state));
                THROW(PROTOCOL_ERROR2);
            }
        }

        int xa_start_flags = TMNOFLAGS;
        if (xid->formatID == NULLXID) {
            /* generate the transaction id */
            LIXA_TRACE(("lixa_tx_begin: generating new xid for transaction\n"));
            lixa_xid_create_new(xid);
        } else {
            LIXA_TRACE(
                ("lixa_tx_begin: xid specified, joining transaction with new branch\n"));
            lixa_xid_create_new_bqual(xid);
        }
        client_status_set_xid(cs, xid);

        /* the real logic must be put here */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
            cs, txrc, xid, txstate, next_txstate,
            &dupid_or_proto, xa_start_flags))) {
            int retry_ok = FALSE;
            if (dupid_or_proto) {
                int txrc2 = TX_FAIL;
                LIXA_TRACE(("lixa_tx_begin: XAER_DUPID or XAER_PROTO "
                    "returned from any resource manager after "
                    "xa_start call; trying a new synchronization with "
                    "tx_rollback...\n"));
                if (LIXA_RC_OK != (ret_cod = lixa_xa_end(
                    cs, &txrc2, FALSE, TMNOFLAGS))) {
                    LIXA_TRACE(("lixa_tx_begin: error while calling "
                        "lixa_xa_end (ret_cod=%d, txrc2=%d), going "
                        "on...\n", ret_cod, txrc2));
                }
                if (LIXA_RC_OK !=
                    (ret_cod = lixa_xa_rollback(cs, &txrc2, FALSE))) {
                    LIXA_TRACE(("lixa_tx_begin: error while calling "
                        "lixa_xa_rollback (ret_cod=%d, txrc2=%d), "
                        "going on...\n", ret_cod, txrc2));
                }
                if (LIXA_RC_OK != (ret_cod = lixa_xa_start(
                    cs, txrc, xid, txstate, next_txstate,
                    &dupid_or_proto, TMNOFLAGS))) {
                    LIXA_TRACE(("lixa_tx_begin: lixa_xa_start failed again, "
                        "returning TX_FAIL\n"));
                    *txrc = TX_FAIL;
                    THROW(LIXA_XA_START_ERROR1);
                }
                retry_ok = TRUE;
            }
            if (!retry_ok) THROW(LIXA_XA_START_ERROR2);
        }

        /* is there a timeout set? */
        if (0 < (timeout = client_status_get_tx_timeout(cs))) {
            struct timeval tv;
            LIXA_TRACE(("lixa_tx_begin: this transaction has a timeout "
                "of %ld seconds\n", timeout));
            if (0 != gettimeofday(&tv, NULL)) THROW(GETTIMEOFDAY_ERROR);
            /* set tx_timeout_time */
            client_status_set_tx_timeout_time(cs, tv.tv_sec + (time_t) timeout);
        }
        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(cs, next_txstate);
        /* update tx_state */
        client_status_set_tx_state(cs, TX_ACTIVE);

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case STATUS_NOT_FOUND:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
                break;
            case OUTSIDE_ERROR:
                *txrc = TX_OUTSIDE;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case PROTOCOL_ERROR1:
            case PROTOCOL_ERROR2:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case LIXA_XA_START_ERROR1:
            case LIXA_XA_START_ERROR2:
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case XID_SERIALIZE_ERROR:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
        } /* switch (excp) */
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
        if (TX_FAIL == *txrc)
            lixa_tx_cleanup();
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_begin/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_close(int *txrc)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        LIXA_XA_CLOSE_ERROR,
        CLIENT_DISCONNECT_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int tmp_txrc = TX_OK;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_close\n"));
    TRY {
        int txstate;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                *txrc = TX_OK;
                /* break intentionally missed */
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
            case TX_STATE_S2:
                break;
            case TX_STATE_S3:
            case TX_STATE_S4: THROW(PROTOCOL_ERROR);
            default: THROW(INVALID_STATUS);
        }

        /* update the TX state, now TX_STATE_S0; the result of XA calls
           must not be waited; see bug 3006369 */
        client_status_set_txstate(cs, TX_STATE_S0);

        if (LIXA_RC_OK != (ret_cod = lixa_xa_close(cs, &tmp_txrc))) THROW(
            LIXA_XA_CLOSE_ERROR);

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
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
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
        if (TX_PROTOCOL_ERROR != *txrc)
            lixa_tx_cleanup();
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_close/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

void lixa_tx_close_cleanup(void)
{
    client_status_t *cs;

    LIXA_TRACE(("lixa_tx_close_cleanup\n"));
    if (LIXA_RC_OK == client_status_coll_get_cs(&global_csc, &cs)) {
        client_status_coll_del(&global_csc);
    }
}

int lixa_tx_end(int *txrc, int flags)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        STATUS_NOT_FOUND,
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        XA_END_ERROR,
        XA_PREPARE_ERROR,
        XA_COMMIT_ERROR,
        INVALID_STATE1,
        INVALID_STATE2,
        INVALID_TXRC1,
        XA_ROLLBACK_ERROR,
        INVALID_STATE3,
        INVALID_STATE4,
        INVALID_TXRC2,
        XA_FORGET_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_end\n"));
    TRY {
        int txstate, next_txstate, commit = TRUE;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND: THROW(STATUS_NOT_FOUND);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
            case TX_STATE_S2: THROW(PROTOCOL_ERROR);
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
        }
        LIXA_TRACE(("lixa_tx_end: txstate = S%d\n", txstate));

        /* check if the there is a timeout */
        if (client_status_is_tx_timeout_time(cs)) {
            LIXA_TRACE(("lixa_tx_commit: this transaction is in state "
                "TX_TIMEOUT_ROLLBACK_ONLY and cannot be committed, "
                "rolling back...\n", txstate));
            commit = FALSE;
        }

        /* detach the transaction */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_end(cs, txrc, commit, flags))) {
            if (TX_ROLLBACK == *txrc)
                commit = FALSE;
            else THROW(XA_END_ERROR);
        }
        /* commit/rollback */
        if (commit) {
            LIXA_TRACE(("lixa_tx_end: go on with end...\n"));
            switch (*txrc) {
                case TX_OK:
                case TX_ROLLBACK:
                case TX_MIXED:
                case TX_HAZARD:
                    if (TX_STATE_S3 == txstate)
                        next_txstate = TX_STATE_S1;
                    else if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE3);
                    break;
                case TX_NO_BEGIN:
                case TX_ROLLBACK_NO_BEGIN:
                case TX_MIXED_NO_BEGIN:
                case TX_HAZARD_NO_BEGIN:
                case TX_COMMITTED_NO_BEGIN:
                    if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE4);
                    break;
                case TX_FAIL:
                    next_txstate = txstate;
                    break;
                default: THROW(INVALID_TXRC2);
            } /* switch */
        } else {
            switch (*txrc) {
                case TX_OK:
                case TX_ROLLBACK:
                case TX_MIXED:
                case TX_HAZARD:
                    if (TX_STATE_S3 == txstate)
                        next_txstate = TX_STATE_S1;
                    else if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE3);
                    break;
                case TX_NO_BEGIN:
                case TX_ROLLBACK_NO_BEGIN:
                case TX_MIXED_NO_BEGIN:
                case TX_HAZARD_NO_BEGIN:
                case TX_COMMITTED_NO_BEGIN:
                    if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE4);
                    break;
                case TX_FAIL:
                    next_txstate = txstate;
                    break;
                default: THROW(INVALID_TXRC2);
            } /* switch */
        } /* else */

        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(cs, next_txstate);
        /* reset the transaction id */
        lixa_xid_reset(client_status_get_xid(cs));

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case STATUS_NOT_FOUND:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XA_END_ERROR:
            case XA_PREPARE_ERROR:
            case XA_COMMIT_ERROR:
                *txrc = TX_FAIL;
                break;
            case INVALID_STATE1:
            case INVALID_STATE2:
            case INVALID_TXRC1:
            case INVALID_STATE3:
            case INVALID_STATE4:
            case INVALID_TXRC2:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XA_FORGET_ERROR:
            case XA_ROLLBACK_ERROR:
                *txrc = TX_FAIL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
        if (TX_FAIL == *txrc)
            lixa_tx_cleanup();
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_end/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_commit(int *txrc, int *begin_new)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        STATUS_NOT_FOUND,
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        XA_END_ERROR,
        XA_PREPARE_ERROR,
        XA_COMMIT_ERROR,
        INVALID_STATE1,
        INVALID_STATE2,
        INVALID_TXRC1,
        XA_ROLLBACK_ERROR,
        INVALID_STATE3,
        INVALID_STATE4,
        INVALID_TXRC2,
        XA_FORGET_ERROR,
        XA_TPM_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;
    GArray *xida = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_commit\n"));
    TRY {
        int txstate, next_txstate, commit = TRUE, finished = TRUE;
        int one_phase_commit = FALSE, prepare_txrc;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND: THROW(STATUS_NOT_FOUND);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
            case TX_STATE_S2: THROW(PROTOCOL_ERROR);
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
        }
        LIXA_TRACE(("lixa_tx_commit: txstate = S%d\n", txstate));

        /* check if the there is a timeout */
        if (client_status_is_tx_timeout_time(cs)) {
            LIXA_TRACE(("lixa_tx_commit: this transaction is in state "
                "TX_TIMEOUT_ROLLBACK_ONLY and cannot be committed, "
                "rolling back...\n", txstate));
            commit = FALSE;
        } else
            one_phase_commit = client_status_could_one_phase(cs);

        /* detach the transaction */
        if (LIXA_RC_OK !=
            (ret_cod = lixa_xa_end(cs, txrc, commit, TMSUCCESS))) {
            if (TX_ROLLBACK == *txrc)
                commit = FALSE;
            else THROW(XA_END_ERROR);
        }
        /* check if there are more transactions that forms part of this gtrid */
        xida = g_array_new(FALSE, FALSE, sizeof(char *));
        if (LIXA_RC_OBJ_NOT_FOUND ==
            (ret_cod = lixa_tx_tpm(xida, FALSE, FALSE))) THROW(
            XA_TPM_ERROR);
        if (xida->len > 1) {
            one_phase_commit = FALSE;
        }

        /* prepare (skip if we are rolling back) */
        XID xid;
        if (commit) {
            /* bypass xa_prepare if one_phase_commit is TRUE */
            if (!one_phase_commit &&
                LIXA_RC_OK !=
                (ret_cod = lixa_xa_prepare(cs, xida, txrc, &commit,
                                           &xid))) THROW(
                XA_PREPARE_ERROR);
            if (one_phase_commit)
                xid = *client_status_get_xid(cs);
        }
        prepare_txrc = *txrc;
        /* commit/rollback */
        if (commit) {
            LIXA_TRACE(("lixa_tx_commit: go on with commit...\n"));
            if (LIXA_RC_OK != (ret_cod = lixa_xa_commit(
                cs, &xid, txrc, one_phase_commit))) THROW(XA_COMMIT_ERROR);
            switch (*txrc) {
                case TX_OK:
                case TX_ROLLBACK:
                case TX_ERROR:
                case TX_MIXED:
                case TX_HAZARD:
                    if (TX_STATE_S3 == txstate)
                        next_txstate = TX_STATE_S1;
                    else if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE1);
                    break;
                case TX_NO_BEGIN:
                case TX_ROLLBACK_NO_BEGIN:
                case TX_MIXED_NO_BEGIN:
                case TX_HAZARD_NO_BEGIN:
                    if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE2);
                    break;
                case TX_FAIL:
                    next_txstate = txstate;
                    finished = FALSE;
                    break;
                default: THROW(INVALID_TXRC1);
            } /* switch */
        } else {
            LIXA_TRACE(("lixa_tx_commit: go on with rollback...\n"));
            if (LIXA_RC_OK !=
                (ret_cod = lixa_xa_rollback(cs, txrc, TRUE))) THROW(
                XA_ROLLBACK_ERROR);
            if (TX_FAIL == prepare_txrc) {
                LIXA_TRACE(("lixa_tx_commit: txrc=%d, prepare_txrc=%d, "
                    "returning TX_FAIL to Application Program\n",
                    *txrc, prepare_txrc));
                *txrc = TX_FAIL;
            }
            switch (*txrc) {
                case TX_OK:
                case TX_ROLLBACK:
                case TX_MIXED:
                case TX_HAZARD:
                    if (TX_STATE_S3 == txstate)
                        next_txstate = TX_STATE_S1;
                    else if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE3);
                    break;
                case TX_NO_BEGIN:
                case TX_ROLLBACK_NO_BEGIN:
                case TX_MIXED_NO_BEGIN:
                case TX_HAZARD_NO_BEGIN:
                case TX_COMMITTED_NO_BEGIN:
                    if (TX_STATE_S4 == txstate)
                        next_txstate = TX_STATE_S2;
                    else THROW(INVALID_STATE4);
                    break;
                case TX_FAIL:
                    next_txstate = txstate;
                    finished = FALSE;
                    break;
                default: THROW(INVALID_TXRC2);
            } /* switch */
        } /* else */

        /* clean Heurstically Completed states... */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_forget(cs, xida, finished))) THROW(
            XA_FORGET_ERROR);

        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(cs, next_txstate);
        /* reset the transaction id */
        lixa_xid_reset(client_status_get_xid(cs));

        if (TX_STATE_S2 == next_txstate) /* start a new transaction */
            *begin_new = TRUE;

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case STATUS_NOT_FOUND:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XA_END_ERROR:
            case XA_PREPARE_ERROR:
            case XA_COMMIT_ERROR:
            case XA_TPM_ERROR:
                *txrc = TX_FAIL;
                break;
            case INVALID_STATE1:
            case INVALID_STATE2:
            case INVALID_TXRC1:
            case INVALID_STATE3:
            case INVALID_STATE4:
            case INVALID_TXRC2:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XA_FORGET_ERROR:
            case XA_ROLLBACK_ERROR:
                *txrc = TX_FAIL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
        if (TX_FAIL == *txrc)
            lixa_tx_cleanup();
        if (NULL != xida)
            g_array_free(xida, TRUE);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_commit/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_info(int *txrc, TXINFO *info)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        COLL_GET_CS_ERROR,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_info\n"));
    TRY {
        int txstate;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succeed -> protocol error */
            THROW(PROTOCOL_ERROR);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);
        switch (txstate) {
            case TX_STATE_S0: THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S2:
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
        }

        if (NULL != info) {
#ifdef _TRACE
            lixa_ser_xid_t xid_str = "";
#endif
            memcpy(&info->xid, client_status_get_xid(cs), sizeof(XID));
            /* Lixa supports only this option; it's constant */
            info->when_return = TX_COMMIT_COMPLETED;
            if (TX_STATE_S1 == txstate || TX_STATE_S3 == txstate)
                info->transaction_control = TX_UNCHAINED;
            else
                info->transaction_control = TX_CHAINED;
            /* update tx_timeout and tx_state */
            client_status_is_tx_timeout_time(cs);
            info->transaction_timeout = client_status_get_tx_timeout(cs);
            info->transaction_state = client_status_get_tx_state(cs);
#ifdef _TRACE
            lixa_xid_serialize(&info->xid, xid_str);
            LIXA_TRACE(("lixa_tx_info: xid='%s', when_return=%ld, "
                        "transaction_control=%ld, transaction_timeout=%ld, "
                        "transaction_state=%ld\n", xid_str, info->when_return,
                        info->transaction_control, info->transaction_timeout,
                        info->transaction_state));
#endif
        }

        if (TX_STATE_S3 == txstate || TX_STATE_S4 == txstate)
            *txrc = 1;
        else
            *txrc = 0;

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case COLL_GET_CS_ERROR:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_info/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_open(int *txrc, int mmode)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        CLIENT_STATUS_COLL_REGISTER_ERROR,
        ADDED_AND_NOT_FOUND,
        CLIENT_STATUS_COLL_GET_CS_ERROR,
        CLIENT_CONFIG_ERROR,
        CLIENT_CONNECT_ERROR,
        CLIENT_CONFIG_JOB_ERROR,
        COLL_GET_CS_ERROR,
        SHUTDOWN_ERROR,
        LIXA_XA_OPEN_ERROR,
        ALREADY_OPENED,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int tmp_txrc = TX_FAIL;

    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_open\n"));
    TRY {
        int txstate, next_txstate;

        /* check if the thread is already registered and
         * retrieve a reference to the status of the current thread */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* already registered, nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND: /* first time, it must be registered */
                /* register this thread in library status */
                if (LIXA_RC_OK != (ret_cod = client_status_coll_add(
                    &global_csc))) THROW(CLIENT_STATUS_COLL_REGISTER_ERROR);
                if (LIXA_RC_OK != (ret_cod = client_status_coll_get_cs(
                    &global_csc, &cs))) {
                    LIXA_TRACE(("lixa_tx_open: the status was added but "
                        "it can not be retrieved; this is a severe "
                        "internal error!\n"));
                    THROW(ADDED_AND_NOT_FOUND);
                }
                break;
            default: THROW(CLIENT_STATUS_COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);
        if (txstate == TX_STATE_S0) {
            if (LIXA_RC_OK != (ret_cod = client_config(&global_ccc))) THROW(
                CLIENT_CONFIG_ERROR);
            if (LIXA_RC_OK != (ret_cod =
                                   client_connect(&global_csc,
                                                  &global_ccc))) THROW(
                CLIENT_CONNECT_ERROR);
            if (LIXA_RC_OK != (ret_cod = client_config_job(
                &global_ccc, client_status_get_sockfd(cs)))) THROW(
                CLIENT_CONFIG_JOB_ERROR);

            next_txstate = TX_STATE_S1;

            /* the real logic is inside this function */
            if (LIXA_RC_OK != (ret_cod = lixa_xa_open(
                cs, &tmp_txrc, next_txstate, mmode))) THROW(LIXA_XA_OPEN_ERROR);

            /* set new state after RMs are open... */
            client_status_set_txstate(cs, next_txstate);
        } else { /* already opened, nothing to do */
            LIXA_TRACE(("lixa_tx_open: already opened (txstate = %d), "
                "bypassing...\n", txstate));
            THROW(ALREADY_OPENED);
        }

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case CLIENT_STATUS_COLL_REGISTER_ERROR:
                break;
            case ADDED_AND_NOT_FOUND:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case CLIENT_STATUS_COLL_GET_CS_ERROR:
            case CLIENT_CONFIG_ERROR:
            case CLIENT_CONNECT_ERROR:
            case CLIENT_CONFIG_JOB_ERROR:
                break;
            case SHUTDOWN_ERROR:
                ret_cod = LIXA_RC_SHUTDOWN_ERROR;
                break;
            case LIXA_XA_OPEN_ERROR:
                /* clean-up socket and free thread status */
                client_disconnect(&global_csc);
                lixa_tx_cleanup();
                *txrc = tmp_txrc;
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

int lixa_tx_rollback(int *txrc, int *begin_new)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        STATUS_NOT_FOUND,
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        XA_END_ERROR,
        XA_ROLLBACK_ERROR,
        INVALID_STATE1,
        INVALID_STATE2,
        INVALID_TXRC,
        XA_FORGET_ERROR,
        XA_TPM_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;
    GArray *xida = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_rollback\n"));
    TRY {
        int txstate, next_txstate, finished = TRUE;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND: THROW(STATUS_NOT_FOUND);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0:
            case TX_STATE_S1:
            case TX_STATE_S2: THROW(PROTOCOL_ERROR);
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
        }
        LIXA_TRACE(("lixa_tx_rollback: txstate = S%d\n", txstate));

        if (LIXA_RC_OK !=
            (ret_cod = lixa_xa_end(cs, txrc, FALSE, TMFAIL))) THROW(
            XA_END_ERROR);

        if (LIXA_RC_OK !=
            (ret_cod = lixa_xa_rollback(cs, txrc, FALSE))) THROW(
            XA_ROLLBACK_ERROR);
        switch (*txrc) {
            case TX_OK:
            case TX_MIXED:
            case TX_HAZARD:
            case TX_COMMITTED:
            case TX_ERROR:
                if (TX_STATE_S3 == txstate)
                    next_txstate = TX_STATE_S1;
                else if (TX_STATE_S4 == txstate)
                    next_txstate = TX_STATE_S2;
                else THROW(INVALID_STATE1);
                break;
            case TX_NO_BEGIN:
            case TX_MIXED_NO_BEGIN:
            case TX_HAZARD_NO_BEGIN:
            case TX_COMMITTED_NO_BEGIN:
                if (TX_STATE_S4 == txstate)
                    next_txstate = TX_STATE_S2;
                else THROW(INVALID_STATE2);
                break;
            case TX_FAIL:
                next_txstate = txstate;
                finished = FALSE;
                break;
            default: THROW(INVALID_TXRC);
        } /* switch */

        /* clean Heuristically Completed states... */
        if (LIXA_RC_OK != (ret_cod = lixa_xa_forget(cs, xida, finished))) THROW(
            XA_FORGET_ERROR);

        /* update the TX state, now TX_STATE_S0 */
        client_status_set_txstate(cs, next_txstate);
        /* reset the transaction id */
        lixa_xid_reset(client_status_get_xid(cs));

        if (TX_STATE_S2 == next_txstate) /* start a new transaction */
            *begin_new = TRUE;

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case STATUS_NOT_FOUND:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XA_END_ERROR:
                *txrc = TX_FAIL;
                break;
            case INVALID_STATE1:
            case INVALID_STATE2:
            case INVALID_TXRC:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case XA_ROLLBACK_ERROR:
            case XA_FORGET_ERROR:
            case XA_TPM_ERROR:
                *txrc = TX_FAIL;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
        if (TX_FAIL == *txrc)
            lixa_tx_cleanup();
        if (NULL != xida)
            g_array_free(xida, TRUE);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_rollback/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_set_commit_return(int *txrc, COMMIT_RETURN when_return)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        COLL_GET_CS_ERROR,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        UNSUPPORTED_OPTION,
        INVALID_OPTION,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_set_commit_return\n"));
    TRY {
        int txstate;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succed -> protocol
                   error */
            THROW(PROTOCOL_ERROR);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0: THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S2:
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
        }

        /* LIXA support only TX_COMMIT_COMPLETED; it does not support
         * TX_COMMIT_DECISION_LOGGED. This behavior does not violate standard
         * (see page 29 of "DTP: The TX (Transaction Demarcation) Specification
         */
        switch (when_return) {
            case TX_COMMIT_COMPLETED:
                /* this is the default and only supported characteristic */
                break;
            case TX_COMMIT_DECISION_LOGGED: THROW(UNSUPPORTED_OPTION);
                break;
            default: THROW(INVALID_OPTION);
                break;
        } /* switch (when_return) */

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
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
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_set_commit_return/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_set_transaction_control(int *txrc,
                                    TRANSACTION_CONTROL control)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        COLL_GET_CS_ERROR,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        INTERNAL_ERROR1,
        INTERNAL_ERROR2,
        INVALID_OPTION,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_set_transaction_control\n"));
    TRY {
        int txstate, new_txstate;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succed -> protocol
                   error */
            THROW(PROTOCOL_ERROR);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);

        switch (txstate) {
            case TX_STATE_S0: THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S2:
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
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
                    default: THROW(INTERNAL_ERROR1);
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
                    default: THROW(INTERNAL_ERROR2);
                } /* switch (txstate) */
                break;
            default: THROW(INVALID_OPTION);
        } /* switch (when_return) */

        LIXA_TRACE(("lixa_tx_set_transaction_control: old status = S%d, "
            "new status = S%d\n", txstate, new_txstate));
        /* set new state... */
        client_status_set_txstate(cs, new_txstate);

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
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
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_set_transaction_control/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_set_transaction_timeout(int *txrc,
                                    TRANSACTION_TIMEOUT timeout)
{
    enum Exception
    {
        CLIENT_STATUS_FAILED,
        COLL_GET_CS_ERROR,
        PROTOCOL_ERROR,
        INVALID_STATUS,
        INVALID_OPTION,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    client_status_t *cs = NULL;

    *txrc = TX_FAIL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_set_transaction_timeout\n"));
    TRY {
        int txstate;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                if (client_status_is_failed(cs)) THROW(CLIENT_STATUS_FAILED);
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succed -> protocol
                   error */
            THROW(PROTOCOL_ERROR);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check TX state (see Table 7-1) */
        txstate = client_status_get_txstate(cs);
        switch (txstate) {
            case TX_STATE_S0: THROW(PROTOCOL_ERROR);
            case TX_STATE_S1:
            case TX_STATE_S2:
            case TX_STATE_S3:
            case TX_STATE_S4:
                break;
            default: THROW(INVALID_STATUS);
        }

        /* see bug # 3008437 */
        if (timeout < 0) {
            LIXA_TRACE(("lixa_tx_set_transaction_timeout: %ld is not "
                "considered a valid value for timeout\n", timeout));
            THROW(INVALID_OPTION);
        }

        /* set the new value for transaction timeout */
        client_status_set_tx_timeout(cs, timeout);

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case CLIENT_STATUS_FAILED:
                *txrc = TX_FAIL;
                ret_cod = LIXA_RC_TX_FAIL;
                break;
            case COLL_GET_CS_ERROR:
                break;
            case PROTOCOL_ERROR:
                *txrc = TX_PROTOCOL_ERROR;
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
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
        if (TX_FAIL == *txrc && NULL != cs)
            client_status_failed(cs);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_set_transaction_timeout/TX_*=%d/excp=%d/"
        "ret_cod=%d/errno=%d\n", *txrc, excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_recover(int report, int commit, int rollback, int bbqc, int bfic,
                    int utf, const char *xid, const char *xid_file)
{
    enum Exception
    {
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR,
        G_TREE_NEW,
        RECOVERY_SCAN_ERROR,
        RECOVERY_REPORT_ERROR,
        FOPEN_ERROR,
        FGETS_ERROR,
        XID_DESERIALIZE_ERROR,
        COLD_COMMIT_ERROR,
        COLD_ROLLBACK_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    GTree *crt = NULL; /* cold recovery table */
    FILE *xid_stream = NULL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_recover\n"));
    TRY {
        int i;
        client_status_t *cs = NULL;

        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succeed -> protocol
                   error */
            THROW(PROTOCOL_ERROR);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

        /* create a new tree; node key is a dynamically allocated XID;
           node data is a dynamic array */
        if (NULL == (crt = g_tree_new_full(
            clnt_rcvr_xid_compare, NULL, free,
            clnt_rcvr_array_free))) THROW(G_TREE_NEW);

        if (LIXA_RC_OK != (ret_cod = client_recovery_scan(
            cs, crt, bbqc, bfic, utf))) THROW(RECOVERY_SCAN_ERROR);

        if (report && LIXA_RC_OK != (
            ret_cod = client_recovery_report(cs, crt))) THROW(
            RECOVERY_REPORT_ERROR);

        /* open file if necessary */
        if (NULL != xid_file &&
            NULL == (xid_stream = fopen(xid_file, "r"))) THROW(FOPEN_ERROR);

        i = 0;
        while (xid || xid_file) {
            char buffer[2 * LIXA_XID_SERIALIZE_LENGTH];
            XID tmp_xid;
            gpointer tree_record;

            i++;
            if (xid && i > 1) break; /* leave after first cycle */
            if (xid) {
                strncpy(buffer, xid, sizeof(buffer) - 1);
                buffer[sizeof(buffer) - 1] = '\0';
            } else {
                char *p;
                if (NULL == fgets(buffer, sizeof(buffer), xid_stream)) {
                    if (feof(xid_stream))
                        break;
                    else THROW(FGETS_ERROR);
                }
                /* remove trailing \n */
                if (NULL != (p = strchr(buffer, '\n')))
                    *p = '\0';
            }
            if (strlen(buffer) > LIXA_XID_SERIALIZE_LENGTH - 1) {
                LIXA_TRACE(("lixa_tx_recover: record # %d is of wrong "
                    "size, discarded ('%s')\n", i, buffer));
                continue;
            }
            printf("Analizing transaction '%s':\n", buffer);
            if (!lixa_xid_deserialize(&tmp_xid, buffer)) THROW(
                XID_DESERIALIZE_ERROR);

            /* look for xid */
            if (NULL == (tree_record = g_tree_lookup(crt, &tmp_xid))) {
                printf("this transaction was not found, skipping...\n");
                continue;
            } else {
                if (commit) {
                    if (LIXA_RC_OK != (
                        ret_cod = client_recovery_cold_commit(
                            cs, &tmp_xid, tree_record))) THROW(
                        COLD_COMMIT_ERROR);
                } else if (rollback) {
                    if (LIXA_RC_OK != (
                        ret_cod = client_recovery_cold_rollback(
                            cs, &tmp_xid, tree_record))) THROW(
                        COLD_ROLLBACK_ERROR);
                } else
                    printf("this transaction exists and could be "
                               "committed/rolled back.\n");
            }
        }

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
                break;
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case G_TREE_NEW:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case RECOVERY_SCAN_ERROR:
            case RECOVERY_REPORT_ERROR:
                break;
            case FOPEN_ERROR:
                ret_cod = LIXA_RC_FOPEN_ERROR;
                break;
            case FGETS_ERROR:
                ret_cod = LIXA_RC_FGETS_ERROR;
                break;
            case XID_DESERIALIZE_ERROR:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case COLD_COMMIT_ERROR:
            case COLD_ROLLBACK_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (NULL != crt) g_tree_destroy(crt);
        if (NULL != xid_stream) fclose(xid_stream);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_recover/excp=%d/"
        "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

int lixa_tx_tpm(GArray *xida, int maint, int report)
{
    enum Exception
    {
        COLL_GET_CS_ERROR,
        CONNECTION_CLOSED,
        PROTOCOL_ERROR,
        G_TREE_NEW,
        TPM_NO_TRANSACTIONS,
        TPM_TRAN_ERROR,
        TPM_REPORT_ERROR,
        XID_DESERIALIZE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    GTree *xidt = NULL;

    LIXA_TRACE_INIT;
    LIXA_CRASH_INIT;
    LIXA_TRACE(("lixa_tx_tpm\n"));
    TRY {
        /* retrieve a reference to the thread status */
        client_status_t *cs = NULL;
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                /* status not found -> tx_open did not succeed -> protocol error */
            THROW(PROTOCOL_ERROR);
            default: THROW(COLL_GET_CS_ERROR);
        }

        /* check the connection to the server is still alive */
        if (!client_status_is_connected(cs)) THROW(CONNECTION_CLOSED);

        /* create a new tree; node key is a dynamically allocated XID; node data is a dynamic array */
        if (NULL == (xidt = g_tree_new_full(tpm_gtrid_compare, NULL, free,
                                            tpm_gtrid_value_destroy))) THROW(
            G_TREE_NEW);

        if (LIXA_RC_OK != (ret_cod = client_tpm_trans(cs, xidt, maint))) {
            if (LIXA_RC_OBJ_NOT_FOUND == ret_cod) {
                if (report)
                    printf("\nThere are no transactions.\n");

                THROW(TPM_NO_TRANSACTIONS);
            } else {
                THROW(TPM_TRAN_ERROR);
            }
        }

        if (g_tree_nnodes(xidt)) {
            g_tree_foreach(xidt, client_tpm_value_foreach, xida);
        }

        LIXA_TRACE(("lixa_tx_tpm: xidt_count=%d\n", xida->len));

        if (report &&
            LIXA_RC_OK != (ret_cod = client_tpm_report(cs, xidt))) THROW(
            TPM_REPORT_ERROR);

        THROW(NONE);
    }
    CATCH
    {
        switch (excp) {
            case COLL_GET_CS_ERROR:
            case CONNECTION_CLOSED:
                break;
            case PROTOCOL_ERROR:
                ret_cod = LIXA_RC_PROTOCOL_ERROR;
                break;
            case G_TREE_NEW:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case TPM_TRAN_ERROR:
            case TPM_NO_TRANSACTIONS:
                break;
            case XID_DESERIALIZE_ERROR:
                ret_cod = LIXA_RC_MALFORMED_XID;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (xidt) g_tree_destroy(xidt);
    } /* TRY-CATCH */
    LIXA_TRACE(
        ("lixa_tx_tpm/excp=%d/ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

void lixa_tx_cleanup(void)
{
    int ret_cod;

    LIXA_TRACE(("lixa_tx_cleanup: emergency socket & memory clean-up\n"));
    ret_cod = client_disconnect(&global_csc);
    LIXA_TRACE(("lixa_tx_cleanup/client_disconnect/ret_cod=%d\n", ret_cod));
    ret_cod = client_unconfig(&global_ccc);
    LIXA_TRACE(("lixa_tx_cleanup/client_unconfig/ret_cod=%d\n", ret_cod));
}
