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
#include <config.h>

#include <tx.h>
#include <lixa_tx.h>
#include <lixa_xid.h>

/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX

int tx_begin(void) {
    int txrc = TX_FAIL;
    XID xid;
    lixa_xid_reset(&xid);
    lixa_tx_begin(&txrc, &xid, TMNOFLAGS);
    return txrc;
}


int tx_resume(XID *xid) {
    int txrc = TX_FAIL;
    lixa_tx_begin(&txrc, xid, TMRESUME);
    return txrc;
}


int tx_join(XID *xid) {
    int txrc = TX_FAIL;
    lixa_tx_begin(&txrc, xid, TMJOIN);
    return txrc;
}


int tx_end(int flags) {
    int txrc = TX_FAIL;
    lixa_tx_end(&txrc, (flags & TMSUSPEND) ? flags : (flags | TMJOIN));
    return txrc;
}


int tx_close(void) {
    int txrc = TX_FAIL;
    lixa_tx_close(&txrc);
    return txrc;
}


int tx_commit(void) {
    int txrc1 = TX_FAIL;
    int txrc2 = TX_FAIL;
    int begin_new = FALSE;
    lixa_tx_commit(&txrc1, &begin_new);
    if (begin_new) {
        XID xid;
        lixa_xid_reset(&xid);
        lixa_tx_begin(&txrc2, &xid, TMNOFLAGS);
        if (TX_OK != txrc2) {
            switch (txrc1) {
                case TX_OK:
                    txrc2 = TX_NO_BEGIN;
                    break;
                case TX_ROLLBACK:
                    txrc2 = TX_ROLLBACK_NO_BEGIN;
                    break;
                case TX_MIXED:
                    txrc2 = TX_MIXED_NO_BEGIN;
                    break;
                case TX_HAZARD:
                    txrc2 = TX_HAZARD_NO_BEGIN;
                    break;
                default:
                    txrc2 = TX_FAIL;
            }
        } else
            txrc2 = txrc1;
    } else
        txrc2 = txrc1;
    return txrc2;
}


int tx_info(TXINFO *info) {
    int txrc = TX_FAIL;
    lixa_tx_info(&txrc, info);
    return txrc;
}


int tx_open(void) {
    int txrc = TX_FAIL;
    lixa_tx_open(&txrc, FALSE);
    return txrc;
}


int tx_rollback(void) {
    int txrc1 = TX_FAIL;
    int txrc2 = TX_FAIL;
    int begin_new = FALSE;
    lixa_tx_rollback(&txrc1, &begin_new);
    if (begin_new) {
        XID xid;
        xid.formatID = NULLXID;
        lixa_tx_begin(&txrc2, &xid, TMNOFLAGS);
        if (TX_OK != txrc2) {
            switch (txrc1) {
                case TX_OK:
                    txrc2 = TX_NO_BEGIN;
                    break;
                case TX_MIXED:
                    txrc2 = TX_MIXED_NO_BEGIN;
                    break;
                case TX_HAZARD:
                    txrc2 = TX_HAZARD_NO_BEGIN;
                    break;
                case TX_COMMITTED:
                    txrc2 = TX_COMMITTED_NO_BEGIN;
                    break;
                default:
                    txrc2 = TX_FAIL;
            }
        } else
            txrc2 = txrc1;
    } else
        txrc2 = txrc1;
    return txrc2;
}


int tx_set_commit_return(COMMIT_RETURN when_return) {
    int txrc = TX_FAIL;
    lixa_tx_set_commit_return(&txrc, when_return);
    return txrc;
}


int tx_set_transaction_control(TRANSACTION_CONTROL control) {
    int txrc = TX_FAIL;
    lixa_tx_set_transaction_control(&txrc, control);
    return txrc;
}


int tx_set_transaction_timeout(TRANSACTION_TIMEOUT timeout) {
    int txrc = TX_FAIL;
    lixa_tx_set_transaction_timeout(&txrc, timeout);
    return txrc;
}


int tx_xid_serialize(TXINFO info, char **sxid) {
    int txrc = TX_FAIL;

    lixa_ser_xid_t xid_str = "";
    if (!lixa_xid_serialize(&info.xid, xid_str)) {
        return txrc;
    }

    if (NULL != *sxid) {
        free(*sxid);
    }

    *sxid = calloc(sizeof(lixa_ser_xid_t), sizeof(char));
    memcpy(*sxid, xid_str, sizeof(lixa_ser_xid_t));

    txrc = TX_OK;
    return txrc;
}


int tx_xid_deserialize(TXINFO *info, char *sxid) {
    int txrc = TX_FAIL;

    if (!lixa_xid_deserialize(&(info->xid), sxid)) {
        return txrc;
    }

    txrc = TX_OK;
    return txrc;
}
