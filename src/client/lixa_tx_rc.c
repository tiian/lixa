/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_tx_rc.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



int lixa_tx_rc_add(lixa_tx_rc_t *ltr, int xa_rc)
{
    enum Exception { UNEXPECTED_XA_RC
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_tx_rc_add\n"));
    TRY {
        int first_rm = ltr->xa_rc->len == 0;
        int tmp_tx_rc = TX_FAIL;
        guint i;
        int prev_xa_rc;

        if (first_rm) {
            switch (xa_rc) {
                case XA_HEURCOM:
                    if (ltr->commit)
                        tmp_tx_rc = TX_OK;
                    else
                        tmp_tx_rc = TX_COMMITTED;
                    break;
                case XA_HEURRB:
                    if (ltr->commit)
                        tmp_tx_rc = TX_ROLLBACK;
                    else
                        tmp_tx_rc = TX_OK;
                    break;
                case XA_HEURHAZ:
                    tmp_tx_rc = TX_HAZARD;
                    break;
                case XA_HEURMIX:
                    tmp_tx_rc = TX_MIXED;
                    break;
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                case XAER_RMERR:
                case XAER_NOTA:
                    if (ltr->commit)
                        tmp_tx_rc = TX_ROLLBACK;
                    else
                        tmp_tx_rc = TX_OK;
                    break;
                case XAER_RMFAIL:
                case XAER_INVAL:
                case XAER_PROTO:
                    tmp_tx_rc = TX_FAIL;
                    break;
                case XA_RETRY:
                    tmp_tx_rc = TX_ERROR;
                    break;
                default:
                    THROW(UNEXPECTED_XA_RC);
            } /* switch (xa_rc) */
        } else if (TX_FAIL != ltr->tx_rc) {
            if (ltr->commit) {
                /* multiple RMs commit table */
                switch (xa_rc) {
                    case XAER_RMFAIL:
                    case XAER_INVAL:
                    case XAER_PROTO:
                    case XAER_NOTA:
                        tmp_tx_rc = TX_FAIL;
                        break;
                    case XA_HEURMIX:
                        tmp_tx_rc = TX_MIXED;
                        break;
                    case XA_HEURHAZ:
                        tmp_tx_rc = TX_HAZARD;
                        break;
                    case XA_RETRY:
                        tmp_tx_rc = TX_ERROR;
                        break;
                    case XA_HEURCOM:
                    case XA_OK:
                        for (i=0; i<ltr->xa_rc->len; ++i) {
                            prev_xa_rc = g_array_index(ltr->xa_rc, int, i);
                            /* @@@ restart from here */
                        }
                } /* switch (xa_rc) */
            } else {
                /* multiple RMs rollback table */
                switch (xa_rc) {
                    case XAER_RMFAIL:
                    case XAER_INVAL:
                    case XAER_PROTO:
                    case XAER_NOTA:
                        tmp_tx_rc = TX_FAIL;
                        break;
                    case XA_HEURMIX:
                        tmp_tx_rc = TX_MIXED;
                        break;
                    case XA_HEURHAZ:
                        tmp_tx_rc = TX_HAZARD;
                        break;
                    case XA_RETRY:
                        tmp_tx_rc = TX_ERROR;
                        break;
                } /* switch (xa_rc) */
            } /* if (ltr->commit) */
        } /* else if (TX_FAIL != ltr->tx_rc) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case UNEXPECTED_XA_RC:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_tx_rc_add/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

