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



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_tx_rc.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



/*
 * The implementation of this "method" is very conservative because it must
 * be an "easy" method to debug forever.
 * Note 1: a multiple nidified switch was preferred to a multidimensional
 *         array because the step by step debugging is more intuitive
 * Note 2: I am not sure the result of the function coded in "The TX
 *         (Transaction Demarcation) Specification, Appendix B.5 is
 *         independent from the order of the arguments (the order of the
 *         comparisons). A mathematical proof could be found, but a next
 *         successive fix - due to some odd circumstance - would require a new
 *         proof. Due to the critical aspects related to a wrong decision in
 *         this function, I prefer to compare the last XA return code with all
 *         the previous return codes. The loop should never be a very long
 *         loop (every step is a resource manager that partecipated in the
 *         transaction).
 */
int lixa_tx_rc_add(lixa_tx_rc_t *ltr, int xa_rc)
{
    enum Exception { UNEXPECTED_XA_RC1
                     , UNEXPECTED_XA_RC2
                     , UNEXPECTED_XA_RC3
                     , UNEXPECTED_XA_RC4
                     , UNEXPECTED_XA_RC5
                     , UNEXPECTED_XA_RC6
                     , UNEXPECTED_XA_RC7
                     , UNEXPECTED_XA_RC8
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_tx_rc_add: (xa_rc=%d)\n", xa_rc));
    TRY {
        int first_rm = ltr->xa_rc->len == 0;
        int tmp_tx_rc = TX_FAIL;
        guint i;
        int prev_xa_rc, prev_tx_rc;

        if (first_rm) {
            /* single RM commit/rollback table */
            switch (xa_rc) {
                case XA_OK:
                    if (ltr->commit)
                        tmp_tx_rc = TX_OK;
                    else if (ltr->tx_commit)
                        tmp_tx_rc = TX_ROLLBACK;
                    else
                        tmp_tx_rc = TX_OK;
                    break;
                case XA_HEURCOM:
                    if (ltr->commit)
                        tmp_tx_rc = TX_OK;
                    else if(ltr->tx_commit)
                        tmp_tx_rc = TX_MIXED;
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
                    THROW(UNEXPECTED_XA_RC1);
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
                    case XA_HEURCOM: /* any commit */
                    case XA_OK:
                        prev_tx_rc = TX_FAIL;
                        tmp_tx_rc = TX_OK;
                        for (i=0; i<ltr->xa_rc->len; ++i) {
                            prev_xa_rc = g_array_index(ltr->xa_rc, int, i);
                            switch (prev_xa_rc) {
                                case XA_HEURCOM: /* any commit */
                                case XA_OK:
                                    prev_tx_rc = TX_OK;
                                    break;
                                case XA_RBROLLBACK: /* any rollback */
                                case XA_RBCOMMFAIL:
                                case XA_RBDEADLOCK:
                                case XA_RBINTEGRITY:
                                case XA_RBOTHER:
                                case XA_RBPROTO:
                                case XA_RBTIMEOUT:
                                case XA_RBTRANSIENT:
                                case XA_HEURRB:
                                case XAER_RMERR:
                                case XA_HEURMIX:
                                    prev_tx_rc = TX_MIXED;
                                    break;
                                case XA_HEURHAZ:
                                    /* case test TX/3.3.1/1.1 */
                                    prev_tx_rc = TX_HAZARD;
                                    break;
                                case XAER_NOTA:
                                    prev_tx_rc = TX_FAIL;
                                    break;
                                default:
                                    THROW(UNEXPECTED_XA_RC2);
                            } /* switch (prev_xa_rc) */
                            if (lixa_tx_rc_hierarchy(prev_tx_rc) <
                                lixa_tx_rc_hierarchy(tmp_tx_rc))
                                tmp_tx_rc = prev_tx_rc;
                        } /* for (i=0; ... */
                        break;
                    case XA_RBROLLBACK: /* any rollback */
                    case XA_RBCOMMFAIL:
                    case XA_RBDEADLOCK:
                    case XA_RBINTEGRITY:
                    case XA_RBOTHER:
                    case XA_RBPROTO:
                    case XA_RBTIMEOUT:
                    case XA_RBTRANSIENT:
                    case XA_HEURRB:
                    case XAER_RMERR:
                        prev_tx_rc = TX_FAIL;
                        tmp_tx_rc = TX_OK;
                        for (i=0; i<ltr->xa_rc->len; ++i) {
                            prev_xa_rc = g_array_index(ltr->xa_rc, int, i);
                            switch (prev_xa_rc) {
                                case XA_HEURCOM: /* any commit */
                                case XA_OK:
                                case XA_HEURMIX:
                                    prev_tx_rc = TX_MIXED;
                                    break;
                                case XA_RBROLLBACK: /* any rollback */
                                case XA_RBCOMMFAIL:
                                case XA_RBDEADLOCK:
                                case XA_RBINTEGRITY:
                                case XA_RBOTHER:
                                case XA_RBPROTO:
                                case XA_RBTIMEOUT:
                                case XA_RBTRANSIENT:
                                case XA_HEURRB:
                                case XAER_RMERR:
                                    prev_tx_rc = TX_ROLLBACK;
                                    break;
                                case XA_HEURHAZ:
                                    prev_tx_rc = TX_HAZARD;
                                    break;
                                default:
                                    THROW(UNEXPECTED_XA_RC3);
                            } /* switch (prev_xa_rc) */
                            if (lixa_tx_rc_hierarchy(prev_tx_rc) <
                                lixa_tx_rc_hierarchy(tmp_tx_rc))
                                tmp_tx_rc = prev_tx_rc;
                        } /* for (i=0; ... */
                        break;
                    default:
                        THROW(UNEXPECTED_XA_RC4);
                } /* switch (xa_rc) */
            } else {
                /* multiple RMs rollback table */
                switch (xa_rc) {
                    case XAER_RMFAIL:
                    case XAER_INVAL:
                    case XAER_PROTO:
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
                        prev_tx_rc = TX_FAIL;
                        tmp_tx_rc = TX_OK;
                        for (i=0; i<ltr->xa_rc->len; ++i) {
                            prev_xa_rc = g_array_index(ltr->xa_rc, int, i);
                            switch (prev_xa_rc) {
                                case XA_HEURCOM:
                                    if (ltr->tx_commit)
                                        prev_tx_rc = TX_OK;
                                    else
                                        prev_tx_rc = TX_COMMITTED;
                                    break;
                                case XA_RBROLLBACK: /* any rollback */
                                case XA_RBCOMMFAIL:
                                case XA_RBDEADLOCK:
                                case XA_RBINTEGRITY:
                                case XA_RBOTHER:
                                case XA_RBPROTO:
                                case XA_RBTIMEOUT:
                                case XA_RBTRANSIENT:
                                case XA_HEURRB:
                                case XAER_RMERR:
                                case XA_OK:
                                case XA_HEURMIX:
                                    prev_tx_rc = TX_MIXED;
                                    break;
                                case XAER_NOTA:
                                    if (ltr->tx_commit)
                                        prev_tx_rc = TX_FAIL;
                                    else
                                        prev_tx_rc = TX_MIXED;
                                    break;
                                case XA_HEURHAZ:
                                    prev_tx_rc = TX_HAZARD;
                                    break;
                                default:
                                    THROW(UNEXPECTED_XA_RC5);
                            } /* switch (prev_xa_rc) */
                            if (lixa_tx_rc_hierarchy(prev_tx_rc) <
                                lixa_tx_rc_hierarchy(tmp_tx_rc))
                                tmp_tx_rc = prev_tx_rc;
                        } /* for (i=0; ... */
                        break;
                    case XA_RBROLLBACK: /* any rollback */
                    case XA_RBCOMMFAIL:
                    case XA_RBDEADLOCK:
                    case XA_RBINTEGRITY:
                    case XA_RBOTHER:
                    case XA_RBPROTO:
                    case XA_RBTIMEOUT:
                    case XA_RBTRANSIENT:
                    case XA_HEURRB:
                    case XAER_RMERR:
                    case XA_OK:
                        prev_tx_rc = TX_FAIL;
                        tmp_tx_rc = TX_OK;
                        for (i=0; i<ltr->xa_rc->len; ++i) {
                            prev_xa_rc = g_array_index(ltr->xa_rc, int, i);
                            switch (prev_xa_rc) {
                                case XA_HEURCOM:
                                case XA_HEURMIX:
                                    prev_tx_rc = TX_MIXED;
                                    break;
                                case XA_RBROLLBACK: /* any rollback */
                                case XA_RBCOMMFAIL:
                                case XA_RBDEADLOCK:
                                case XA_RBINTEGRITY:
                                case XA_RBOTHER:
                                case XA_RBPROTO:
                                case XA_RBTIMEOUT:
                                case XA_RBTRANSIENT:
                                case XA_HEURRB:
                                case XA_OK:
                                case XAER_NOTA:
                                    if (ltr->tx_commit)
                                        prev_tx_rc = TX_ROLLBACK;
                                    else
                                        prev_tx_rc = TX_OK;
                                    break;
                                case XAER_RMERR:
                                    if (ltr->tx_commit)
                                        prev_tx_rc = TX_HAZARD;
                                    else
                                        prev_tx_rc = TX_OK;
                                    break;
                                case XA_HEURHAZ:
                                    /* case test TX/3.3.1/1.0 */
                                    prev_tx_rc = TX_HAZARD;
                                    break;
                                default:
                                    THROW(UNEXPECTED_XA_RC6);
                            } /* switch (prev_xa_rc) */
                            if (lixa_tx_rc_hierarchy(prev_tx_rc) <
                                lixa_tx_rc_hierarchy(tmp_tx_rc))
                                tmp_tx_rc = prev_tx_rc;
                        } /* for (i=0; ... */
                        break;
                    case XAER_NOTA:
                        prev_tx_rc = TX_FAIL;
                        tmp_tx_rc = TX_OK;
                        for (i=0; i<ltr->xa_rc->len; ++i) {
                            prev_xa_rc = g_array_index(ltr->xa_rc, int, i);
                            switch (prev_xa_rc) {
                                case XA_HEURCOM:
                                    if (ltr->tx_commit)
                                        prev_tx_rc = TX_FAIL;
                                    else
                                        prev_tx_rc = TX_MIXED;
                                    break;
                                case XA_RBROLLBACK: /* any rollback */
                                case XA_RBCOMMFAIL:
                                case XA_RBDEADLOCK:
                                case XA_RBINTEGRITY:
                                case XA_RBOTHER:
                                case XA_RBPROTO:
                                case XA_RBTIMEOUT:
                                case XA_RBTRANSIENT:
                                case XA_HEURRB:
                                case XAER_RMERR:
                                case XA_OK:
                                case XAER_NOTA:
                                    if (ltr->tx_commit)
                                        prev_tx_rc = TX_ROLLBACK;
                                    else
                                        prev_tx_rc = TX_OK;
                                    break;
                                case XA_HEURHAZ:
                                    prev_tx_rc = TX_HAZARD;
                                    break;
                                default:
                                    THROW(UNEXPECTED_XA_RC7);
                            } /* switch (prev_xa_rc) */
                            if (lixa_tx_rc_hierarchy(prev_tx_rc) <
                                lixa_tx_rc_hierarchy(tmp_tx_rc))
                                tmp_tx_rc = prev_tx_rc;
                        } /* for (i=0; ... */
                        break;
                    default:
                        THROW(UNEXPECTED_XA_RC8);
                } /* switch (xa_rc) */
            } /* if (ltr->commit) */
        } /* else if (TX_FAIL != ltr->tx_rc) */

        /* store the new values */
        if (lixa_tx_rc_hierarchy(tmp_tx_rc) <
            lixa_tx_rc_hierarchy(ltr->tx_rc))
            ltr->tx_rc = tmp_tx_rc;
        g_array_append_val(ltr->xa_rc, xa_rc); 
        LIXA_TRACE(("lixa_tx_rc_add: ltr->tx_rc=%d\n", ltr->tx_rc));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case UNEXPECTED_XA_RC1:
            case UNEXPECTED_XA_RC2:
            case UNEXPECTED_XA_RC3:
            case UNEXPECTED_XA_RC4:
            case UNEXPECTED_XA_RC5:
            case UNEXPECTED_XA_RC6:
            case UNEXPECTED_XA_RC7:
            case UNEXPECTED_XA_RC8:
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

