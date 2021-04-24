/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_tx_rc.h"
#include "xa.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



int lixa_tx_rc_add(lixa_tx_rc_t *ltr, int xa_rc)
{
    enum Exception { NULL_OBJECT
                     , INVALID_OPTION1
                     , INVALID_OPTION2
                     , INVALID_OPTION3
                     , INVALID_OPTION4
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_tx_rc_add\n"));
    TRY {
        /* check object consistency */
        if (NULL == ltr || NULL == ltr->xa_rc)
            THROW(NULL_OBJECT);

        /* check xa_rc */
        if (ltr->commit) /* commit return code check */
            switch(xa_rc) {
                case XA_HEURHAZ:
                case XA_HEURCOM:
                case XA_HEURRB:
                case XA_HEURMIX:
                case XA_RETRY:
                case XA_OK:
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                case XAER_RMERR:
                case XAER_RMFAIL:
                case XAER_NOTA:
                case XAER_INVAL:
                case XAER_PROTO:
                    break;
                case XAER_ASYNC:
                    LIXA_TRACE(("lixa_tx_rc_add: XAER_ASYNC can not be "
                                "accepted because LIXA does not use "
                                "asynchronous operations\n"));
                    THROW(INVALID_OPTION1);
                    break;
                default:
                    LIXA_TRACE(("lixa_tx_rc_add: %d can not be "
                                "accepted for xa_commit()\n", xa_rc));
                    THROW(INVALID_OPTION2);
            }
        else /* rollback return code check */
            switch (xa_rc) {
                case XA_HEURHAZ:
                case XA_HEURCOM:
                case XA_HEURRB:
                case XA_HEURMIX:
                case XA_OK:
                case XA_RBROLLBACK:
                case XA_RBCOMMFAIL:
                case XA_RBDEADLOCK:
                case XA_RBINTEGRITY:
                case XA_RBOTHER:
                case XA_RBPROTO:
                case XA_RBTIMEOUT:
                case XA_RBTRANSIENT:
                case XAER_RMERR:
                case XAER_RMFAIL:
                case XAER_NOTA:
                case XAER_INVAL:
                case XAER_PROTO:
                case LIXA_XAER_HAZARD:
                    break;
                case XAER_ASYNC:
                    LIXA_TRACE(("lixa_tx_rc_add: XAER_ASYNC can not be "
                                "accepted because LIXA does not use "
                                "asynchronous operations\n"));
                    THROW(INVALID_OPTION3);
                    break;
                    /* 
                       case XA_RETRY:
                       this return code is not documented in "DTP: The XA
                       Specification" page 49-51
                    */
                default:
                    LIXA_TRACE(("lixa_tx_rc_add: %d can not be "
                                "accepted for xa_rollback()\n", xa_rc));
                    THROW(INVALID_OPTION4);
            }
            
        g_array_append_val(ltr->xa_rc, xa_rc); 
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_OPTION1:
            case INVALID_OPTION2:
            case INVALID_OPTION3:
            case INVALID_OPTION4:
                ret_cod = LIXA_RC_INVALID_OPTION;
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_tx_rc_get(lixa_tx_rc_t *ltr)
{
    int first, second;
    guint i, j;
    
    /* check the array is not empty */
    if (NULL == ltr || NULL == ltr->xa_rc) {
        LIXA_TRACE(("lixa_tx_get_rc: ltr is NULL, returning TX_FAIL\n"));
        return TX_FAIL;
    }

    /* no value, it's OK (all dynamic without activity) */
    ltr->tx_rc = TX_OK;
    if (0 == ltr->xa_rc->len) {
        LIXA_TRACE(("lixa_tx_get_rc: the array is empty, ltr->tx_rc=%d\n",
                    ltr->tx_rc));
        return ltr->tx_rc;
    }
    
    LIXA_TRACE(("lixa_tx_get_rc: ltr->commit=%d, ltr->tx_commit=%d\n",
                ltr->commit, ltr->tx_commit));
    /* check the first value */
    ltr->tx_rc = TX_OK;
    first = g_array_index(ltr->xa_rc, int, 0);
    switch (first) {
        case XA_OK:
            if (!ltr->commit && ltr->tx_commit)
                ltr->tx_rc = TX_ROLLBACK;
            break;
        case XA_HEURCOM:
            if (ltr->tx_commit)
                ltr->tx_rc = TX_OK;
            else
                ltr->tx_rc = TX_COMMITTED;
            break;
        case XA_HEURRB:
            if (ltr->tx_commit)
                ltr->tx_rc = TX_ROLLBACK;
            else
                ltr->tx_rc = TX_OK;
            break;
        case XA_HEURHAZ:
        case LIXA_XAER_HAZARD:
            ltr->tx_rc = TX_HAZARD;
            break;
        case XA_HEURMIX:
            ltr->tx_rc = TX_MIXED;
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
            if (ltr->tx_commit)
                ltr->tx_rc = TX_ROLLBACK;
            else
                ltr->tx_rc = TX_OK;
            break;
        case XAER_RMFAIL:
        case XAER_INVAL:
        case XAER_PROTO:
            ltr->tx_rc = TX_FAIL;
            break;
        case XA_RETRY:
            if (1 == ltr->xa_rc->len)
                ltr->tx_rc = TX_ERROR;
            else
                ltr->tx_rc = TX_FAIL;
            break;
        default:
            LIXA_TRACE(("lixa_tx_get_rc: first=%d, invalid value\n", first));
            ltr->tx_rc = TX_ERROR;
            break;            
    }
    LIXA_TRACE(("lixa_tx_get_rc: first=%d, ltr->tx_rc=%d\n",
                first, ltr->tx_rc));
    if (1 == ltr->xa_rc->len || TX_FAIL == ltr->tx_rc)
        return ltr->tx_rc;

    /* compare all the values */
    for (i=0; i<ltr->xa_rc->len; ++i) {
        for (j=i+1; j<ltr->xa_rc->len; ++j) {
            first = g_array_index(ltr->xa_rc, int, i);
            second = g_array_index(ltr->xa_rc, int, j);
            LIXA_TRACE(("lixa_tx_get_rc: i=%d, j=%d, first=%d, second=%d\n",
                        i, j, first, second));
            if (ltr->commit) { /* xa_commit() */
                /* [XAER_RMFAIL]      any          ->      [TX_FAIL]
                   [XAER_INVAL]       any          ->      [TX_FAIL]
                   [XAER_PROTO]       any          ->      [TX_FAIL]
                   [XAER_NOTA]        any          ->      [TX_FAIL] */
                if (XAER_RMFAIL == first || XAER_RMFAIL == second ||
                    XAER_INVAL == first || XAER_INVAL == second ||
                    XAER_PROTO == first || XAER_PROTO == second ||
                    XAER_NOTA == first || XAER_NOTA == second) {
                    ltr->tx_rc = TX_FAIL;
                    break;
                }
                /* [XA_HEURMIX]       any          ->      [TX_MIXED] */
                if (XA_HEURMIX == first || XA_HEURMIX == second)
                    if (lixa_tx_rc_hierarchy(TX_MIXED) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_MIXED;
                /* [XA_HEURHAZ]       any          ->      [TX_HAZARD] */
                if (XA_HEURHAZ == first || XA_HEURHAZ == second)
                    if (lixa_tx_rc_hierarchy(TX_HAZARD) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_HAZARD;
                /* [XA_RETRY]         any          ->      [TX_FAIL] */
                if (XA_RETRY == first || XA_RETRY == second)
                    if (lixa_tx_rc_hierarchy(TX_ERROR) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_FAIL;
                /* any commit         any commit   ->      [TX_OK]
                   no check must be performed */
                /* any commit         any rollback ->      [TX_MIXED] */
                if (((XA_OK == first || XA_HEURCOM == first) &&
                     (XA_HEURRB == second || XAER_RMERR == second ||
                      (XA_RBBASE <= second && XA_RBEND >= second))) ||
                    ((XA_OK == second || XA_HEURCOM == second) &&
                     (XA_HEURRB == first || XAER_RMERR == first ||
                      (XA_RBBASE <= first && XA_RBEND >= first))))
                    if (lixa_tx_rc_hierarchy(TX_MIXED) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_MIXED;
                /* any rollback       any rollback ->      [TX_MIXED] */
                if ((XA_HEURRB == first || XAER_RMERR == first ||
                     (XA_RBBASE <= first && XA_RBEND >= first)) &&
                    (XA_HEURRB == second || XAER_RMERR == second ||
                     (XA_RBBASE <= second && XA_RBEND >= second)))
                    if (lixa_tx_rc_hierarchy(TX_ROLLBACK) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_ROLLBACK;
            } else { /* xa_rollback() */
                /* [XAER_RMFAIL]      any          ->      [TX_FAIL]
                   [XAER_INVAL]       any          ->      [TX_FAIL]
                   [XAER_PROTO]       any          ->      [TX_FAIL] */
                if (XAER_RMFAIL == first || XAER_RMFAIL == second ||
                    XAER_INVAL == first || XAER_INVAL == second ||
                    XAER_PROTO == first || XAER_PROTO == second) {
                    ltr->tx_rc = TX_FAIL;
                    break;
                }
                /* [XA_HEURMIX]       any          ->      [TX_MIXED] */
                if (XA_HEURMIX == first || XA_HEURMIX == second)
                    if (lixa_tx_rc_hierarchy(TX_MIXED) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_MIXED;
                /* [XA_HEURHAZ]       any          ->      [TX_HAZARD] */
                if (XA_HEURHAZ == first || XA_HEURHAZ == second ||
                    LIXA_XAER_HAZARD == first || LIXA_XAER_HAZARD == second)
                    if (lixa_tx_rc_hierarchy(TX_HAZARD) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_HAZARD;
                /* [XA_RETRY]         any          ->      [TX_FAIL] */
                if (XA_RETRY == first || XA_RETRY == second)
                    if (lixa_tx_rc_hierarchy(TX_ERROR) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_FAIL;
                /* [XA_HEURCOM]       [XA_HEURCOM] ->      Note 3 */
                if (XA_HEURCOM == first && XA_HEURCOM == second)
                    if (!ltr->tx_commit)
                        if (lixa_tx_rc_hierarchy(TX_COMMITTED) <
                            lixa_tx_rc_hierarchy(ltr->tx_rc))
                            ltr->tx_rc = TX_COMMITTED;
                /* [XA_HEURCOM]       any rollback ->      [TX_MIXED] */
                if ((XA_HEURCOM == first &&
                     (XA_HEURRB == second || XAER_RMERR == second ||
                      XA_OK == second ||
                      (XA_RBBASE <= second && XA_RBEND >= second))) ||
                    (XA_HEURCOM == second &&
                     (XA_HEURRB == first || XAER_RMERR == first ||
                      XA_OK == first ||
                      (XA_RBBASE <= first && XA_RBEND >= first))))
                    if (lixa_tx_rc_hierarchy(TX_MIXED) <
                        lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_MIXED;
                /* tx_commit()/xa_rollback()
                   [XA_HEURCOM]       [XAER_NOTA]  ->      [TX_FAIL]
                   tx_rollback()/xa_rollback()
                   [XA_HEURCOM]       [XAER_NOTA]  ->      [TX_MIXED] */ 
                if ((XA_HEURCOM == first && XAER_NOTA == second) ||
                    (XA_HEURCOM == second && XAER_NOTA == first)) {
                    if (ltr->tx_commit) {
                        ltr->tx_rc = TX_FAIL;
                        break;
                    } else if (lixa_tx_rc_hierarchy(TX_MIXED) <
                               lixa_tx_rc_hierarchy(ltr->tx_rc))
                        ltr->tx_rc = TX_MIXED;
                }
                /* any rollback       any rollback ->      Note 4 */
                if ((XA_HEURRB == first || XAER_RMERR == first ||
                      XA_OK == first || XAER_NOTA == first ||
                      (XA_RBBASE <= first && XA_RBEND >= first)) &&
                     (XA_HEURRB == second || XAER_RMERR == second ||
                      XA_OK == second || XAER_NOTA == second ||
                      (XA_RBBASE <= second && XA_RBEND >= second)))
                    if (ltr->tx_commit)
                        if (lixa_tx_rc_hierarchy(TX_ROLLBACK) <
                            lixa_tx_rc_hierarchy(ltr->tx_rc))
                            ltr->tx_rc = TX_ROLLBACK;
            }
        } /* for j */
        if (TX_FAIL == ltr->tx_rc)
            break;
        LIXA_TRACE(("lixa_tx_get_rc: ltr->tx_rc=%d\n", ltr->tx_rc));
    } /* for i */
    LIXA_TRACE(("lixa_tx_get_rc: ltr->tx_rc=%d\n", ltr->tx_rc));
    return ltr->tx_rc;
}

