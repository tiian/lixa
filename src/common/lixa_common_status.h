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
#ifndef LIXA_COMMON_STATUS_H
# define LIXA_COMMON_STATUS_H



#include <config.h>



#include <lixa_xml_msg.h>
#include <lixa_trace.h>
#include <lixa_config.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_STATUS



/**
 * No RMs have been opened or initialized. An application thread of control
 * cannot start a global transaction until it has successfully opened its RMs
 * via @ref tx_open().
 */
#define TX_STATE_S0    0
/**
 * The thread haso opened its RMs but is not in a transaction. Its transaction
 * control characteristics is TX_UNCHAINED
 */
#define TX_STATE_S1    1
/**
 * The thread haso opened its RMs but is not in a transaction. Its transaction
 * control characteristics is TX_CHAINED
 */
#define TX_STATE_S2    2
/**
 * The thread has opened its RMs and is in a transaction. Its transaction
 * control characteristics is TX_UNCHAINED
 */
#define TX_STATE_S3    3
/**
 * The thread has opened its RMs and is in a transaction. Its transaction
 * control characteristics is TX_CHAINED
 */
#define TX_STATE_S4    4



/**
 * Resource manager state un-initialized
 */
#define XA_STATE_R0    0
/**
 * Resource manager state initialized
 */
#define XA_STATE_R1    1
/**
 * Transaction branch association state "not associated"
 */
#define XA_STATE_T0   10
/**
 * Transaction branch association state "associated"
 */
#define XA_STATE_T1   11
/**
 * Transaction branch association state "association suspended"
 */
#define XA_STATE_T2   12
/**
 * Transaction branch association state (dynamic registration)
 * "not registered"
 */
#define XA_STATE_D0   20
/**
 * Transaction branch association state (dynamic registration)
 * "registered with valid XID"
 */
#define XA_STATE_D1   21
/**
 * Transaction branch association state (dynamic registration)
 * "registration suspended"
 */
#define XA_STATE_D2   22
/**
 * Transaction branch association state (dynamic registration)
 * "registered with NULLXID"
 */
#define XA_STATE_D3   23
/**
 * Transaction branch state "non-existent transaction"
 */
#define XA_STATE_S0   30
/**
 * Transaction branch state "active"
 */
#define XA_STATE_S1   31
/**
 * Transaction branch state "idle"
 */
#define XA_STATE_S2   32
/**
 * Transaction branch state "prepared"
 */
#define XA_STATE_S3   33
/**
 * Transaction branch state "rollback only"
 */
#define XA_STATE_S4   34
/**
 * Transaction branch state "heuristically completed"
 */
#define XA_STATE_S5   35



/**
 * Store the status of the current control thread is partecipating in the
 * transaction; the client keeps a volatile copy, the server stores the
 * persistent copy
 */
struct common_status_conthr_s {
    /**
     * State of the control thread as in "X/Open CAE Specification -
     * Distribute Transaction Processing: The TX (Transaction Demarcation)
     * Specification - chapter 7
     */
    int   txstate;
    /**
     * Transaction ID
     */
    XID   xid;
    /**
     * Boolean value: 
     * the Application Program called @ref tx_commit and the current status
     * is moving to resource commit
     */
    int   will_commit;
    /**
     * Boolean value:
     * the Application Program called @ref tx_rollback and the current status
     * is moving to resource rollback or the Application Program called
     * @ref tx_commit and one resource manager was not able to prepare for
     * commit
     */
    int   will_rollback;
    /**
     * Boolean value:
     * the Application Program transaction is completed and there are no more
     * tasks related to it
     */
    int   finished;
};



/**
 * Store the status of every resource manager is partecipating in the
 * transaction; this is the volatile local copy, the persistent remote copy
 * is stored on the server side
 */
struct common_status_rsrmgr_s {
    /**
     * Resource manager state as in "X/Open CAE Specification -
     * Distribute Transaction Processing: The XA Specification - chapter 6
     * table 6-1
     */
    int   xa_r_state;
    /**
     * The resource manager is using dynamic registration (TRUE) or static
     * registration (FALSE)
     */
    int   dynamic;
    /**
     * Transaction branch association state as in "X/Open CAE Specification -
     * Distribute Transaction Processing: The XA Specification - chapter 6
     * table 6-2 (static registration) and table 6-3 (dynamic registration)
     */
    int   xa_td_state;
    /**
     * Transaction branch state as in "X/Open CAE Specification -
     * Distribute Transaction Processing: The XA Specification - chapter 6
     * table 6-4
     */
    int   xa_s_state;
    /**
     * This is the next verb will be issued against the resource manager;
     * xa_r_state, xa_t_state, xa_s_state show the reached state, this property
     * is used to trace a tentative to a new state
     */
    int   next_verb;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    static inline void common_status_conthr_init(
        struct common_status_conthr_s *csc) {
        csc->txstate = TX_STATE_S0;
        memset(&csc->xid, 0, sizeof(XID));
        csc->xid.formatID = NULLXID;
        csc->will_commit = FALSE;
        csc->will_rollback = FALSE;
        csc->finished = FALSE;
        return;
    }



    /**
     * Initialize a common status resource manager structure
     * @param csr IN/OUT reference to the struct must be initialized
     * @param dynamic IN boolean: the resource manager uses dynamic
     *                registration (TRUE) or static registration (FALSE)
     */
    static inline void common_status_rsrmgr_init(
        struct common_status_rsrmgr_s *csr, int dynamic) {
        csr->xa_r_state = XA_STATE_R0;
        csr->dynamic = dynamic;
        if (dynamic)
            csr->xa_td_state = XA_STATE_D0;
        else
            csr->xa_td_state = XA_STATE_T0;
        csr->xa_s_state = XA_STATE_S0;
        csr->next_verb = LIXA_MSG_VERB_NULL;
        return;
    }



    /**
     * Display the content of a common status control thread object
     * @param csc IN object reference
     */
    void common_status_conthr_display(
        const struct common_status_conthr_s *csc);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_COMMON_STATUS_H */
