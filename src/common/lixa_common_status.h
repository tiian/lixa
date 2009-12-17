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
#ifndef LIXA_COMMON_STATUS_H
# define LIXA_COMMON_STATUS_H



#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <xa.h>
#include <lixa_xml_msg.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_NO_TRACE



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
 * Store the status of the current control thread is partecipating in the
 * transaction; this is the volatile local copy, the persistent remote copy
 * is stored on the server side
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
};



/**
 * Store the status of every resource manager is partecipating in the
 * transaction; this is the volatile local copy, the persistent remote copy
 * is stored on the server side
 */
struct common_status_rsrmgr_s {
    /**
     * State of the resource manager as in "X/Open CAE Specification -
     * Distribute Transaction Processing: The XA Specification - chapter 6
     */
    int   xastate;
    /**
     * This is the next verb will be issued against the resource manager;
     * @ref xastate shows the reached state, this property is used to trace
     * a tentative to a new state
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
        return;
    }


    
    static inline void common_status_rsrmgr_init(
        struct common_status_rsrmgr_s *csr) {
        csr->xastate = XA_STATE_R0;
        csr->next_verb = LIXA_MSG_VERB_NULL;
        return;
    }


    
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
