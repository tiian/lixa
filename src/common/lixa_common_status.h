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
#ifndef LIXA_COMMON_STATUS_H
# define LIXA_COMMON_STATUS_H



#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_UUID_UUID_H
# include <uuid/uuid.h>
#endif



#include <xa.h>
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
 * This is the formatID associated to XID generated by LIXA
 * 32 bytes for gtrid (Global TRansaction ID)
 * 16 bytes for bqual (Branch QUALifier)
 */
#define LIXA_XID_FORMAT_ID   0x494c4158



/**
 * Character separator used when serializing/deserializing a xid
 */
#define LIXA_XID_SEPARATOR   '.'



/**
 * Minimum size of a buffer used to store a serialized xid (the null terminator
 * is computed in this constant
 */
#define LIXA_XID_SERIALIZED_BUFFER_SIZE  (2*(2*sizeof(uuid_t)+4)+2)



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
     * Transaction branch association state as in "X/Open CAE Specification -
     * Distribute Transaction Processing: The XA Specification - chapter 6
     * table 6-2
     */
    int   xa_t_state;
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



/**
 * Global branch qualifier: it's unique for every thread of a process;
 * every process acts as a distinct transaction manager
 */
extern uuid_t lixa_xid_global_bqual;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    static inline void common_status_conthr_init(
        struct common_status_conthr_s *csc) {
        csc->txstate = TX_STATE_S0;
        memset(&csc->xid, 0, sizeof(XID));
        csc->will_commit = FALSE;
        csc->will_rollback = FALSE;
        csc->finished = FALSE;
        return;
    }


    
    static inline void common_status_rsrmgr_init(
        struct common_status_rsrmgr_s *csr) {
        csr->xa_r_state = XA_STATE_R0;
        csr->xa_t_state = XA_STATE_T0;
        csr->xa_s_state = XA_STATE_S0;
        csr->next_verb = LIXA_MSG_VERB_NULL;
        return;
    }



    /**
     * Use an MD5 digest to set the global bqual
     * <b>Note:</b> this function is not thread safe and MUST be called
     * with a serialization technique
     * @param md5_digest_hex IN pointer to a string of
     * @ref MD5_DIGEST_LENGTH * 2 characters
     */
    void xid_set_global_bqual(const char *md5_digest_hex);



    /**
     * Check if the branch qualifier of the transaction matched the global
     * branch qualifier of current running transaction manager instance
     * @param xid IN transaction id to inspect
     * @return a boolean value
     */
    int xid_bqual_is_global(const XID *xid);

    
                            
    /**
     * Create a new XID
     * @param xid OUT the generated unique transaction id
     */
    void xid_create_new(XID *xid);



    /**
     * Retrieve an ASCII string with the human readable version of the gtrid
     * (Global TRansaction ID).
     * @param xid IN unique transaction id
     * @return a string MUST be freed by the caller using "free" function or
     *         NULL if an error happens
     */
    char *xid_get_gtrid_ascii(const XID *xid);


    
    /**
     * Retrieve an ASCII string with the human readable version of the bqual
     * (Branch QUALifier).
     * @param xid IN unique transaction id
     * @return a string MUST be freed by the caller using "free" function or
     *         NULL if an error happens
     */
    char *xid_get_bqual_ascii(const XID *xid);



    /**
     * Retrieve an ASCII string with the human readable version of the 
     * transaction id; the serialized string can be transmitted over a network
     * without encoding issues
     * @param xid IN unique transaction id
     * @return a string MUST be freed by the caller using "free" function or
     *         NULL if an error happens
     */
    char *xid_serialize(const XID *xid);



    /**
     * Retrieve a XID object from a serialized version
     * @param ser_xid IN string containing a serialized XID (
     * @ref xid_serialize )
     * @param xid OUT transaction id object
     * @return a standardized return code
     */
    int xid_deserialize(char *ser_xid, XID *xid);

    

    /**
     * Compare two xids
     * @param a IN first object to compare
     * @param b IN second object to compare
     * @return if (a==b) --> 0 <br>
     *         if (a<b) --> -1 <br>
     *         if (a>b) --> +1
     */
    int xid_compare(const XID *a, const XID *b);


    
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
