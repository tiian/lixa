/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_XID_H
# define LIXA_XID_H



#include <config.h>



#ifdef HAVE_UUID_H
# include <uuid.h>
#endif
#ifdef HAVE_UUID_UUID_H
# include <uuid/uuid.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include "xa.h"
#include "lixa_trace.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_XID



/**
 * This is the formatID associated to XID generated by LIXA
 * 32 bytes for gtrid (Global TRansaction ID)
 * 16 bytes for bqual (Branch QUALifier)
 */
extern const long LIXA_XID_FORMAT_ID;



/**
 * Character separator used when serializing/deserializing a xid
 */
#define LIXA_XID_SEPARATOR '.'



/**
 * Length of a string that can contain a serialized XID:
 * formatID + separator + gtrid + separator + bqual + terminator
 */
#define LIXA_XID_SERIALIZE_LENGTH (LIXA_SERIALIZED_LONG_INT+1+2*XIDDATASIZE+1+1)



#define LIXA_XID_GTRID_ASCII_LENGTH (2 * sizeof(uuid_t) + 4 + 1)
#define LIXA_XID_BQUAL_ASCII_LENGTH (2 * sizeof(uuid_t) + 4 + 1)



/**
 * A string used to serialize a XID.
 * NOTE: this is not XA standard compliant, but it just works in
 * conjunction with LIXA Transaction Manager.
 */
typedef char lixa_ser_xid_t[LIXA_XID_SERIALIZE_LENGTH];



/**
 * Global branch qualifier: it's unique for every thread of a process;
 * every process acts as a distinct transaction manager
 */
extern uuid_t lixa_xid_global_bqual;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    
    /**
     * Use an MD5 digest to set a branch qualifier.
     * <b>Note:</b> this function is not thread safe and if used to set
     *         lixa_xid_global_bqual MUST be called with a serialization
     *         technique
     * @param[in] md5_digest_hex pointer to a string of
     *            @ref MD5_DIGEST_LENGTH * 2 characters
     * @param[out] branch_qualifier that must be set
     */
    void lixa_xid_set_bqual(const char *md5_digest_hex,
                            uuid_t branch_qualifier);


    
    /**
     * Check if the branch qualifier of the transaction matched the global
     * branch qualifier of current running transaction manager instance
     * @param[in] xid transaction id to inspect
     * @return a boolean value
     */
    int lixa_xid_bqual_is_global(const XID *xid);


    
    /**
     * Create a new XID
     * @param[in] branch_qualifier the branch qualifier that must be assigned
     *            to xid or NULL if the global one is OK
     * @param[out] xid the generated unique transaction id
     */
    void lixa_xid_create_new(uuid_t branch_qualifier, XID *xid);



    /**
     * Create a new subordinate XID for a branch of a superior XID: global
     * transaction ID is preserved, branch transaction ID is partially
     * preserved and partially new
     * @param[in] superior is the XID of the superior branch
     * @param[out] subordinate is the XID of the subordinate branch
     */
    void lixa_xid_branch_new(const XID *superior, XID *subordinate);

    
    
    /**
     * Generate a new branch qualifier and update the XID
     * @param[in,out] xid the updated XID
     */
    void lixa_xid_create_new_bqual(XID *xid);


    
    /**
     * Retrieve an ASCII string with the human readable version of the gtrid
     * (Global TRansaction ID).
     * @param[in] xid unique transaction id
     * @return a string MUST be freed by the caller using "free" function or
     *         NULL if an error happens
     */
    char *lixa_xid_get_gtrid_ascii(const XID *xid);



    /**
     * Retrieve an ASCII string with the human readable version of the bqual
     * (Branch QUALifier).
     * @param[in] xid unique transaction id
     * @return a string MUST be freed by the caller using "free" function or
     *         NULL if an error happens
     */
    char *lixa_xid_get_bqual_ascii(const XID *xid);


    
    /**
     * Retrieve the raw version of GTRID (Global TRansaction ID).
     * NOTE: output buffer is filled with 0 at the end
     * @param[in] xid unique transaction id
     * @param[out] gtrid is a buffer allocated by the caller with at least
     *             XIDDATASIZE bytes
     * @return the number of bytes copied in gtrid
     */
    long lixa_xid_get_gtrid(const XID *xid, char *gtrid);

    
    
    /**
     * Retrieve the raw version of BQUAL (Branch QUALifier).
     * NOTE: output buffer is filled with 0 at the end
     * @param[in] xid unique transaction id
     * @param[out] bqual is a buffer allocated by the caller with at least
     *             XIDDATASIZE bytes
     * @return the number of bytes copied in bqual
     */
    long lixa_xid_get_bqual(const XID *xid, char *bqual);

    

    /**
     * Retrieve the formatID
     * @param[in] xid unique transaction id
     * @return formatID as a native long
     */
    static inline long lixa_xid_get_formatID(const XID *xid) {
        return xid->formatID; }


    
    /**
     * Reset a xid structure
     * @param[in,out] xid transaction id to be resetted
     */
    static inline void lixa_xid_reset(XID *xid)
    {
        memset(xid, 0, sizeof(XID));
        xid->formatID = NULLXID;
    }


    
    /**
     * Check if the XID is in a reset state
     * @param[in] xid transaction id to be checked
     * @return TRUE if the transaction id is in a reset state, otherwise FALSE
     */
    static inline int lixa_xid_is_reset(XID *xid)
    {
        if (NULL == xid || NULLXID == xid->formatID) {
            return TRUE;
        }

        return FALSE;
    }


    
    /**
     * Retrieve the LIXA format ID serialized; it's useful to query PostgreSQL
     * and retrieve all the current prepared transactions (xa_recover function)
     * @param[out] lsx the serialized format ID
     */
    void lixa_xid_formatid(lixa_ser_xid_t lsx);


    
    /**
     * Serialize XID to a string
     * @param[in] xid the XID to be serialized
     * @param[out] lsx the serialized XID
     * @return TRUE if serialization was completed, FALSE if there was an error
     */
    int lixa_xid_serialize(const XID *xid, lixa_ser_xid_t lsx);


    
    /**
     * Deserialize a string compatible with PostgreSQL to a XID
     * @param[out] xid the deserialized XID
     * @param[in] lsx the string must be deserialized
     * @return TRUE if deserialization was completed, <br>
     *         FALSE if there was an error
     */
    int lixa_xid_deserialize(XID *xid, const lixa_ser_xid_t lsx);


    
    /**
     * Compare two xids
     * @param[in] a first object to compare
     * @param[in] b second object to compare
     * @return if (a==b) --> 0 <br>
     *         if (a<b) --> -1 <br>
     *         if (a>b) --> +1
     */
    int lixa_xid_compare(const XID *a, const XID *b);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */

/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif /* LIXA_XID_H */
