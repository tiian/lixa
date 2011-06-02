/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_UTILS_H
# define LIXA_UTILS_H



#include <config.h>



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif
#ifdef HAVE_UUID_UUID_H
# include <uuid/uuid.h>
#endif



#include <xa.h>

    

/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_UTILS



/**
 * Minimun length to store a timestamp using ISO standard format; it comprises
 * the null terminator
 */
#define ISO_TIMESTAMP_BUFFER_SIZE 32
/**
 * If @ref lixa_get_program_name is not able to retrieve the current program
 * name, it will retrieve this value
 */
#define DEFAULT_PROGRAM_NAME "lixac"
/**
 * Length of a string that can contain a serialized XID:
 * formatID + separator + gtrid + separator + bqual + terminator
 */
#define LIXA_XID_SERIALIZE_LENGTH (2*sizeof(long)+1+2*sizeof(uuid_t)+1+2*sizeof(uuid_t)+1)



/**
 * A string used to serialize a XID and use it with PostgreSQL.
 * NOTE: this is not XA standard compliant, but it just works in
 * conjunction with LIXA Transaction Manager.
 */
typedef char lixa_ser_xid_t[LIXA_XID_SERIALIZE_LENGTH];



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Print software version info
     * @param stream IN stdio stream to use for fprintf function
     */
    void lixa_print_version(FILE *stream);
    


    /**
     * Retrieve an ISO formatted timestamp from a timeval struct;
     * @param tv IN the timeval struct retrieved with gettimeofday system
     *              function
     * @param buf OUT the buffer will filled with the ISO formatted timestamp
     *                (it must be at least @ref ISO_TIMESTAMP_BUFFER_SIZE
     *                characters long)
     * @param buf_size IN the buffer size
     * @return a standardized reason code
     */
    int lixa_utils_iso_timestamp(const struct timeval *tv, char *buf,
                                 size_t buf_size);



    /**
     * Retrieve the name of the current running program
     * NOTE: this function is strictly PLATFORM DEPENDENT and returns a
     * default constant value on a system there is no implementation for
     * @param buf OUT buffer will contain the output string; the returned
     *            string is NULL TERMINATED, ever
     * @param buf_size IN buffer size (trailing zero uses 1 char in the buffer)
     * @return a standardized reason code
     */
    int lixa_get_program_name(char *buf, size_t buf_size);



    /**
     * Micro seconds sleep based on select call
     * @param usec IN micro seconds
     */
    void lixa_micro_sleep(long usec);


    
    /**
     * Retrieve the LIXA format ID serialized; it's useful to query PostgreSQL
     * and retrieve all the current prepared transactions (xa_recover function)
     * @param lsx OUT the serialized format ID
     */
    void lixa_ser_xid_formatid(lixa_ser_xid_t lsx);



    /**
     * Serialize XID to a string compatible with PostgreSQL
     * @param lsx OUT the serialized XID
     * @param xid IN the XID to be serialized
     * @return TRUE if serialization was completed, FALSE if there was an error
     */
    int lixa_ser_xid_serialize(lixa_ser_xid_t lsx, XID *xid);



    /**
     * Deserialize a string compatible with PostgreSQL to a XID
     * @param lsx IN the string must be deserialized
     * @param xid OUT the deserialized XID
     * @return TRUE if deserialization was completed,
     *         FALSE if there was an error
     */
    int lixa_ser_xid_deserialize(lixa_ser_xid_t lsx, XID *xid);



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



#endif /* LIXA_UTILS_H */
