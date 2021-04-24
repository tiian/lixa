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
#ifndef LIBLIXAPQ_H
# define LIBLIXAPQ_H



#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_trace.h>
/* Utilities for resource manager without standard switch file */
#include <lixa_sw.h>
/* MySQL front-end */
#include <mysql.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_XA_SWITCH



/**
 * This struct contains the pointers to XA methods and properties
 */
struct xa_switch_t xamyls;



/**
 * Length of a string that can contain a serialized XID for MySQL:
 * ' + gtrid + ',' + bqual + ', + formatID
 */
#define LIXA_MY_XID_SERIALIZE_LENGTH (2*XIDDATASIZE+5+LIXA_SERIALIZED_LONG_INT+1)



/**
 * A string used to serialize a XID for MySQL.
 * NOTE: this is not XA standard compliant, but it just works in
 * conjunction with LIXA Transaction Manager.
 */
typedef char lixa_my_ser_xid_t[LIXA_MY_XID_SERIALIZE_LENGTH];



/**
 * This struct is used to split xa_info passed from configuration file
 * (lixac_conf.xml) to broken down values that can be passed to
 * mysql_real_connect() function. The field of the structure are the same
 * of the parameter of mysql_real_connect(); the _buffer fields are used to
 * avoid useless dynamic allocation (xa_info is a length contrained string)
 */
struct lixa_mysql_real_connect_s {
    char         *host;
    char          host_buffer[MAXINFOSIZE];
    char         *user;
    char          user_buffer[MAXINFOSIZE];
    char         *passwd;
    char          passwd_buffer[MAXINFOSIZE];
    char         *db;
    char          db_buffer[MAXINFOSIZE];
    unsigned int  port;
    char         *unix_socket;
    char          unix_socket_buffer[MAXINFOSIZE];
    unsigned long client_flag;
};



/**
 * Char used to separe a key=value field from the following one
 */
#define LIXA_MYSQL_XA_INFO_SEPARATOR ','
/**
 * Char used to separe a key from a value
 */
#define LIXA_MYSQL_XA_INFO_ASSIGN    '='



/**
 * Parses an xa_info string and creates a broken down structure with the
 * fields necessary to invoke mysql_real_connect() function
 * @param xa_info IN the string passed to xa_open from lixac_conf.xml file
 * @param lmrc OUT the corresponding broken down structure
 * @return an XA reason code
 */
int lixa_my_parse_xa_info(const char *xa_info,
                          struct lixa_mysql_real_connect_s *lmrc);



/**
 * Parses a key/value couple and put into parameters structure
 * @param lmrc OUT the broken down (parameters) structure
 * @param key IN key string
 * @param value IN value string
 * @return an XA reason code
 */
int lixa_my_parse_key_value(struct lixa_mysql_real_connect_s *lmrc,
                            const char *key, const char *value);



/**
 * Serialize XID to a string compatible with MySQL XA commands
 * @param[in] xid to be serialized
 * @param[out] lmsx serialized XID
 * @return TRUE if serialization was completed, FALSE if there was an error
 */
int lixa_my_xid_serialize(const XID *xid, lixa_my_ser_xid_t lmsx);



/**
 * Deserialize the string fetched after XA RECOVER command and build a
 * XID standard object
 * @param[out] xid resulting XID (XA standard)
 * @param[in] formatID a string containing formatID
 * @param[in] gtrid_length a string containing the length of gtrid part
 * @param[in] bqual_length a string containing the length of bqual part
 * @param[in] data a string containing the concatenation of gtrid and bqual
 * @return TRUE if deserialization was completed, FALSE if there was an error
 */
int lixa_my_xid_deserialize(XID *xid, const char *formatID,
                            const char *gtrid_length, const char *bqual_length,
                            const char *data);



/**
 * Implementation of "xa_open" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_open(char *xa_info, int rmid, long flags);


    
/**
 * Implementation of "xa_close" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_close(char *xa_info, int rmid, long flags);


    
/**
 * Implementation of "xa_start" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_start(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_start" for MySQL: it's referenced by
 * @ref lixa_my_start (XA interface) and by @ref xta_mysql_xa_start
 */
int lixa_my_start_core(struct lixa_sw_status_rm_s *lpsr,
                       const XID *xid, int rmid, long flags);



/**
 * Implementation of "xa_end" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_end(const XID *xid, int rmid, long flags);


    
/**
 * Core implementation of "xa_end" for MySQL: it's referenced by
 * @ref lixa_my_end (XA interface) and by @ref xta_mysql_xa_end
 */
int lixa_my_end_core(struct lixa_sw_status_rm_s *lpsr,
                     const XID *xid, int rmid, long flags);



/**
 * Implementation of "xa_rollback" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_rollback(const XID *xid, int rmid, long flags);


    
/**
 * Core implementation of "xa_rollback" for MySQL: it's referenced by
 * @ref lixa_my_rollback (XA interface) and by @ref xta_mysql_xa_rollback
 */
int lixa_my_rollback_core(struct lixa_sw_status_rm_s *lpsr,
                          const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_prepare" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_prepare(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_prepare" for MySQL: it's referenced by
 * @ref lixa_my_prepare (XA interface) and by @ref xta_mysql_xa_prepare
 */
int lixa_my_prepare_core(struct lixa_sw_status_rm_s *lpsr,
                         const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_commit" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_commit(const XID *xid, int rmid, long flags);


    
/**
 * Core implementation of "xa_commit" for MySQL: it's referenced by
 * @ref lixa_my_commit (XA interface) and by @ref xta_mysql_xa_commit
 */
int lixa_my_commit_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_recover" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description.
 * Unfortunately TMSTARTRSCAN and TMENDRSCAN are not implemented as the
 * standard explains because an open cursor need an open block (transaction)
 * and inside an open block "ROLLBACK PREPARED" cannot be used. For the sake of
 * LIXA usage a single scan is good enought, but this is not the standard
 * behavior
 */
int lixa_my_recover(XID *xids, long count, int rmid, long flags);


    
/**
 * Core implementation of "xa_recover" for MySQL: it's referenced by
 * @ref lixa_my_recover (XA interface) and by @ref xta_mysql_xa_recover
 */
int lixa_my_recover_core(struct lixa_sw_status_rm_s *lpsr,
                         XID *xids, long count, int rmid, long flags);


    
/**
 * Implementation of "xa_forget" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_forget(const XID *xid, int rmid, long flags);


    
/**
 * Core implementation of "xa_forget" for MySQL: it's referenced by
 * @ref lixa_my_forget (XA interface) and by @ref xta_mysql_xa_forget
 */
int lixa_my_forget_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_complete" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_complete(int *handle, int *retval, int rmid, long flags);



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIBLIXAPQ_H */
