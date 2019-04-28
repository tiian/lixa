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
#ifndef LIBLIXAPQ_H
# define LIBLIXAPQ_H



#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_trace.h>
/* Utilities for resource manager without standard switch file */
#include <lixa_sw.h>
/* PostgreSQL front-end */
#include <libpq-fe.h>



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
struct xa_switch_t xapqls;



/**
 * Character separator used when serializing/deserializing a xid
 */
#define LIXA_PQ_XID_SEPARATOR '_'



/**
 * Length of a string that can contain a serialized XID for PostgreSQL:
 * formatID + '_' + base64(gtrid) + '_' + base64(bqual)
 */
#define LIXA_PQ_XID_SERIALIZE_LENGTH (LIXA_SERIALIZED_LONG_INT+1+XIDDATASIZE*2/3+4+1+XIDDATASIZE*2/3+4+1)



/**
 * A string used to serialize a XID for PostgreSQL.
 * NOTE: this is not XA standard compliant, but it the same used by the
 * PostgreSQL JDBC driver.
 */
typedef char lixa_pq_ser_xid_t[LIXA_PQ_XID_SERIALIZE_LENGTH];



/**
 * Serialize XID to a string with the same schema used by
 * https://jdbc.postgresql.org/
 * See method xidToString in https://github.com/pgjdbc/pgjdbc/blob/master/pgjdbc/src/main/java/org/postgresql/xa/RecoveredXid.java
 * @param[in] xid the XID to be serialized
 * @param[out] lpsx the serialized XID
 * @return TRUE if serialization was completed, FALSE if there was an error
 */
int lixa_pq_xid_serialize(const XID *xid, lixa_pq_ser_xid_t lpsx);


    
/**
 * Deserialize a string compatible with PostgreSQL JDBC driver to a XID
 * See method stringToXid in https://github.com/pgjdbc/pgjdbc/blob/master/pgjdbc/src/main/java/org/postgresql/xa/RecoveredXid.java
 * @param[out] xid the deserialized XID
 * @param[in] lpsx the string must be deserialized
 * @return TRUE if deserialization was completed, <br>
 *         FALSE if there was an error
 */
int lixa_pq_xid_deserialize(XID *xid, const lixa_pq_ser_xid_t lpsx);



/**
 * Implementation of "xa_open" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_open(char *xa_info, int rmid, long flags);


    
/**
 * Implementation of "xa_close" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_close(char *xa_info, int rmid, long flags);


    
/**
 * Implementation of "xa_start" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_start(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_start" for PostgreSQL: it's referenced by
 * @ref lixa_pq_start (XA interface) and by @ref xta_postgresql_xa_start
 */
int lixa_pq_start_core(struct lixa_sw_status_rm_s *lpsr,
                       const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_end" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_end(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_end" for PostgreSQL: it's referenced by
 * @ref lixa_pq_end (XA interface) and by @ref xta_postgresql_xa_end
 */
int lixa_pq_end_core(struct lixa_sw_status_rm_s *lpsr,
                     const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_rollback" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_rollback(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_rollback" for PostgreSQL: it's referenced by
 * @ref lixa_pq_rollback (XA interface) and by @ref xta_postgresql_xa_rollback
 */
int lixa_pq_rollback_core(struct lixa_sw_status_rm_s *lpsr,
                          const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_prepare" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_prepare(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_prepare" for PostgreSQL: it's referenced by
 * @ref lixa_pq_prepare (XA interface) and by @ref xta_postgresql_xa_prepare
 */
int lixa_pq_prepare_core(struct lixa_sw_status_rm_s *lpsr,
                         const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_commit" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_commit(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_commit" for PostgreSQL: it's referenced by
 * @ref lixa_pq_commit (XA interface) and by @ref xta_postgresql_xa_commit
 */
int lixa_pq_commit_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_recover" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description.
 * Unfortunately TMSTARTRSCAN and TMENDRSCAN are not implemented as the
 * standard explains because an open cursor need an open block (transaction)
 * and inside an open block "ROLLBACK PREPARED" cannot be used. For the sake of
 * LIXA usage a single scan is good enought, but this is not the standard
 * behavior
 */
int lixa_pq_recover(XID *xids, long count, int rmid, long flags);



/**
 * Core implementation of "xa_recover" for PostgreSQL: it's referenced by
 * @ref lixa_pq_recover (XA interface) and by @ref xta_postgresql_xa_recover
 */
int lixa_pq_recover_core(struct lixa_sw_status_rm_s *lpsr,
                         XID *xids, long count, int rmid, long flags);


    
/**
 * Implementation of "xa_forget" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_forget(const XID *xid, int rmid, long flags);



/**
 * Core implementation of "xa_forget" for PostgreSQL: it's referenced by
 * @ref lixa_pq_forget (XA interface) and by @ref xta_postgresql_xa_forget
 */
int lixa_pq_forget_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_complete" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_complete(int *handle, int *retval, int rmid, long flags);



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIBLIXAPQ_H */
