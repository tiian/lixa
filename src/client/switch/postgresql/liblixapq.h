/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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
int lixa_pq_start(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_end" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_end(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_rollback" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_rollback(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_prepare" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_prepare(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_commit" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_commit(XID *xid, int rmid, long flags);


    
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
 * Implementation of "xa_forget" for PostgreSQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_pq_forget(XID *xid, int rmid, long flags);


    
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
