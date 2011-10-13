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
#ifndef LIBLIXAPQ_H
# define LIBLIXAPQ_H



#include <config.h>



#ifdef HAVE_STRING_H
# include <string.h>
#endif



/* Utilities for resource manager without standard switch file */
#include <lixa_sw.h>
/* MySQL front-end */
#include <mysql.h>



/**
 * This struct contains the pointers to XA methods and properties
 */
struct xa_switch_t xapqls;



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
int lixa_my_start(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_end" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_end(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_rollback" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_rollback(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_prepare" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_prepare(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_commit" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_commit(XID *xid, int rmid, long flags);


    
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
 * Implementation of "xa_forget" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_forget(XID *xid, int rmid, long flags);


    
/**
 * Implementation of "xa_complete" for MySQL;
 * refer to "Distributed Transaction Processing: The XA Specification" for
 * a complete description
 */
int lixa_my_complete(int *handle, int *retval, int rmid, long flags);



#endif /* LIBLIXAPQ_H */
