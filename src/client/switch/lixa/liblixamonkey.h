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
#ifndef TEMPLATE_H
# define TEMPLATE_H



#include <config.h>



#include <lixa_trace.h>
#include <lixa_common_status.h>
#include <xa.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_XA



/**
 * Switch structure for LIXA Monkey RM (static version)
 */
extern struct xa_switch_t lixa_monkeyrm_sta_sw;



/**
 * Switch structure for LIXA Monkey RM (dynamic version)
 */
extern struct xa_switch_t lixa_monkeyrm_dyn_sw;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * LIXA Monkey RM implementation of xa_open function
     */ 
    int lixa_monkeyrm_open(char *xa_info, int rmid, long flags);


    
    /**
     * LIXA Monkey RM implementation of xa_close function
     */
    int lixa_monkeyrm_close(char *xa_info, int rmid, long flags);


    
    /**
     * LIXA Monkey RM implementation of xa_start function
     */
    int lixa_monkeyrm_start(XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_end function
     */
    int lixa_monkeyrm_end(XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_rollback function
     */
    int lixa_monkeyrm_rollback(XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_prepare function
     */
    int lixa_monkeyrm_prepare(XID *xid, int rmid, long flags);


    
    /**
     * LIXA Monkey RM implementation of xa_commit function
     */
    int lixa_monkeyrm_commit(XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_recover function
     */
    int lixa_monkeyrm_recover(XID *xid, long count, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_forget function
     */
    int lixa_monkeyrm_forget(XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_complete function
     */
    int lixa_monkeyrm_complete(int *handle, int *retval, int rmid, long flags);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* TEMPLATE_H */
