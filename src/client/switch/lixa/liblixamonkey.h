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
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_XA_SWITCH



/**
 * Switch structure for LIXA Monkey RM (static version)
 */
extern struct xa_switch_t lixa_monkeyrm_sta_sw;



/**
 * Switch structure for LIXA Monkey RM (dynamic version)
 */
extern struct xa_switch_t lixa_monkeyrm_dyn_sw;



/**
 * Enumeration used to assing a numerical id to any XA verb
 */
enum monkey_status_verb_e { XA_OPEN = 1,
                            XA_CLOSE,
                            XA_START,
                            XA_END,
                            XA_PREPARE,
                            XA_COMMIT,
                            XA_ROLLBACK,
                            XA_RECOVER,
                            XA_FORGET,
                            XA_COMPLETE };



/**
 * This struct is the base record of @ref monkey_status_s struct
 */
struct monkey_status_record_s {
    /**
     * Monkey resource manager is waiting for this verb
     */
    enum monkey_status_verb_e   verb;
    /**
     * Monkey resource manager will reply this return code
     */
    int                         rc;
};



/**
 * This struct contains all the pertinent status data related to a resource
 * manager instance (thread_id/rmid)
 */
struct monkey_status_s {
    /**
     * next record is waited by the resource manager
     */
    guint   next_record;
    /**
     * array of records of type @ref monkey_status_record_s
     */
    GArray *records;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Comparison function for @ref monkey_status first & second level hast
     * table
     */
    gboolean monkey_status_gequal(gconstpointer a, gconstpointer b);



    /**
     * Value destroy function for @ref monkey_status first level hash table
     */
    void monkey_status_destroy1(gpointer data);



    /**
     * Value destroy function for @ref monkey_status second level hash table
     */
    void monkey_status_destroy2(gpointer data);



    /**
     * Check verb, return the rc has been configured for this step
     * @param mss IN reference to the monkey status struct
     * @param verb IN the verb must be checked
     * @param rc OUT the return code must be returned to the transaction
     *           manager
     * @return a standardized reason code
     */
    int lixa_monkeyrm_get_rc(struct monkey_status_s *mss, 
                             enum monkey_status_verb_e verb, int *rc);


    
    /**
     * LIXA Monkey RM implementation of xa_open function
     */ 
    int lixa_monkeyrm_open(char *xa_info, int rmid, long flags);


    
    /**
     * LIXA Monkey RM implementation of xa_open function
     * @param xa_info IN same as in xa_open
     * @param rmid IN same as in xa_open
     * @param flags IN same as in xa_open
     * @param mss IN/OUT points to the block stores the status of the
     *                   resource manager
     * @return a standardized return code
     */ 
    int lixa_monkeyrm_open_init(char *xa_info, int rmid, long flags,
                                struct monkey_status_s *mss);


    
    /**
     * LIXA Monkey RM implementation of xa_close function
     */
    int lixa_monkeyrm_close(char *xa_info, int rmid, long flags);


    
    /**
     * LIXA Monkey RM implementation of xa_start function
     */
    int lixa_monkeyrm_start(const XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_end function
     */
    int lixa_monkeyrm_end(const XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_rollback function
     */
    int lixa_monkeyrm_rollback(const XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_prepare function
     */
    int lixa_monkeyrm_prepare(const XID *xid, int rmid, long flags);


    
    /**
     * LIXA Monkey RM implementation of xa_commit function
     */
    int lixa_monkeyrm_commit(const XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_recover function
     */
    int lixa_monkeyrm_recover(XID *xids, long count, int rmid,
                              long flags);



    /**
     * LIXA Monkey RM implementation of xa_forget function
     */
    int lixa_monkeyrm_forget(const XID *xid, int rmid, long flags);



    /**
     * LIXA Monkey RM implementation of xa_complete function
     */
    int lixa_monkeyrm_complete(int *handle, int *retval, int rmid, long flags);

    

    /**
     * This function is used to simulate a call-back from the resource manager
     * library when dynamic registration is used
     * @param rmid IN resource manager id will be passed to ax_reg
     * @return the value returned from ax_reg
     */
    int lixa_monkeyrm_call_ax_reg(int rmid);



    /**
     * This function is used to simulate a call-back from the resource manager
     * library when dynamic registration is used
     * @param rmid IN resource manager id will be passed to ax_unreg
     * @return the value returned from ax_unreg
     */
    int lixa_monkeyrm_call_ax_unreg(int rmid);



    /**
     * This function is used to release the dynamic memory allocated by
     * monkey resource manager and its useful for memory leak detection only
     */
    void lixa_monkeyrm_call_cleanup(void);


    
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
