/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_SERVER_TRANS_TBL_H
# define LIXA_SERVER_TRANS_TBL_H



#include "config.h"



#ifdef HAVE_GLIB_H
# include <glib.h>
# include <glibconfig.h>
#endif



#include "lixa_trace.h"
#include "lixa_xid.h"
#include "xa.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#else
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_SERVER_TPM



/**
 * Server Transaction Table Query is the record used to query and pass data
 * to @ref server_trans_tbl_s functions
 */
struct server_trans_tbl_qry_s {
    /**
     * Global TRansaction ID is used as key
     */
    char          *gtrid;
    /**
     * Thread State IDentifier
     */
    int            tsid;
    /**
     * Position of the header record of the branch inside the state file
     */
    uint32_t       slot_id;
    /**
     * Transaction ID, XID
     */
    lixa_ser_xid_t xid;
};



/**
 * An array of @ref server_trans_tbl_qry_s is used as the result of queries
 * on @ref server_trans_tbl_s
 */
typedef GArray server_trans_tbl_qry_arr_t;



/**
 * Server Transaction Table is an index of the state file content: it keeps
 * track of the currently managed transactions
 */
struct server_trans_tbl_s {
    /**
     * This structure is used by all state thread and the access to it must be
     * serialized by a mutex
     */
    GMutex    mutex;
    /**
     * Multidimentional tree with gtrid as key
     */
    GTree    *records;
};



/**
 * Record for the first level of the Server Transaction Table: every leaf of
 * three uses this type of record
 */
struct server_trans_tbl_rec1_s {
    /**
     * Identifier of the status thread that manages all the branches of this
     * global transaction id
     */
    int          tsid;
    /**
     * Array that contains all the branches related to the same global
     * transaction id. Every element of the GPtrArray is of type
     * @ref server_trans_tbl_rec2_s
     */
    GPtrArray   *branches;
};



/**
 * Record for the second level of the Server Transaction Table: every array of
 * branches uses this element.
 */
struct server_trans_tbl_rec2_s {
    /**
     * Position of the header record inside the state file
     */
    uint32_t       slot_id;
    /**
     * Serialized XID of the branch
     */
    lixa_ser_xid_t xid;
};



typedef struct server_trans_tbl_s server_trans_tbl_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    

    /**
     * Initialize a Server Transaction Table object
     * @param[in,out] stt reference to the object that must be initialized
     * @param[in] tsid_array_size is the number of state threads and should be
     *            no more useful because all the branches are managed by a
     *            single state thread
     * @return a reason code
     */
    int server_trans_tbl_init(server_trans_tbl_t *stt, guint tsid_array_size);



    /**
     * Compare two global transaction identifiers passed like serialized ASCII
     * C strings
     * @param[in] a first global transaction identifier
     * @param[in] b second global transaction identifier
     * @param[in] user_data UNUSED
     * @return comparison values -1, 0, +1
     */
    int server_trans_tbl_comp(gconstpointer a, gconstpointer b,
                              gpointer user_data);



    /**
     * Helper function used for data clean-up and associated to the g_tree
     * that store all the keys (global transaction ids)
     * @param[in] data represents the value field associated to the key
     */
    void server_trans_tbl_rec1_destroy(gpointer data);



    /**
     * Insert a new record in the Server Transaction Table
     * @param[in,out] stt reference to the Server Transaction Table
     * @param[in] sttq reference to the record that must be inserted
     * @return a reason code
     */
    int server_trans_tbl_insert(server_trans_tbl_t *stt,
                                const struct server_trans_tbl_qry_s *sttq);


    
    /**
     * Remove a record from the Server Transaction Table
     * @param[in,out] stt reference to the Server Transaction Table
     * @param[in] sttq reference to the record that must be removed
     * @return a reason code
     */
    int server_trans_tbl_remove(server_trans_tbl_t *stt,
                                const struct server_trans_tbl_qry_s *sttq);



    /**
     * Delete and release all the memory of a Server Transaction Table
     * @param[in,out] stt reference to the Server Transaction Table
     * @return a reason code
     */
    int server_trans_tbl_clear(server_trans_tbl_t *stt);


    
    /**
     * Query the Server Transaction Table and returns an array with all the
     * records with a specified Global TRansaction ID
     * @param[in] stt Server Transaction Table
     * @param[in] sttq Server Tramsactopm Table Query used for the query
     * @param[in,out] result with all the matching records
     * @param[in] maint boolean value, TRUE for maintenance mode
     * @return a reason code
     */
    int server_trans_tbl_query_xid(server_trans_tbl_t *stt,
                                   const struct server_trans_tbl_qry_s *sttq,
                                   server_trans_tbl_qry_arr_t *result,
                                   int maint);



    /**
     * Traverse the three and collect all the branches
     */
    gboolean server_trans_tbl_traverse(gpointer key, gpointer value,
                                       gpointer data);



    /**
     * Create a new array to store query results
     * @return a new object or NULL in the event of error
     */
    server_trans_tbl_qry_arr_t *server_trans_tbl_qry_arr_new(void);



    /**
     * Delete an array that stores query results
     * @param[in,out] sttqa is the array to delete
     */
    void server_trans_tbl_qry_arr_delete(server_trans_tbl_qry_arr_t *sttqa);



    /**
     * This function implements a GDestroyNotify associated to
     * @ref server_trans_tbl_qry_s
     */
    void server_trans_tbl_qry_destroy(gpointer data);

    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */

/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif //LIXA_SERVER_TRANS_TBL_H
