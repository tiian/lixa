/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
 * Server Transaction Table Record is the record used by
 * @ref server_trans_tbl_s to index state file content
 */
struct server_trans_tbl_rec_s {
    /**
     * Global TRansaction ID is used as key
     */
    char *gtrid;
    /**
     * Thread State IDentifier
     */
    guint tsid;
    /**
     * Transaction ID, XID
     */
    lixa_ser_xid_t xid;
    /**
     * Relevant block inside the state file
     */
    uint32_t block_id;
};



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
    /**
     * size of thread status identifier array
     */
    guint     tsid_array_size;
};



typedef struct server_trans_tbl_s server_trans_tbl_t;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    int server_trans_tbl_init(server_trans_tbl_t *stt, guint tsid_array_size);

    int server_trans_tbl_comp(gconstpointer a, gconstpointer b,
                              gpointer user_data);

    void server_trans_tbl_value_destroy(gpointer data);

    int server_trans_tbl_insert(server_trans_tbl_t *stt,
                                const struct server_trans_tbl_rec_s *sttr);

    int server_trans_tbl_remove(server_trans_tbl_t *stt,
                                const struct server_trans_tbl_rec_s *sttr);

    int server_trans_tbl_clear(server_trans_tbl_t *stt);


    
    /**
     * Query the Server Transaction Table and returns an array with all the
     * records with a specified Global TRansaction ID
     * @param[in] stt Server Transaction Table
     * @param[in] sttr Server Tramsactopm Table Record used for the query
     * @param[in,out] result GArray with all the matching records
     * @param[in] maint boolean value, TRUE for maintenance mode
     * @return a reason code
     */
    int server_trans_tbl_query_xid(server_trans_tbl_t *stt,
                                   const struct server_trans_tbl_rec_s *sttr,
                                   GArray *result, int maint);


    
    gboolean server_trans_tbl_traverse(gpointer key, gpointer value,
                                       gpointer data);


    
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
