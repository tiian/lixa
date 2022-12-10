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

/*
 * This file contains utilities used by LIXA provided switch files (MySQL,
 * PostgreSQL, ...)
 */

#ifndef LIXA_SW_H
# define LIXA_SW_H



#include <glib.h>
#include <string.h>
/* XA standard header */
#include <xa.h>



/**
 * This mutex is used to protect the status structure when the library is
 * used in a multithread environment
 */
extern GMutex lixa_sw_status_mutex;
/**
 * The status is saved in a hash table: there is an element for every
 * thread
 */
extern GHashTable  *lixa_sw_status;



/**
 * Default, null resource manager type
 */
#define LIXA_SW_STATUS_RM_TYPE_NULL        0
/**
 * Resource manager type associated to PostgreSQL
 */
#define LIXA_SW_STATUS_RM_TYPE_POSTGRESQL  1
/**
 * Resource manager type associated to MySQL
 */
#define LIXA_SW_STATUS_RM_TYPE_MYSQL       2



/**
 * Used to build an array of connections; useful when there are two or more
 * identical resource managers in the same transaction
 */
struct lixa_sw_status_rm_s {
    /** Resource Manager ID */
    int      rmid;
    /** Resource Manager type: it's used to distinguish the records in the
     * list */
    int      rm_type;
    /** Resource Manager state */
    struct {
        /** Resource Manager State:
         *  0=Un-initialized,
         *  1=Initialized */
        int R;
        /** Transaction Branch Association State:
         *  0=Not Associated,
         *  1=Associated,
         *  2=Association Suspended (UNSUPPORTED) */
        int T;
        /** Transaction Branch State:
         *  0=Non-existent Transaction,
         *  1=Active,
         *  2=Idle,
         *  3=Prepared,
         *  4=Rollback Only,
         *  5=Heuristically Completed */
        int S;
    } state;
    /** Transaction ID as passed by the Transaction Manager */
    XID      xid;
    /** Generic connection pointer */
    gpointer conn;
};



/**
 * This is the status record associated to a thread
 */
struct lixa_sw_status_s {
    /** Store the context of every generic resource manager used by the
     *  transaction */
    GArray *rm;
};



typedef struct lixa_sw_status_s lixa_sw_status_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Reset the content of an object
     * @param lps IN/OUT object reference
     */
    static inline void lixa_sw_status_init(lixa_sw_status_t *lps) {
        lps->rm = g_array_new(FALSE, FALSE, sizeof(struct lixa_sw_status_rm_s));
    }


    /**
     * Reset the content of an object
     * @param lpsr IN/OUT object reference
     */
    static inline void lixa_sw_status_rm_init(struct lixa_sw_status_rm_s *lpsr)
    {
        lpsr->rmid = 0;
        lpsr->rmid = LIXA_SW_STATUS_RM_TYPE_NULL;
        lpsr->state.R = lpsr->state.T = lpsr->state.S = 0;
        memset(&(lpsr->xid), 0, sizeof(XID));
        lpsr->conn = NULL;
    }



    /**
     * Retrieve a pointer to the status of the resource manager for the current
     * thread
     * @param rmid IN Resource Manager ID
     * @return the pointer to the desired status or NULL
     */
    struct lixa_sw_status_rm_s *lixa_sw_status_rm_get(int rmid);



    /**
     * Destroy an object of type @ref lixa_sw_status_t
     * @param data IN reference to the object
     */
    void lixa_sw_status_destroy(gpointer data);



    /**
     * Retrieve the connection pointer of a generic resource manager
     * @param rmid IN Resource Manager ID
     * @param rm_type IN Resource Manager type (it's used to avoid the
     *                   application program could specify an rmid of
     *                   a different resource manager type)
     * @return the pointer to the desired status or NULL
     */
    gpointer lixa_sw_get_conn_by_rmid(int rmid, int rm_type);



    /**
     * Retrieve the i-th connection pointer of a specified resource manager type
     * @param pos IN Resource Manager position
     * @param rm_type IN Resource Manager type
     * @return the pointer to the desired status or NULL
     */
    gpointer lixa_sw_get_conn_by_pos(int pos, int rm_type);



    /**
     * Check if a connection is managed by LIXA
     * @param conn IN pointer to a Resource Manager connection
     * @param rm_type IN Resource Manager type
     * @return a boolean value: TRUE / FALSE
     */
    int lixa_sw_is_managed_conn(const gpointer conn, int rm_type);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_SW_H */
