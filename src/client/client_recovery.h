/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef CLIENT_RECOVERY_H
# define CLIENT_RECOVERY_H



#include <config.h>



#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include "client_status.h"
#include "tx.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_RECOVERY



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Perform the revovery action: the server (lixad) is
     * polled to retrieve the transactions need to be recovered
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] client reference to the client piece of info sent with
     *               @ref LIXA_MSG_VERB_OPEN message
     * @return a reason code     
     */
    int client_recovery(client_config_coll_t *ccc, client_status_t *cs,
                        const struct lixa_msg_body_open_8_client_s *client);
    


    /**
     * Analyze the data received from the server
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] rpl information received from the server
     * @param[out] commit boolean value: TRUE = perform commit,
     *                                   FALSE = perform rollback
     * @return a reason code
     */
    int client_recovery_analyze(client_config_coll_t *ccc,
                                const client_status_t *cs,
                                struct lixa_msg_s *rpl, int *commit);


    
    /**
     * Commit the transaction received from the server
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] rpl information received from the server
     * @param[in] updt information must be sent to the server (recovery
     *                update info)
     * @return a reason code
     */
    int client_recovery_commit(client_config_coll_t *ccc,
                               const client_status_t *cs,
                               struct lixa_msg_s *rpl,
                               struct lixa_msg_s *updt);


    
    /**
     * Rollback the transaction received from the server
     * @param[in] ccc client config collection
     * @param[in] cs reference to the status of the calling client
     * @param[in] rpl information received from the server
     * @param[in] updt information must be sent to the server (recovery
     *                update info)
     * @return a reason code
     */
    int client_recovery_rollback(client_config_coll_t *ccc,
                                 const client_status_t *cs,
                                 struct lixa_msg_s *rpl,
                                 struct lixa_msg_s *updt);



    /**
     * Scan all the resource manager to find out the prepared and in-doubt
     * transactions: this should be performed only AFTER the normal recovery
     * phase
     * @param cs IN reference to the status
     * @param crt OUT reference to the cold recovery table
     * @param bbqc IN bypass the brach qualifier check (TRUE/FALSE)
     * @param bfic IN bypass the format id check (TRUE/FALSE)
     * @param utf IN use TMENDRSCAN flag for last xa_recover call (TRUE/FALSE)
     * @return a standardized reason code
     */
    int client_recovery_scan(const client_status_t *cs, GTree *crt,
                             int bbqc, int bfic, int utf);



    /**
     * Report the content of a cold recovery table
     * @param cs IN reference to the status
     * @param crt IN reference to the cold recovery table
     * @return a standardized reason code
     */
    int client_recovery_report(const client_status_t *cs, GTree *crt);


    
    /**
     * Used to traverse the tree (cold recovery table) and report every
     * node of the tree
     * @param key IN xid object
     * @param value IN array of resource manager ids
     * @param data IN output stream
     */
    gboolean client_recovery_report_foreach(gpointer key, gpointer value,
                                            gpointer data);


    
    /**
     * This is a wrapper for @ref lixa_xid_compare
     */
    int clnt_rcvr_xid_compare(gconstpointer a, gconstpointer b, gpointer foo);



    /**
     * This is a wrapper for g_array_free(*, TRUE)
     */
    void clnt_rcvr_array_free(gpointer data);

    

    /**
     * Cold commit a transaction
     * @param cs IN reference to the status of the calling client
     * @param xid IN transaction to commit
     * @param rsrmgrs IN the array of resource managers should be committed
     * @return a reason code
     */
    int client_recovery_cold_commit(const client_status_t *cs,
                                    XID *xid,
                                    const GArray *rsrmgrs);


    
    /**
     * Cold rollback a transaction
     * @param cs IN reference to the status of the calling client
     * @param xid IN transaction to commit
     * @param rsrmgrs IN the array of resource managers should be rolled back
     * @return a reason code
     */
    int client_recovery_cold_rollback(const client_status_t *cs,
                                      XID *xid,
                                      const GArray *rsrmgrs);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* CLIENT_RECOVERY_H */
