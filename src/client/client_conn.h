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
#ifndef CLIENT_CONN_H
#define CLIENT_CONN_H



#include <config.h>



#include <client_status.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_CONN



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Connect this client to the transaction manager
     * @param csc OUT object containing the status of this client
     * @param ccc OUT object containing the configuration of this client
     * @return a standardized return code
     */
    int client_connect(client_status_coll_t *csc,
                       client_config_coll_t *ccc);

    

    /**
     * Disconnect this client from the transaction manager
     * @param csc OUT object containing the status of this client
     * @return a standardized return code
     */     
    int client_disconnect(client_status_coll_t *csc);

    

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* CLIENT_CONN_H */
