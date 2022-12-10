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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXAPQ_H
# define LIXAPQ_H



/* PostgreSQL front-end */
#include <libpq-fe.h>



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve the connection established by tx_open/xa_open to one of the
     * PostgreSQL Resource Managers; this function should be used instead of
     * @ref lixa_pq_get_conn when you are using more than one PostgreSQL
     * database.
     * @param rmid IN it can be 0, 1, 2, 3, ... in accordance to
     *                lixac_conf.xml (the position of the interested
     *                PostgreSQL resource manager inside the resource
     *                manager list of the current profile)
     * @return a valid connection handle or NULL if rmid is not a PostgreSQL
     *         Resource Manager or rmid is a valid PostgreSQL Resource
     *         Manager, but there is no a valid connection
     */
    PGconn *lixa_pq_get_conn_by_rmid(int rmid);


    
    /**
     * Retrieve the connection established by tx_open/xa_open to the
     * PostgreSQL Resource Manager; this is necessary because tx_open
     * does not return values and PQconnectdb returns a new connection
     * every time it's called from the same process/thread;
     * @param pos IN the position of the interested PostgreSQL resource manager
     *               inside PostgreSQL only resource manager list
     * @return a valid connection handle or NULL if the handle is not available
     */
    PGconn *lixa_pq_get_conn_by_pos(int pos);



    /**
     * Retrieve the connection established by tx_open/xa_open to the
     * PostgreSQL Resource Manager; this is necessary because tx_open
     * does not return values and PQconnectdb returns a new connection
     * every time it's called from the same process/thread; this is the
     * same of lixa_pq_get_conn_by_pos(0)
     * @return a valid connection handle or NULL if the handle is not available
     */
    PGconn *lixa_pq_get_conn(void);



    /**
     * Check if a PostgreSQL connection is managed by LIXA
     * @param conn IN pointer to a PostgreSQL Resource Manager connection
     * @return a boolean value: TRUE / FALSE
     */
    int lixa_pq_is_managed_conn(const PGconn *conn);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#ifdef HAVE_GNU_COBOL
/**
 * COBOL wrapper for @ref lixa_pq_get_conn
 * @return a valid connection handle or NULL if the handle is not available
 */
PGconn *LIXAPQGETCONN();



/**
 * COBOL wrapper for @ref lixa_pq_get_conn_by_rmid
 * @param rmid IN it can be 0, 1, 2, 3, ... in accordance to
 *                lixac_conf.xml (the position of the interested
 *                PostgreSQL resource manager inside the resource
 *                manager list of the current profile)
 * @return a valid connection handle or NULL if the handle is not available
 */
PGconn *LIXAPQGETCONNBYRMID(int rmid);



/**
 * COBOL wrapper for @ref lixa_pq_get_conn_by_pos
 * @param pos IN the position of the interested PostgreSQL resource manager
 *               inside PostgreSQL only resource manager list
 * @return a valid connection handle or NULL if the handle is not available
 */
PGconn *LIXAPQGETCONNBYPOS(int pos);
#endif /* HAVE_GNU_COBOL */



#endif /* LIXAPQ_H */
