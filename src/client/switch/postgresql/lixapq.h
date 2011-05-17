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
#ifndef LIXAPQ_H
# define LIXAPQ_H



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve the connection established by tx_open/xa_open to one of the
     * PostgreSQL Resource Managers; this function should be used instead of
     * @ref lixa_pq_get_conn when you are using more than one PostgreSQL
     * database.
     * @param rmid IN it can be 0, 1, 2, 3, ... in accordance to
     *                lixac_conf.xml
     * @return a valid connection handle or NULL if rmid is not a PostgreSQL
     *         Resource Manager or rmid is a valid PostgreSQL Resource
     *         Manager, but there is no a valid connection
     */
    PGconn *lixa_pq_get_conn_by_rmid(int rmid);


    
    /**
     * Retrieve the connection established by tx_open/xa_open to one of the
     * PostgreSQL Resource Managers; this function should be used instead of
     * @ref lixa_pq_get_conn when you are using more than one PostgreSQL
     * database.
     * @param pos IN it can be 0, 1, 2, 3, ... and it's the position in the
     *                list of PostgreSQL Resource Managers (the first is
     *                in position 0, the second is in position 1 and so on
     * @return a valid connection handle or NULL if rmid is not a PostgreSQL
     *         Resource Manager or rmid is a valid PostgreSQL Resource
     *         Manager, but there is no a valid connection
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
    static inline PGconn *lixa_pq_get_conn(void) {
        return lixa_pq_get_conn_by_pos(0);
    }



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXAPQ_H */
