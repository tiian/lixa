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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
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



#endif /* LIXAPQ_H */
