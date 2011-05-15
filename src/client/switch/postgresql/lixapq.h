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
     * Retrieve the connection established by tx_open/xa_open to the
     * PostgreSQL Resource Manager; this is necessary because tx_open
     * does not return values and PQconnectdb returns a new connection
     * every time it's called from the same process/thread
     * @return a valid connection handle or NULL if the handle is not available
     */
    PGconn *lixa_pq_get_conn(void);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXAPQ_H */
