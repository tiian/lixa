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
#ifndef LIXAMY_H
# define LIXAMY_H



/* 
  MySQL front-end: does not include it if included by PHP mysqli extension
  compiled with Native Driver (ND) feature
*/
#ifndef MYSQLI_USE_MYSQLND
# include <mysql.h>
#endif



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve the connection established by tx_open/xa_open to one of the
     * MySQL Resource Managers; this function should be used instead of
     * @ref lixa_pq_get_conn when you are using more than one MySQL
     * database.
     * @param rmid IN it can be 0, 1, 2, 3, ... in accordance to
     *                lixac_conf.xml (the position of the interested
     *                MySQL resource manager inside the resource
     *                manager list of the current profile)
     * @return a valid connection handle or NULL if rmid is not a MySQL
     *         Resource Manager or rmid is a valid MySQL Resource
     *         Manager, but there is no a valid connection
     */
    MYSQL *lixa_my_get_conn_by_rmid(int rmid);


    
    /**
     * Retrieve the i-th connection established by tx_open/xa_open to the
     * MySQL Resource Manager; this is necessary because tx_open
     * does not return values and mysql_real_connect returns a new connection
     * every time it's called from the same process/thread;
     * @param pos IN the position of the interested MySQL resource manager
     *               inside MySQL only resource manager list
     * @return a valid connection handle or NULL if the handle is not available
     */
    MYSQL *lixa_my_get_conn_by_pos(int pos);



    /**
     * Retrieve the connection established by tx_open/xa_open to the
     * MySQL Resource Manager; this is necessary because tx_open
     * does not return values and mysql_real_connect returns a new connection
     * every time it's called from the same process/thread; this is the
     * same of lixa_my_get_conn_by_pos(0)
     * @return a valid connection handle or NULL if the handle is not available
     */
    MYSQL *lixa_my_get_conn(void);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXAMY_H */
