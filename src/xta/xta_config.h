/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef XTA_CONFIG_H
# define XTA_CONFIG_H



/**
 * This typedef is necessary to avoid the inclusion of LIXA internals that
 * are unnecessary for the XTA interface, but necessary for XTA implementation.
 * The real type is client_config_coll_t, by XTA is used only as a
 * pointer
 */
typedef void xta_config_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    

    /**
     * Retrieve connection timeout
     * @param[in] config object
     * @return the value of connection timeout
     */
    int xta_config_get_connection_timeout(const xta_config_t *config);


    
    /**
     * Set a new connection timeout
     * @param[in,out] config object
     * @param[in] value will be the new connection timeout
     * @return a reason code
     */
    int xta_config_set_connection_timeout(xta_config_t *config, int value);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */






#endif /* XTA_CONFIG_H */
