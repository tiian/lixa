/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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



/*
 * This header contains the prototypes of useful functions that can be used
 * for some special purposes such language extension wrappers and so on.
 * The functions declared in this prototypes are NOT intended for
 * Application Program developers, but only for software system integrators
 */
#ifndef LIXANONAPI_H
# define LIXANONAPI_H



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Parse a URI related to a LIXA state server expressed as <BR>
     * tcp://address:port/name <BR>
     * and retrieve the values associated to address, port and name.
     * NOTE: all the returned pointers are inside the passed string
     * @param[in] sttsrv_string contains the URI of the LIXA state server
     * @param[out] address IP or DNS mapped
     * @param[out] port
     * @param[out] name associated to the state server
     * @return a reason code
     */
    int lixa_nonapi_parse_uri_sttsrv(const char *sttsrv_string,
                                     const char **address,
                                     const char **port,
                                     const char **name);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXANONAPI_H */
