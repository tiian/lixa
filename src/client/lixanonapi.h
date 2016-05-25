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
     * Parse a connection string expressed with syntax <BR>
     * lixa/[pos|rmid]/[id] <BR>
     * and retrieve the values
     * @param conn_string IN the string to be parsed
     * @param use_lixa_conn OUT boolean: the parsed string matches and
     *                          a LIXA connection must be used
     * @param use_lixa_rmid OUT boolean: meaningful only if
     *                          use_lixa_conn == TRUE; <BR>
     *                          use_lixa_rmid == TRUE => id is an rmid; <BR>
     *                          use_lixa_rmid == FALSE => id is a pos;
     * @param use_lixa_id OUT integer: meaningful only if
     *                        use_lixa_conn == TRUE; <BR>
     *                        it specifies the value to be passed to
     *                        @ref lixa_my_get_conn_by_pos,
     *                        @ref lixa_my_get_conn_by_rmid,
     *                        @ref lixa_pq_get_conn_by_pos,
     *                        @ref lixa_pq_get_conn_by_rmid
     * @return 0 if parsing was OK, <BR>
     *         !0 if an error happened (the error is
     *           encoded as a LIXA standard return code)
     */
    int lixa_nonapi_parse_conn_string(const char *conn_string,
                                      int *use_lixa_conn,
                                      int *use_lixa_rmid,
                                      int *use_lixa_id);



#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXANONAPI_H */
