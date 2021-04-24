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
     * Parse a connection string expressed with syntax <BR>
     * lixa/[pos|rmid]/[id] <BR>
     * and retrieve the values
     * @param[in] conn_string the string to be parsed
     * @param[out] use_lixa_conn boolean: the parsed string matches and
     *                          a LIXA connection must be used
     * @param[out] use_lixa_rmid boolean: meaningful only if
     *                          use_lixa_conn == TRUE; <BR>
     *                          use_lixa_rmid == TRUE => id is an rmid; <BR>
     *                          use_lixa_rmid == FALSE => id is a pos;
     * @param[out] use_lixa_id integer: meaningful only if
     *                        use_lixa_conn == TRUE; <BR>
     *                        it specifies the value to be passed to
     *                        lixa_my_get_conn_by_pos,
     *                        lixa_my_get_conn_by_rmid,
     *                        lixa_pq_get_conn_by_pos,
     *                        lixa_pq_get_conn_by_rmid
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
