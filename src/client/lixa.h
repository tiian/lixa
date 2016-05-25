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
#ifndef LIXA_H
# define LIXA_H



/*
 * This header specify the LIXA extensions to TX interface.
 *
 * NOTE: these functions are NOT standard: they are LIXA specific.
 */



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * This function is useful to inspect if the current build is integrated
     * with IBM DB2 database manager
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_ibmdb2(void);

    

    /**
     * This function is useful to inspect if the current build is integrated
     * with MySQL database manager
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_mysql(void);

    

    /**
     * This function is useful to inspect if the current build is integrated
     * with PostgreSQL database manager
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_postgresql(void);

    

    /**
     * This function is useful to inspect if the current build is integrated
     * with Oracle database manager
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_oracle(void);

    

    /**
     * This function is useful to inspect if the current build is integrated
     * with WebSphere MQ queue manager MOM (Message Oriented Middleware)
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_webspheremq(void);

    

    /**
     * This function is useful to inspect if the current build is integrated
     * with WebSphere MQ queue manager MOM (Message Oriented Middleware) and
     * the switch file are built for Extended Transactional Cliend mode
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_webspheremq_etc(void);

    

    /**
     * This function is useful to inspect if the current build is integrated
     * with WebSphere MQ queue manager MOM (Message Oriented Middleware) and
     * the switch file are build for server bind mode
     * @return a boolean value: TRUE or FALSE
     */
    int lixa_config_have_webspheremq_srv(void);

    

#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* LIXA_H */
