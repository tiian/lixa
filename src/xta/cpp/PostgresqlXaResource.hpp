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
#ifndef POSTGRESQL_XA_RESOURCE_HPP
# define POSTGRESQL_XA_RESOURCE_HPP



#include <string>



/* include XTA header file */
#include "xta.h"
#include "AcquiredXaResource.hpp"



namespace xta {
    /**
     * XTA PostgreSQL XA Resource class
     */
    class PostgresqlXaResource : public AcquiredXaResource {
        public:
        /**
         * Create a new PostgreSQL resource (C++ style)
         * @param[in,out] connection to PostgreSQL already opened by the
         *                application program
         * @param[in] name : unique identifier of the resource
         * @param[in] open_info : unique description of the connection
         *                        properties like network name/IP address,
         *                        port, user/schema, etc.
         */
        PostgresqlXaResource(PGconn *connection, std::string const& name,
                             std::string const& open_info);
        ~PostgresqlXaResource();
        /**
         * Return a pointer to the C base XA resource object
         */
        xta_xa_resource_t *getCBaseXaResource() {
            return (xta_xa_resource_t *)pxar; }
        
        private:
        /**
         * Pointer to the native C object
         */
        xta_postgresql_xa_resource_t *pxar;
    };
};



#endif /* POSTGRESQL_XA_RESOURCE_HPP */
