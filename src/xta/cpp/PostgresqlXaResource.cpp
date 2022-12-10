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



/* include XTA header file */
#include "xta.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



#include "Exception.hpp"
#include "PostgresqlXaResource.hpp"



namespace xta {
    PostgresqlXaResource::PostgresqlXaResource(
        PGconn *connection, std::string const& name,
        std::string const& open_info)
    {
        if (NULL == (pxar = xta_postgresql_xa_resource_new(
                         connection, name.c_str(), open_info.c_str())))
            throw Exception(LIXA_RC_NULL_OBJECT,
                            "xta_postgresql_xa_resource_new");
    };
    PostgresqlXaResource::~PostgresqlXaResource()
    {
        xta_postgresql_xa_resource_delete(pxar);
        pxar = NULL;
    };
    
}
