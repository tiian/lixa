/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "NativeXaResource.hpp"



namespace xta {
    NativeXaResource::NativeXaResource(std::string const& name,
                                       std::string const& switch_file,
                                       std::string const& open_info,
                                       std::string const& close_info)
    {
        if (NULL == (nxar = xta_native_xa_resource_new(
                         name.c_str(), switch_file.c_str(),
                         open_info.c_str(), close_info.c_str())))
            throw Exception(LIXA_RC_NULL_OBJECT,
                            "xta_native_xa_resource_new");
    };
    
    NativeXaResource::~NativeXaResource()
    {
        xta_native_xa_resource_delete(nxar);
        nxar = NULL;
    };
    
}
