/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef NATIVE_XA_RESOURCE_HPP
# define NATIVE_XA_RESOURCE_HPP



/* include XTA header file */
#include "xta.h"
#include "XaResource.hpp"



namespace xta {
    class NativeXaResource : public XaResource {
        public:
        /**
         * Create a new Native XA Resource class
         * @param[in] name : the name assigned to the Resource Manager just for
         *                   debugging purposes
         * @param[in] switch_file : absolute path of the XA switch file that
         *                          points to xa_ functions for the resource
         *                          manager
         * @param[in] open_info : a string that may contain instance-specific
         *                        information for the resource manager when
         *                        xa_open must be called
         * @param[in] close_info : a string that may contain instance-specific
         *                         information for the resource manager when
         *                         xa_close must be called
         */
        NativeXaResource(std::string const& name,
                         std::string const& switch_file,
                         std::string const& open_info,
                         std::string const& close_info);
        ~NativeXaResource();
        /**
         * Return a pointer to the C base XA resource object
         */
        xta_xa_resource_t *getCBaseXaResource() {
            return (xta_xa_resource_t *)nxar; }
        
        private:
        /**
         * Pointer to the native C object
         */
        xta_native_xa_resource_t *nxar;
    };
};



#endif /* NATIVE_XA_RESOURCE_HPP */
