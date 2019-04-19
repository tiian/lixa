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
#ifndef XTA_HPP
# define XTA_HPP



/* include XTA header file */
#include "xta.h"
/* include all XTA C++ header files */
#include "Config.hpp"
#include "Exception.hpp"
#include "NativeXaResource.hpp"
#include "MysqlXaResource.hpp"
#include "PostgresqlXaResource.hpp"
#include "Transaction.hpp"
#include "TransactionManager.hpp"



namespace xta {
    class Xta {
        public:
        /**
         * Initialize XTA environment: it must be call by the main program
         * before calling other XTA methods
         */
        static void init(void);
    };
};



#endif /* XTA_HPP */
