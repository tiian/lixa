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
#include "MysqlXaResource.hpp"
#include "PostgresqlXaResource.hpp"
#include "Transaction.hpp"
#include "TransactionManager.hpp"



namespace xta {
    class Xta {
        public:
        static void Init(void);
    };
};



#endif /* XTA_HPP */
