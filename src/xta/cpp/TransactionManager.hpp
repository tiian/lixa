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
#ifndef TRANSACTION_MANAGER_HPP
# define TRANSACTION_MANAGER_HPP



/* include XTA header file */
#include "xta.h"



namespace xta {
    class TransactionManager {
        public:
        TransactionManager();
        ~TransactionManager();
        
        private:
        xta_transaction_manager_t *tm;
    };
};



#endif /* TRANSACTION_MANAGER_HPP */
