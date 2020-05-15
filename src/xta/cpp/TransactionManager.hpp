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
#ifndef TRANSACTION_MANAGER_HPP
# define TRANSACTION_MANAGER_HPP



/* include XTA header file */
#include "xta.h"
#include "Transaction.hpp"



namespace xta {
    /**
     * XTA Transaction Manager class
     */
    class TransactionManager {
        public:
        TransactionManager();
        ~TransactionManager();

        /**
         * Create a new XA Transaction object, associate it with the current
         * process/thread and returns it to the caller. In the event that the
         * caller thread has already created an XA Transaction, the previously
         * created XA Transaction object is returned
         */
        Transaction createTransaction();
        private:
        /**
         * Pointer to the native C object
         */
        xta_transaction_manager_t *tm;
    };
};



#endif /* TRANSACTION_MANAGER_HPP */
