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
#ifndef TRANSACTION_HPP
# define TRANSACTION_HPP



/* include XTA header file */
#include "xta.h"
#include "XaResource.hpp"



namespace xta {
    /**
     * XTA Transaction Manager class
     */
    class Transaction {
        public:
        /**
         * A Transaction object can be created only if the base C object has
         * been already created (this is just a wrapper, not a native C++
         * implementation)
         */
        Transaction(xta_transaction_t *tx);
        ~Transaction();
        /**
         * Enlist the resource specified with the Transaction associated with
         * the Transaction object
         * @param[in] xaRes : resource to associate
         */
        void EnlistResource(XaResource *xaRes);
        
        private:
        /**
         * Pointer to the native C object
         */
        xta_transaction_t *tx;
    };
};



#endif /* TRANSACTION_HPP */
