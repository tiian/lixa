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



/* include C++ standard header files */
#include <string>

/* include XTA header file */
#include "xta.h"
#include "XaResource.hpp"



using namespace std;



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
        /**
         * Prepare the XA Resource Managers and the XA Transaction Manager for
         * a new transactional Unit of Work. From the XA specification point of
         * view, it calls xa_open (for the Native XA Resource Managers)
         */
        void Open(void);
        /**
         * Start a new XA Transaction. From the XA specification point of
         * view, it calls xa_start (for the Native XA Resource Managers)
         * @param[in] MultipleBranches : boolean value: <br>
         *            true = the created transaction will span more
         *                   applications,
         *                   @ref Branch will be called subsequently <br>
         *            false = the created transaction will not span more
         *                   applications and @ref Branch will
         *                   not be called for this transaction <br>
         */
        void Start(bool MultipleBranches=false);
        /**
         * Commit the transaction represented by this transaction object
         * @param[in] NonBlocking boolean value: <br>
         *            true = xa_prepare will not block the caller <br>
         *            false = xa_prepare will block the caller <br>
         *            the option is used only for multiple branch transactions
         */
        void Commit(bool NonBlocking=false);
        /**
         * Rollback the transaction represented by this transaction object
         */
        void Rollback(void);
        /**
         * Shut down the XA Resource Managers and the XA Transaction Manager
         * after transactional Unit of Work completion. From the XA
         * specification point of view, it calls xa_close (for the Native XA
         * Resource Managers)
         */
        void Close(void);
        /**
         * Create a new branch of the transaction represented by xid in this
         * transaction object; the global transaction has been previously
         * started
         * @param[in] XidString serialized identifier of the global transaction
         *            that must be branched
         */
        void Branch(const string& XidString);
        
        private:
        /**
         * Pointer to the native C object
         */
        xta_transaction_t *tx;
    };
};



#endif /* TRANSACTION_HPP */
