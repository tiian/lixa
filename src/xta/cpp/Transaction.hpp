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
#include "Config.hpp"
#include "Xid.hpp"
#include "XaResource.hpp"



using namespace std;



namespace xta {
    /**
     * XTA Transaction class
     */
    class Transaction {
        public:
        /**
         * A Transaction object can be created only if the base C object has
         * been already created (this is just a wrapper, not a native C++
         * implementation)
         */
        Transaction(xta_transaction_t *tx);
        /**
         * Destructor must be called only by the @ref TransactionManager
         * class; if called directly by the customer program, it will just
         * nullify the pointer to the underlaying C object
         */
        ~Transaction();
        /**
         * Enlist the resource specified with the Transaction associated with
         * the Transaction object
         * @param[in] xaRes : resource to associate
         */
        void enlistResource(XaResource *xaRes);
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
        void start(bool MultipleBranches=false);
        /**
         * Commit the transaction represented by this transaction object
         * @param[in] NonBlocking boolean value: <br>
         *            true = xa_prepare will not block the caller <br>
         *            false = xa_prepare will block the caller <br>
         *            the option is used only for multiple branch transactions
         */
        void commit(bool NonBlocking=false);
        /**
         * Rollback the transaction represented by this transaction object
         */
        void rollback(void);
        /**
         * Suspend the transaction represented by this transaction object; the
         * transaction can be resumed with @ref resume at a later time
         * @param[in] Flags can be @ref TMMIGRATE if the Resource Manager
         *            supports transaction migration and @ref TMNOFLAGS
         *            otherwise
         */
        void suspend(long Flags=TMMIGRATE);
        /**
         * Resume the transaction represented by xid in this transaction
         * object; the transaction has been previously suspended with
         * @ref suspend
         * @param[in] XidString serialized identifier of the transaction that
         *            must be resumed
         * @param[in] Flags can be @ref TMRESUME if the transaction has been
         *            suspended using @ref TMMIGRATE or @ref TMJOIN if the
         *            transaction has been suspended using @ref TMNOFLAGS
         */
        void resume(const string& XidString, long Flags=TMRESUME);
        /**
         * Explicitly open and close all the enlisted resource to look for
         * recovery pending transaction in the LIXA state server. In normal
         * condition, this is not necessary, because the same happens when
         * @ref start, @ref resume and @ref branch are called
         */
        void recover(void);
        /**
         * Create a new branch of the transaction represented by xid in this
         * transaction object; the global transaction has been previously
         * started
         * @param[in] XidString serialized identifier of the global transaction
         *            that must be branched
         */
        void branch(const string& XidString);

        Xid getXid();

        Config getConfig();
        private:
        /**
         * Pointer to the native C object
         */
        xta_transaction_t *tx;
    };
};



#endif /* TRANSACTION_HPP */
