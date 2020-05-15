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



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



#include "Exception.hpp"
#include "TransactionManager.hpp"



namespace xta {
    
    TransactionManager::TransactionManager()
    {
        if (NULL == (tm = xta_transaction_manager_new()))
            throw Exception(LIXA_RC_NULL_OBJECT,
                            "xta_transaction_manager_new");
    };

    TransactionManager::~TransactionManager()
    {
        xta_transaction_manager_delete(tm);
        tm = NULL;
    };
    
    Transaction TransactionManager::createTransaction()
    {
        xta_transaction_t *tx = NULL;
        if (NULL == (tx = xta_transaction_manager_create_transaction(tm)))
            throw Exception(LIXA_RC_NULL_OBJECT,
                            "xta_transaction_manager_create_transaction");
        Transaction t(tx);
        return t;
    };
    
}
