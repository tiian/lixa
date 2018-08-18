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



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



#include "Exception.hpp"
#include "Transaction.hpp"



namespace xta {
    
    Transaction::Transaction(xta_transaction_t *tx)
    {
        this->tx = tx;
    };

    Transaction::~Transaction()
    {
        /* this->tx is just a reference to an object created/destroyed by
           TransactionManager */
        this->tx = NULL;
    };

    void Transaction::EnlistResource(XaResource *xaRes)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                               tx, xaRes->getCBaseXaResource())))
            throw Exception(rc, "xta_transaction_enlist_resource");
    }

    void Transaction::Open(void)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_open(tx)))
            throw Exception(rc, "xta_transaction_open");
    }

    void Transaction::Start(bool MultipleBranches)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_start(
                               tx, (int)MultipleBranches)))
            throw Exception(rc, "xta_transaction_start");
    }
    
    void Transaction::Commit(bool NonBlocking)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_commit(
                               tx, NonBlocking)))
            throw Exception(rc, "xta_transaction_commit");
    }
    
    void Transaction::Rollback(void)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_rollback(tx)))
            throw Exception(rc, "xta_transaction_rollback");
    }
    
    void Transaction::Close(void)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_close(tx)))
            throw Exception(rc, "xta_transaction_close");
    }

    void Transaction::Branch(const string& XidString)
    {
        int rc;
        if (LIXA_RC_OK != (rc = xta_transaction_branch(tx, XidString.c_str())))
            throw Exception(rc, "xta_transaction_branch");
    }

}
