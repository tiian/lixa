/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <config.h>



#include <tx.h>
#include <lixa_tx.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



int tx_begin(void)
{
    int txrc = TX_FAIL;
    lixa_tx_begin(&txrc);
    return txrc;
}



int tx_close(void)
{
    int txrc = TX_FAIL;
    lixa_tx_close(&txrc);
    return txrc;
}



int tx_commit(void)
{
    int txrc = TX_FAIL;
    int begin_new = FALSE;
    lixa_tx_commit(&txrc, &begin_new);
    if (begin_new)
        lixa_tx_begin(&txrc);
    return txrc;
}



int tx_info(TXINFO *info)
{
    int txrc = TX_FAIL;
    lixa_tx_info(&txrc, info);
    return txrc;
}



int tx_open(void)
{
    int txrc = TX_FAIL;
    lixa_tx_open(&txrc);
    return txrc;
}



int tx_rollback(void)
{
    int txrc = TX_FAIL;
    int begin_new = FALSE;
    lixa_tx_rollback(&txrc, &begin_new);
    if (begin_new)
        lixa_tx_begin(&txrc);
    return txrc;
}



int tx_set_commit_return(COMMIT_RETURN when_return)
{
    int txrc = TX_FAIL;
    lixa_tx_set_commit_return(&txrc, when_return);
    return txrc;
}



int tx_set_transaction_control(TRANSACTION_CONTROL control)
{
    int txrc = TX_FAIL;
    lixa_tx_set_transaction_control(&txrc, control);
    return txrc;
}



int tx_set_transaction_timeout(TRANSACTION_TIMEOUT timeout)
{
    int txrc = TX_FAIL;
    lixa_tx_set_transaction_timeout(&txrc, timeout);
    return txrc;
}
    


