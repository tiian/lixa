/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
    /** @@@ must be fixed see bug 2907537 */
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
    /** @@@ must be fixed see bug 2907542 */
    return TX_OK;
}



int tx_info(TXINFO *info)
{
    /** @@@ must be fixed see bug 2907545 */
    return TX_OK;
}



int tx_open(void)
{
    int txrc = TX_FAIL;
    lixa_tx_open(&txrc);
    return txrc;
}



int tx_rollback(void)
{
    /** @@@ must be fixed see bug 2907548 */
    return TX_OK;
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
    /** @@@ must be fixed see bug 2907562 */
    return TX_OK;
}
    


