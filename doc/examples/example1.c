/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <stdio.h>
#include <stdlib.h>

#include <tx.h>



/* this basic example shows simple usage of the LIXA software */


void info(void)
{
    int rc;
    TXINFO info;

    printf("tx_info(): %d\n", rc = tx_info(&info));
    if (TX_OK > rc) exit(1);
    printf("xid.formatID: %ld\n", info.xid.formatID);
    printf("xid.gtrid_length: %ld\n", info.xid.gtrid_length);
    printf("xid.bqual_length: %ld\n", info.xid.bqual_length);
    printf("when_return: %ld (%s)\n", info.when_return,
           TX_COMMIT_COMPLETED == info.when_return ?
           "TX_COMMIT_COMPLETED" : "TX_COMMIT_DECISION_LOGGED");
    printf("transaction_control: %ld (%s)\n", info.transaction_control,
           TX_UNCHAINED == info.transaction_control ?
           "TX_UNCHAINED" : "TX_CHAINED");
    printf("transaction_timeout: %ld s\n", info.transaction_timeout);
    printf("transaction_state: %ld (%s)\n", info.transaction_state,
           TX_ACTIVE == info.transaction_state ?
           "TX_ACTIVE" :
           TX_TIMEOUT_ROLLBACK_ONLY == info.transaction_state ?
           "TX_TIMEOUT_ROLLBACK_ONLY" : "TX_ROLLBACK_ONLY"
           );           
}



int main(int argc, char *argv[])
{
    int rc;

    printf("Executing %s\n", argv[0]);
    printf("tx_open(): %d\n", rc = tx_open());
    if (TX_OK != rc) exit(1);
    printf("tx_begin(): %d\n", rc = tx_begin());
    if (TX_OK != rc) exit(1);
    info();
    
    printf("tx_commit(): %d\n", rc = tx_commit());
    if (TX_OK != rc) exit(1);
    info();

    /* set non default parameters */
    /* LIXA does not support this option */
    printf("tx_set_commit_return(): %d\n",
           rc = tx_set_commit_return(TX_COMMIT_DECISION_LOGGED));
    if (TX_NOT_SUPPORTED != rc) exit(1);
    /* LIXA supports timeout */
    printf("tx_set_transaction_timeout(): %d\n",
           rc = tx_set_transaction_timeout(5));
    if (TX_OK != rc) exit(1);
    /* LIXA supports transaction control */
    printf("tx_set_transaction_control(): %d\n",
           rc = tx_set_transaction_control(TX_CHAINED));
    if (TX_OK != rc) exit(1);
    info();
    /* Transaction control must be resetted to avoid error during close */
    printf("tx_set_transaction_control(): %d\n",
           rc = tx_set_transaction_control(TX_UNCHAINED));
    if (TX_OK != rc) exit(1);
    info();
    
    printf("tx_begin(): %d\n", rc = tx_begin());
    if (TX_OK != rc) exit(1);
    printf("tx_rollback(): %d\n", rc = tx_rollback());
    if (TX_OK != rc) exit(1);
    
    printf("tx_close(): %d\n", rc = tx_close());
    if (TX_OK != rc) exit(1);
    return 0;
}
