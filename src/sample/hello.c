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
#include <stdio.h>
#include <unistd.h>


#include <tx.h>



int main(int argc, char *argv[])
{
    TXINFO info;
    /*
    int rc, rmid = 0;
    */
    
    printf("lixa hello before first tx_open()\n");
    printf("lixa hello: tx_open(): %d\n", tx_open());
    printf("lixa hello: tx_info(): %d\n", tx_info(&info));
    /* this is a supported characteristic */
    printf("lixa hello: tx_set_commit_return(): %d\n",
           tx_set_commit_return(TX_COMMIT_COMPLETED));
    /* this is an unsupported characteristic */
    printf("lixa hello: tx_set_commit_return(): %d\n",
           tx_set_commit_return(TX_COMMIT_DECISION_LOGGED));
    printf("lixa hello: tx_set_transaction_control(): %d\n",
           tx_set_transaction_control(TX_CHAINED));
    printf("lixa hello: tx_set_transaction_control(): %d\n",
           tx_set_transaction_control(TX_UNCHAINED));
    printf("lixa hello: tx_set_transaction_timeout(): %d\n",
           tx_set_transaction_timeout(3));
    printf("lixa hello: tx_begin(): %d\n", tx_begin());
    sleep(2);
    printf("lixa hello: tx_info(): %d\n", tx_info(&info));
    printf("lixa hello: tx_commit(): %d\n", tx_commit());
    /*
    printf("lixa hello: tx_rollback(): %d\n", tx_rollback());
    */
    printf("lixa hello: tx_close(): %d\n", tx_close());
    printf("lixa hello after first tx_close()\n");



    printf("lixa hello before second tx_open()\n");
    printf("lixa hello: tx_open(): %d\n", tx_open());
    printf("lixa hello: tx_info(): %d\n", tx_info(&info));
    /* this is a supported characteristic */
    printf("lixa hello: tx_set_commit_return(): %d\n",
           tx_set_commit_return(TX_COMMIT_COMPLETED));
    /* this is an unsupported characteristic */
    printf("lixa hello: tx_set_commit_return(): %d\n",
           tx_set_commit_return(TX_COMMIT_DECISION_LOGGED));
    printf("lixa hello: tx_set_transaction_control(): %d\n",
           tx_set_transaction_control(TX_CHAINED));
    printf("lixa hello: tx_set_transaction_control(): %d\n",
           tx_set_transaction_control(TX_UNCHAINED));
    printf("lixa hello: tx_set_transaction_timeout(): %d\n",
           tx_set_transaction_timeout(3));
    printf("lixa hello: tx_begin(): %d\n", tx_begin());
    sleep(2);
    printf("lixa hello: tx_info(): %d\n", tx_info(&info));
    printf("lixa hello: tx_commit(): %d\n", tx_commit());
    /*
    printf("lixa hello: tx_rollback(): %d\n", tx_rollback());
    */
    printf("lixa hello: tx_close(): %d\n", tx_close());
    printf("lixa hello after second tx_close()\n");

    return 0;
}
