/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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
    char *pgm = argv[0];
    TXINFO info;
    
    printf("%s| before tx_open()\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, tx_open());
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    /* this is a supported characteristic */
    printf("%s| tx_set_commit_return(): %d\n", pgm,
           tx_set_commit_return(TX_COMMIT_COMPLETED));
    /* this is an unsupported characteristic */
    printf("%s| tx_set_commit_return(): %d\n", pgm,
           tx_set_commit_return(TX_COMMIT_DECISION_LOGGED));
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           tx_set_transaction_control(TX_CHAINED));
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           tx_set_transaction_control(TX_UNCHAINED));
    printf("%s| tx_set_transaction_timeout(): %d\n", pgm,
           tx_set_transaction_timeout(2));
    printf("%s| tx_begin(): %d\n", pgm, tx_begin());
    sleep(1);
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    printf("%s| tx_commit(): %d\n", pgm, tx_commit());
    /*
    printf("%s| tx_rollback(): %d\n", pgm, tx_rollback());
    */
    printf("%s| tx_close(): %d\n", pgm, tx_close());
    printf("%s| after tx_close()\n", pgm);
    return 0;
}
