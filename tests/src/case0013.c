/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include <tx.h>
#include <liblixamonkey.h>


/* This case test is quite long because for every test, a different unique
   sequence of functions must be called */


int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    TXINFO info;
    int rc;
    int rmid = 1;
    
    printf("%s| starting...\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    
    printf("%s| tx_set_commit_return(): %d\n", pgm,
           rc = tx_set_commit_return(TX_COMMIT_COMPLETED));
    assert(TX_OK == rc);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_CHAINED));
    assert(TX_OK == rc);
    
    printf("%s| tx_set_transaction_timeout(): %d\n", pgm,
           rc = tx_set_transaction_timeout(2));
    assert(TX_OK == rc);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_COMMIT_COMPLETED == info.when_return);
    assert(TX_CHAINED == info.transaction_control);
    assert(2 == info.transaction_timeout);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_COMMIT_COMPLETED == info.when_return);
    assert(TX_CHAINED == info.transaction_control);
    assert(2 == info.transaction_timeout);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_COMMIT_COMPLETED == info.when_return);
    assert(TX_CHAINED == info.transaction_control);
    assert(2 == info.transaction_timeout);
    
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    assert(TM_OK == rc);

    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_COMMIT_COMPLETED == info.when_return);
    assert(TX_CHAINED == info.transaction_control);
    assert(2 == info.transaction_timeout);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_UNCHAINED));
    assert(TX_OK == rc);
    
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    
    return 0;
}
