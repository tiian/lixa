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



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include <tx.h>



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    TXINFO info;
    int rc;
    long fail_point;

    if (argc < 2) {
        fprintf(stderr, "%s: at least one option must be specified\n",
                argv[0]);
        exit(1);
    }

    fail_point = strtol(argv[1], NULL, 0);
    
    printf("%s| starting...\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    if (0 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_CHAINED));
    if (0 == fail_point) {
        assert(TX_FAIL == rc);
        exit(0);
    } else
        assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_CHAINED));
    assert(TX_OK == rc);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_CHAINED == info.transaction_control);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(333));
    assert(TX_EINVAL == rc);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_CHAINED == info.transaction_control);
    
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_PROTOCOL_ERROR == rc);
    
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_PROTOCOL_ERROR == rc);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_UNCHAINED));
    assert(TX_OK == rc);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    assert(TX_UNCHAINED == info.transaction_control);
    
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    
    printf("%s| ...finished\n", pgm);
    return 0;
}
