/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <liblixamonkey.h>



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
    assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    if (0 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_UNCHAINED));
    if (0 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);

    printf("%s| tx_set_commit_return(): %d\n", pgm,
           rc = tx_set_commit_return(TX_COMMIT_COMPLETED));
    if (0 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    printf("%s| tx_set_transaction_timeout(): %d\n", pgm,
           rc = tx_set_transaction_timeout(1));
    if (0 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    if (0 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    if (0 == fail_point) {
        printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
        assert(TX_FAIL == rc);
        printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
        assert(TX_FAIL == rc);
        printf("%s| ...finished (TX_FAIL)\n", pgm);
        /* memory leak prevention */
        lixa_monkeyrm_call_cleanup();
        return 0;
    }
    
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    if (1 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    if (1 == fail_point) {
        assert(TX_FAIL == rc);
        printf("%s| ...finished (TX_FAIL)\n", pgm);
        /* memory leak prevention */
        lixa_monkeyrm_call_cleanup();
        return 0;
    } else
        assert(TX_OK == rc);
    
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    if (2 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);

    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    if (2 == fail_point) {
        assert(TX_FAIL == rc);
        printf("%s| ...finished (TX_FAIL)\n", pgm);
        /* memory leak prevention */
        lixa_monkeyrm_call_cleanup();
        return 0;
    } else
        assert(TX_OK == rc);
    
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    if (3 == fail_point)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    if (3 == fail_point) {
        assert(TX_FAIL == rc);
        printf("%s| ...finished (TX_FAIL)\n", pgm);
        exit(0);
    } else
        assert(TX_OK == rc);
    
    printf("%s| ...finished\n", pgm);
    return 0;
}
