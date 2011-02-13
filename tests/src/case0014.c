/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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


/* This case test is for tx_begin() */


int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc, test_rc;
    int rmid = 2;
    
    if (argc < 2) {
        fprintf(stderr, "%s: at least one option must be specified\n",
                argv[0]);
        exit (1);
    }
    test_rc = strtol(argv[1], NULL, 0);

    printf("%s| starting...\n", pgm);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_PROTOCOL_ERROR == rc);

    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_PROTOCOL_ERROR == rc);

    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    assert(TM_OK == rc);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OUTSIDE == rc);

    printf("%s| lixa_monkeyrm_call_ax_unreg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_unreg(rmid));
    assert(TM_OK == rc);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);

    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(test_rc == rc);
    if (TX_FAIL == test_rc) {
        /* test can not go on after TX_FAIL */
        /* memory leak prevention */
        lixa_monkeyrm_call_cleanup();    
        printf("%s| ...finished\n", pgm);
        return 0;
    }
    
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);

    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    assert(TM_OK == rc);

    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);

    /* memory leak prevention */
    lixa_monkeyrm_call_cleanup();    

    printf("%s| ...finished\n", pgm);
    return 0;
}
