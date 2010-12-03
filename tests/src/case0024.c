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
    int rc, mode, test_rc, dynam;
    int rmid1 = 2, rmid2 = 3;
    
    if (argc < 4) {
        fprintf(stderr, "%s: at least three options must be specified\n",
                argv[0]);
        exit (1);
    }

    mode = strtol(argv[1], NULL, 0);
    dynam = strtol(argv[2], NULL, 0);
    test_rc = strtol(argv[3], NULL, 0);

    if (mode < 1 || mode > 2) {
        fprintf(stderr, "%s: mode must be 1 (commit), 2 (rollback)\n",
                argv[0]);
        exit (1);
    }

    printf("%s| starting...\n", pgm);

    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);

    if (dynam & 1) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid1, rc = lixa_monkeyrm_call_ax_reg(rmid1));
        assert(TX_OK == rc);
    }
    if (dynam & 2) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid2, rc = lixa_monkeyrm_call_ax_reg(rmid2));
        assert(TX_OK == rc);
    }
    
    switch (mode) {
        case 1:
            printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
            break;
        case 2:
            printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
            break;
        default:
            fprintf(stderr, "%s| invalid mode: %d\n", pgm, mode);
            exit (1);
    }
    assert(test_rc == rc);

    /* memory leak prevention */
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    if (TX_FAIL == rc)
        lixa_tx_close_cleanup();
    /*
    lixa_monkeyrm_call_cleanup();
    */
    
    printf("%s| ...finished\n", pgm);
    return 0;
}
