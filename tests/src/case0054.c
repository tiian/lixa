/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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



/* this is a special case test: it is a basic test for statically registered
   resource managers with a code-specified profile supplied as an argument */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    char *profile = NULL;
    int rc;
    TXINFO info;

    if (argc > 0) {
        profile = argv[1];
    }
    
    printf("%s| starting...\n", pgm);
    printf("%s| lixa_tx_set_profile(%s): %d\n", pgm, profile ? profile : "NULL", rc = lixa_tx_set_profile(profile));
    assert(TX_OK == rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    if (TX_ERROR == rc) {
        /* memory leak prevention */
        printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
        lixa_monkeyrm_call_cleanup();
    }
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(&info));
    assert(1 == rc);
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);

    /* memory leak prevention */
    lixa_monkeyrm_call_cleanup();

    printf("%s| ...finished\n", pgm);
    return 0;
}
