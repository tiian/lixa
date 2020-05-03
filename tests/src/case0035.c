/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "config.h"



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include "tx.h"
#include "liblixamonkey.h"
#include "lixa_utils.h"



/* this is a special case test: it is a basic test for dynamically registered
   resource managers */


/* this delay is necessary to exploits some bugs related to timing */
#define DELAY 200000


int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    TXINFO info;
    int commit;
    int exit_point;
    int test_rc, test_rc2;

    if (argc < 4) {
        fprintf(stderr, "%s: at least three options must be specified\n",
                argv[0]);
        exit (1);
    }
    if (!strcmp(argv[1], "commit"))
        commit = TRUE;
    else if (!strcmp(argv[1], "rollback"))
        commit = FALSE;
    else {
        fprintf(stderr, "%s: first option must be [commit|rollback]\n",
                argv[0]);
        exit (1);
    }
    exit_point = (int)strtol(argv[2], NULL, 0);
    test_rc = test_rc2 = (int)strtol(argv[3], NULL, 0);
    /* the last parameter is optional */
    if (argc == 5)
        test_rc2 = (int)strtol(argv[4], NULL, 0);
    
    printf("%s| starting (%s/%d/%d)...\n", pgm, argv[1], exit_point, test_rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    if (1 == exit_point)
        exit(1);

    /* emulate operations without XA transaction management */
    lixa_monkeyrm_call_ax_reg(2);
    lixa_micro_sleep(DELAY);
    lixa_monkeyrm_call_ax_reg(3);
    lixa_micro_sleep(DELAY);
    lixa_monkeyrm_call_ax_unreg(2);
    lixa_micro_sleep(DELAY);
    lixa_monkeyrm_call_ax_unreg(3);
    lixa_micro_sleep(DELAY);
    if (2 == exit_point)
        exit(2);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    if (TX_OK != test_rc && 3 == exit_point)
        assert(test_rc == rc);
    else
        assert(TX_OK == rc);
    if (3 == exit_point)
        exit(3);
    
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(&info));
    assert(1 == rc);

    /* emulate callback registration from Resource Manager when accessing
     * resource manager owned resources; you may imagine this is the
     * equivalent of a SQLExecDirect function call */
    lixa_monkeyrm_call_ax_reg(2);
    lixa_micro_sleep(DELAY);
    lixa_monkeyrm_call_ax_reg(3);
    lixa_micro_sleep(DELAY);
    if (4 == exit_point)
        exit(4);

    if (commit)
        printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    else
        printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    if ((TX_OK != test_rc || TX_OK != test_rc2) && 5 == exit_point)
        assert(test_rc == rc || test_rc2 == rc);
    else
        assert(TX_OK == rc);
    if (5 == exit_point)
        exit(5);
    lixa_micro_sleep(DELAY);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    if (TX_OK != test_rc && 6 == exit_point)
        assert(test_rc == rc);
    else
        assert(TX_OK == rc);
    if (6 == exit_point)
        exit(6);
    printf("%s| ...finished\n", pgm);
    return 0;
}
