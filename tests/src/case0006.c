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



/* this case test is used to test TX_CHAINED transactions */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    TXINFO info1, info2;
    int commit;

    if (argc < 2) {
        fprintf(stderr, "%s: at least one option must be specified\n",
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

    printf("%s| starting...\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_CHAINED));
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(&info1));
    assert(1 == rc);

    /* emulate callback registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLExecDirect function call */
    lixa_monkeyrm_call_ax_reg(1);

    if (commit)
        printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    else
        printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(&info2));
    assert(1 == rc);

    /* compare transactions: they must be different! */
    assert(memcmp(&info1.xid, &info2.xid, sizeof(XID)) != 0);
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_PROTOCOL_ERROR == rc);

    /* memory leak prevention */
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_UNCHAINED));
    assert(TX_OK == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    lixa_monkeyrm_call_cleanup();    

    printf("%s| ...finished\n", pgm);
    return 0;
}
