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
    long begin_pos;
    int rmid;

    if (argc < 2) {
        fprintf(stderr, "%s: at least one option must be specified\n",
                argv[0]);
        exit (1);
    }
    begin_pos = strtol(argv[1], NULL, 0);
    
    printf("%s| starting...\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);

    if (0 == begin_pos) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OK == rc);
    }
    
    /* emulate callback registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLExecDirect function call */
    for (rmid=0; rmid<3; ++rmid) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    }
    
    if (1 == begin_pos) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OUTSIDE == rc);
    }
    
    /* emulate callback de-registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLEndTran function call */
    for (rmid=0; rmid<3; ++rmid) {
        printf("%s| lixa_monkeyrm_call_ax_unreg(%d): %d\n",
               pgm, rmid, rc = lixa_monkeyrm_call_ax_unreg(rmid));
        if (0 == begin_pos)
            assert(TMER_PROTO == rc);
    }
    
    if (2 == begin_pos) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OK == rc);
    }
    
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    if (1 == begin_pos)
        assert(TX_PROTOCOL_ERROR == rc);
    else
        assert(TX_OK == rc);

    /* emulate callback registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLExecDirect function call */
    for (rmid=0; rmid<3; ++rmid) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
        assert(TM_OK == rc);
    }
    
    /* emulate callback de-registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLEndTran function call */
    for (rmid=0; rmid<3; ++rmid) {
        printf("%s| lixa_monkeyrm_call_ax_unreg(%d): %d\n",
               pgm, rmid, rc = lixa_monkeyrm_call_ax_unreg(rmid));
        assert(TM_OK == rc);
    }
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);

    /* memory leak prevention */
    lixa_monkeyrm_call_cleanup();    

    printf("%s| ...finished\n", pgm);
    return 0;
}
