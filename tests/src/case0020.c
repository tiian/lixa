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


/* This case test is for tx_begin() */


int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc, chained, all_dyn, all_sta;
    int rmid = 1, rmid0 = 0;
    
    if (argc < 3) {
        fprintf(stderr, "%s: at least two options must be specified\n",
                argv[0]);
        exit (1);
    }
    chained = strtol(argv[1], NULL, 0);
    all_dyn = 1 == strtol(argv[2], NULL, 0);
    all_sta = 2 == strtol(argv[2], NULL, 0);

    printf("%s| starting...\n", pgm);
    printf("%s| testing state S0...\n", pgm);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_PROTOCOL_ERROR == rc);
 
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);

    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_PROTOCOL_ERROR == rc);

    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(NULL));
    assert(TX_PROTOCOL_ERROR == rc);

    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_PROTOCOL_ERROR == rc);

    printf("%s| tx_set_commit_return(): %d\n", pgm,
           rc = tx_set_commit_return(TX_COMMIT_COMPLETED));
    assert(TX_PROTOCOL_ERROR == rc);
    
    if (chained) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
    } else {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
    }
    assert(TX_PROTOCOL_ERROR == rc);
    
    printf("%s| tx_set_transaction_timeout(): %d\n", pgm,
           rc = tx_set_transaction_timeout(100));
    assert(TX_PROTOCOL_ERROR == rc);
    
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);

    if (chained) {
        printf("%s| testing state S2...\n", pgm);
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
    } else {
        printf("%s| testing state S1...\n", pgm);
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
    }
    assert(TX_OK == rc);

    /* S1 -> S3 -> S1 */
    /* S2 -> S4 -> S2 */
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    if (chained && !all_dyn)
        assert(TX_NO_BEGIN == rc);
    else
        assert(TX_OK == rc);
    if (chained && all_dyn) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
        assert(TX_OK == rc);
        printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
        assert(TX_OK == rc);
    }

    /* S1 -> S0 -> S1 */
    /* S2 -> S0 -> S2 */
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
        
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_PROTOCOL_ERROR == rc);

    /* S1 -> S0 -> S1 */
    /* S2 -> S0 -> S2 */
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(NULL));
    assert(0 == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);

    /* S1 -> S0 -> S1 */
    /* S2 -> S0 -> S2 */
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
        
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_PROTOCOL_ERROR == rc);

    /* S1 -> S0 -> S1 */
    /* S2 -> S0 -> S2 */
    printf("%s| tx_set_commit_return(): %d\n", pgm,
           rc = tx_set_commit_return(TX_COMMIT_COMPLETED));
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
        
    /* S1 -> S0 -> S1 */
    /* S2 -> S0 -> S2 */
    printf("%s| tx_set_transaction_timeout(): %d\n", pgm,
           rc = tx_set_transaction_timeout(100));
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);

    if (chained) {
        printf("%s| testing state S4...\n", pgm);
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
    } else {
        printf("%s| testing state S3...\n", pgm);
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
    }
    assert(TX_OK == rc);
            
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);

    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_PROTOCOL_ERROR == rc);
 
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_PROTOCOL_ERROR == rc);

    /* S3 -> S1 -> S3 */
    /* S4 -> S4 */
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
    assert(TX_OK == rc);
    if (chained) {
        if (all_dyn) {
            printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
                   pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
            assert(TM_OK == rc);
        }
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
        if (!all_sta)
            assert(TM_OK == rc);
        else
            assert(TMER_TMERR == rc);
        printf("%s| tx_commit(): %d\n", pgm, rc = tx_commit());
        if (!all_dyn)
            assert(TX_NO_BEGIN == rc);
        else {
            assert(TM_OK == rc);
            printf("%s| tx_set_transaction_control(): %d\n", pgm,
                   rc = tx_set_transaction_control(TX_UNCHAINED));
            assert(TX_OK == rc);
            printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
            assert(TX_OK == rc);
        }
    }
    if (chained && all_dyn) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
        assert(TX_OK == rc);
    }
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    
    /* S3 -> S1 -> S3 */
    /* S4 -> S4 */
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(NULL));
    assert(1 == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    if (!chained) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OK == rc);
    }
    
    /* S3 -> S1 -> S3 */
    /* S4 -> S4 */
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    if (!chained) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OK == rc);
    }
    
    /* S3 -> S1 -> S3 */
    /* S4 -> S4 */
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    if (chained) {
        if (all_dyn) {
            printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
                   pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
            assert(TM_OK == rc);
        }
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
        if (!all_sta)
            assert(TM_OK == rc);
        else
            assert(TMER_TMERR == rc);
        printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
        if (!all_dyn)
            assert(TX_NO_BEGIN == rc);
        else {
            assert(TM_OK == rc);
            printf("%s| tx_set_transaction_control(): %d\n", pgm,
                   rc = tx_set_transaction_control(TX_UNCHAINED));
            assert(TX_OK == rc);
            printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
            assert(TX_OK == rc);
        }
    }
    if (chained && all_dyn) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
        assert(TX_OK == rc);
    }
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    
    /* S3 -> S1 -> S3 */
    /* S4 -> S4 */
    printf("%s| tx_set_commit_return(): %d\n", pgm,
           rc = tx_set_commit_return(TX_COMMIT_COMPLETED));
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    if (!chained) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OK == rc);
    }
        
    /* S3 -> S1 -> S3 */
    /* S4 -> S4 */
    printf("%s| tx_set_transaction_timeout(): %d\n", pgm,
           rc = tx_set_transaction_timeout(100));
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    if (!chained) {
        printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
        assert(TX_OK == rc);
    }

    printf("%s| testing state chaining switch...\n", pgm);
    /* S3 -> S3 */
    /* S4 -> S4 */
    if (chained) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
    } else {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
    }
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    if (chained)
        assert(TX_PROTOCOL_ERROR == rc);
    else
        assert(TX_OK == rc);

    /* S3 -> S4 */
    /* S4 -> S3 */
    if (!chained) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
    } else {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
    }
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    if (!chained)
        assert(TX_PROTOCOL_ERROR == rc);
    else
        assert(TX_OK == rc);

    /* S3 -> S1 */
    /* S4 -> S1 */
    printf("%s| tx_set_transaction_control(): %d\n", pgm,
           rc = tx_set_transaction_control(TX_UNCHAINED));
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);

    /* S1 -> S1 */
    /* S2 -> S2 */
    if (chained) {
        /* S1 -> S2 */
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
        assert(TX_OK == rc);
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_CHAINED));
    } else {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
    }
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    if (chained)
        assert(TX_PROTOCOL_ERROR == rc);
    else
        assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    if (chained) {
        printf("%s| tx_set_transaction_control(): %d\n", pgm,
               rc = tx_set_transaction_control(TX_UNCHAINED));
        assert(TX_OK == rc);
    }    
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);

    /* S1 */
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    if (all_dyn) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, rmid0, rc = lixa_monkeyrm_call_ax_reg(rmid0));
        assert(TM_OK == rc);
    }
    printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
           pgm, rmid, rc = lixa_monkeyrm_call_ax_reg(rmid));
    if (!all_sta)
        assert(TM_OK == rc);
    else
        assert(TMER_TMERR == rc);
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);

    /* memory leak prevention */
    printf("%s| tx_rollback(): %d\n", pgm, rc = tx_rollback());
    assert(TX_OK == rc);
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_OK == rc);
    lixa_monkeyrm_call_cleanup();

    printf("%s| ...finished\n", pgm);
    return 0;
}
