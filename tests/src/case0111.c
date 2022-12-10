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



#include "xta/xta.h"



/*
 * Forced xta_transaction_recover in a loop of multiple transactions
 * Related issue: https://github.com/tiian/lixa/issues/6
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    /* XTA variables (objects) */
    xta_transaction_manager_t *tm;
    xta_transaction_t *tx;
    xta_native_xa_resource_t *native_xa_res0 = NULL;
    xta_native_xa_resource_t *native_xa_res1 = NULL;
    /* control variables */
    int        commit;
    int        test_rc;
    int        number_of_resources;
    int        i;

    /* turn ON trace for debugging purpose */
    xta_init();
    
    printf("%s| starting...\n", pgm);
    if (argc < 4) {
        fprintf(stderr, "%s: at least three options must be specified\n",
                argv[0]);
        return 1;
    }
    commit = strtol(argv[1], NULL, 0);
    test_rc = strtol(argv[2], NULL, 0);
    number_of_resources = strtol(argv[3], NULL, 0);
    if (number_of_resources < 1 || number_of_resources > 2) {
        printf("%s| number of resources must be 1 or 2\n", pgm);
        return 1;
    }
    
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        printf("%s| xta_transaction_manager_new: returned NULL\n", pgm);
        return 1;
    }
    /*
     * create an XA native (static) resource object linked to the first Resouce
     * Manager configured in LIXA profile
     */
    if (NULL == (native_xa_res0 = xta_native_xa_resource_new_by_rmid(
                     0, xta_transaction_manager_get_config()))) {
        printf("%s| xta_native_xa_resource_new(0): returned NULL\n", pgm);
        return 1;
    }
    if (number_of_resources == 2) {
        /*
         * create another XA native (static) resource object linked to the
         * second Resouce Manager configured in LIXA profile
         */
        if (NULL == (native_xa_res1 = xta_native_xa_resource_new_by_rmid(
                         1, xta_transaction_manager_get_config()))) {
            printf("%s| xta_native_xa_resource_new(1): returned NULL\n", pgm);
            return 1;
        }
    }
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_begin: returned NULL\n", pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }

    /* a loop on the same transaction */
    for (i=0; i<10; ++i) {
        printf("%s| executing transaction number %i\n", pgm, i);
    
        /* force a recovery of previous transactions, if any */
        rc = xta_transaction_recover(tx);
        if (LIXA_RC_OK != rc && LIXA_RC_BYPASSED_OPERATION != rc) {
            printf("%s| xta_transaction_recover: returned %d\n", pgm, rc);
            return 1;
        }
    
        /* start a Distributed Transaction */
        if (LIXA_RC_OK != (rc = xta_transaction_start(tx, FALSE))) {
            printf("%s| xta_transaction_start: returned %d\n", pgm, rc);
            return 1;
        }

        /* commit the Distributed Transaction */
        if (commit) {
            if (test_rc != (rc = xta_transaction_commit(tx, FALSE))) {
                fprintf(stderr, "%s| xta_transaction_commit: returned %d "
                        "instead of %d\n", pgm, rc, test_rc);
                return 1;
            }
            printf("%s| XTA commit performed\n", pgm);
        } else {
            if (test_rc != (rc = xta_transaction_rollback(tx))) {
                fprintf(stderr, "%s| xta_transaction_rollback: returned %d "
                        "instead of %d\n", pgm, rc, test_rc);
                return 1;
            }
            printf("%s| XTA rollback performed\n", pgm);
        }
    } /* for i */
    
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    /*
     * delete first native XA Resource object
     */
    xta_native_xa_resource_delete(native_xa_res0);
    if (number_of_resources == 2) {
        /*
         * delete second native XA Resource object
         */
        xta_native_xa_resource_delete(native_xa_res1);
    }
    /*
     * end of XTA API calls
     */
    printf("%s| ...finished\n", pgm);
    return 0;
}
