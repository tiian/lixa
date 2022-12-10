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
 * XTA test case: the same transaction object is started many times
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
    char *xid_string = NULL;
    /* control variables */
    int        commit;
    int        multiple_branches;
    int        re_create;

    /* turn ON trace for debugging purpose */
    xta_init();
    
    printf("%s| starting...\n", pgm);
    if (argc < 4) {
        fprintf(stderr, "%s: at least three options must be specified\n",
                argv[0]);
        return 1;
    }
    commit = strtol(argv[1], NULL, 0);
    multiple_branches = strtol(argv[2], NULL, 0);
    re_create = strtol(argv[3], NULL, 0);
    
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        printf("%s| xta_transaction_manager_new: returned NULL\n", pgm);
        return 1;
    }
    /*
     * create two XA native (static) resource objects linked to the first
     * and second Resouce Manager configured in LIXA profile
     */
    if (NULL == (native_xa_res0 = xta_native_xa_resource_new_by_rmid(
                     0, xta_transaction_manager_get_config()))) {
        printf("%s| xta_native_xa_resource_new(0): returned NULL\n", pgm);
        return 1;
    }
    if (NULL == (native_xa_res1 = xta_native_xa_resource_new_by_rmid(
                     1, xta_transaction_manager_get_config()))) {
        printf("%s| xta_native_xa_resource_new(1): returned NULL\n", pgm);
        return 1;
    }
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_begin: returned NULL\n", pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }

    /* start a Distributed Transaction */
    if (LIXA_RC_OK != (rc = xta_transaction_start(tx, multiple_branches))) {
        printf("%s| xta_transaction_start: returned %d\n", pgm, rc);
        return 1;
    }

    /* retrieve the generated XID */
    xid_string = xta_xid_to_string(xta_transaction_get_xid(tx));
    if (xid_string == NULL) {
        fprintf(stderr, "xta_transaction_get_xid returned NULL\n");
        return 1;
    } else {
        /* print it and release */
        printf("XID: '%s'\n", xid_string);
        free(xid_string);
        xid_string = NULL;
    }
    
    /* commit the Distributed Transaction */
    if (commit) {
        if (LIXA_RC_OK != (rc = xta_transaction_commit(tx, FALSE))) {
            fprintf(stderr, "%s| xta_transaction_commit: returned %d\n",
                    pgm, rc);
            return 1;
        }
        printf("%s| XTA commit performed\n", pgm);
    } else {
        if (LIXA_RC_OK != (rc = xta_transaction_rollback(tx))) {
            fprintf(stderr, "%s| xta_transaction_rollback: returned %d\n",
                    pgm, rc);
            return 1;
        }
        printf("%s| XTA rollback performed\n", pgm);
    }
    
    /* create a new transaction for this thread */
    if (re_create &&
        NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_begin: returned NULL\n", pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }
    
    /* start a Distributed Transaction AGAIN */
    rc = xta_transaction_start(tx, multiple_branches);
    if (LIXA_RC_NON_REUSABLE_TX == rc) {
        printf("%s| xta_transaction_start: returned %d ('%s')\n", pgm, rc,
               lixa_strerror(rc));
        return 2;
    } else if (LIXA_RC_OK != rc) {
        printf("%s| xta_transaction_start: returned %d\n", pgm, rc);
        return 1;
    }

    /* retrieve the generated XID */
    xid_string = xta_xid_to_string(xta_transaction_get_xid(tx));
    if (xid_string == NULL) {
        fprintf(stderr, "xta_transaction_get_xid returned NULL\n");
        return 1;
    } else {
        /* print it and release */
        printf("XID: '%s'\n", xid_string);
        free(xid_string);
        xid_string = NULL;
    }
    
    /* commit the Distributed Transaction */
    if (commit) {
        if (LIXA_RC_OK != (rc = xta_transaction_commit(tx, FALSE))) {
            fprintf(stderr, "%s| xta_transaction_commit: returned %d\n",
                    pgm, rc);
            return 1;
        }
        printf("%s| XTA commit performed\n", pgm);
    } else {
        if (LIXA_RC_OK != (rc = xta_transaction_rollback(tx))) {
            fprintf(stderr, "%s| xta_transaction_rollback: returned %d\n",
                    pgm, rc);
            return 1;
        }
        printf("%s| XTA rollback performed\n", pgm);
    }
    
    /* create a new transaction for this thread */
    if (re_create &&
        NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_begin: returned NULL\n", pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }
    
    /* start a Distributed Transaction for the third time... */
    if (LIXA_RC_OK != (rc = xta_transaction_start(tx, multiple_branches))) {
        printf("%s| xta_transaction_start: returned %d\n", pgm, rc);
        return 1;
    }

    /* retrieve the generated XID */
    xid_string = xta_xid_to_string(xta_transaction_get_xid(tx));
    if (xid_string == NULL) {
        fprintf(stderr, "xta_transaction_get_xid returned NULL\n");
        return 1;
    } else {
        /* print it and release */
        printf("XID: '%s'\n", xid_string);
        free(xid_string);
        xid_string = NULL;
    }
    
    /* commit the Distributed Transaction */
    if (commit) {
        if (LIXA_RC_OK != (rc = xta_transaction_commit(tx, FALSE))) {
            fprintf(stderr, "%s| xta_transaction_commit: returned %d\n",
                    pgm, rc);
            return 1;
        }
        printf("%s| XTA commit performed\n", pgm);
    } else {
        if (LIXA_RC_OK != (rc = xta_transaction_rollback(tx))) {
            fprintf(stderr, "%s| xta_transaction_rollback: returned %d\n",
                    pgm, rc);
            return 1;
        }
        printf("%s| XTA rollback performed\n", pgm);
    }
    
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    /*
     * delete first native XA Resource object
     */
    xta_native_xa_resource_delete(native_xa_res0);
    /*
     * delete second native XA Resource object
     */
    xta_native_xa_resource_delete(native_xa_res1);
    /*
     * end of XTA API calls
     */
    printf("%s| ...finished\n", pgm);
    return 0;
}
