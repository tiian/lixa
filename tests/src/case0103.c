/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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



#include "xta/xta.h"



/*
 * Standard case test for XTA: simple transaction
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    char *xid_string = NULL;
    FILE *xid_file = NULL;
    
    /* XTA variables (objects) */
    xta_transaction_manager_t *tm;
    xta_transaction_t *tx;
    xta_native_xa_resource_t *native_xa_res0, *native_xa_res1;
    /* control variables */
    enum Phase { INITIAL, INTERMEDIATE, FINAL, NO_PHASE } phase;
    int        commit;
    int        test_rc;
    const char *filename;

    /* turn ON trace for debugging purpose */
    xta_init();
    
    fprintf(stderr, "%s| starting...\n", pgm);
    if (argc < 5) {
        fprintf(stderr, "%s: at least four options must be specified\n",
                argv[0]);
        return 1;
    }
    phase = strtol(argv[1], NULL, 0);
    commit = strtol(argv[2], NULL, 0);
    test_rc = strtol(argv[3], NULL, 0);
    filename = argv[4];

    /* check phase */
    switch(phase) {
        case INITIAL:
            fprintf(stderr, "%s| phase=%d (INITIAL)\n", pgm, phase);
            /* open file for write */
            if (NULL == (xid_file = fopen(filename, "w"))) {
                fprintf(stderr, "%s| error while opening file '%s'\n",
                        pgm, filename);
            }
            break;
        case INTERMEDIATE:
            fprintf(stderr, "%s| phase=%d (INTERMEDIATE)\n", pgm, phase);
            /* open file for read */
            if (NULL == (xid_file = fopen(filename, "r"))) {
                fprintf(stderr, "%s| error while opening file '%s'\n",
                        pgm, filename);
            }
            break;
        case FINAL:
            fprintf(stderr, "%s| phase=%d (FINAL)\n", pgm, phase);
            /* open file for read */
            if (NULL == (xid_file = fopen(filename, "r"))) {
                fprintf(stderr, "%s| error while opening file '%s'\n",
                        pgm, filename);
            }
            break;
        case NO_PHASE:
            fprintf(stderr, "%s| phase=%d (NO_PHASE)\n", pgm, phase);
            break;
        default:
            fprintf(stderr, "%s| phase=%d UNKNOWN!\n", pgm, phase);
            break;
    } /* switch(phase) */
    
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        fprintf(stderr, "%s| xta_transaction_manager_new: returned NULL\n",
                pgm);
        return 1;
    }
    /*
     * create two XA native (static) resource objects linked to the first and
     * second Resouce Managers configured in LIXA profile
     */
    if (NULL == (native_xa_res0 = xta_native_xa_resource_new_by_rmid(
                     0, xta_transaction_manager_get_config()))) {
        fprintf(stderr, "%s| xta_native_xa_resource_new(0): returned NULL\n",
                pgm);
        return 1;
    }
    if (NULL == (native_xa_res1 = xta_native_xa_resource_new_by_rmid(
                     1, xta_transaction_manager_get_config()))) {
        fprintf(stderr, "%s| xta_native_xa_resource_new(1): returned NULL\n",
                pgm);
        return 1;
    }
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        fprintf(stderr, "%s| xta_transaction_manager_begin: returned NULL\n",
                pgm);
        return 1;
    } else {
        fprintf(stderr, "%s| xta_transaction_manager_get_transaction: "
                "transaction reference is %p\n", pgm, tx);
    }
    /* register the native XA Resources to the transaction manager: this step
     * is useless but it's not dangerous */
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)native_xa_res0))) {
        fprintf(stderr, "%s| xta_transaction_enlist_resource/native_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }
    if (LIXA_RC_OK != (rc = xta_transaction_enlist_resource(
                           tx, (xta_xa_resource_t *)native_xa_res1))) {
        fprintf(stderr, "%s| xta_transaction_enlist_resource/native_xa_res: "
               "returned %d\n", pgm, rc);
        return 1;
    }

    /* open all the resources for Distributed Transactions */
    if (LIXA_RC_OK != (rc = xta_transaction_open(tx))) {
        fprintf(stderr, "%s| xta_transaction_open: returned %d\n", pgm, rc);
        return 1;
    }

    if (INITIAL == phase || NO_PHASE == phase) {
        /* start a Distributed Transaction */
        if (LIXA_RC_OK != (rc = xta_transaction_start(tx, FALSE))) {
            fprintf(stderr, "%s| xta_transaction_start: returned %d\n",
                    pgm, rc);
            return 1;
        }

        /* get XID as a string */
        if (NULL == (xid_string = xta_xid_to_string(
                         xta_transaction_get_xid(tx)))) {
            fprintf(stderr, "%s| xta XID is NULL\n", pgm);
            return 1;
        } else {
            fprintf(stderr, "%s| xta XID is '%s'\n", pgm, xid_string);
        }
        if (INITIAL == phase) {
            /* write to xid_file the transaction that will be resumed */
            fprintf(xid_file, "%s", xid_string);
            fclose(xid_file);
            xid_file = NULL;
        }
        /* release xid_string */
        free(xid_string);
        xid_string = NULL;
    } else {
        char buffer[1000];
        /* read from xid_file the transaction that must be resumed */
        if (NULL == fgets(buffer, sizeof(buffer), xid_file)) {
            fprintf(stderr, "%s| error while retrieving XID from file '%s'\n",
                    pgm, filename);
            return 1;
        }
        fprintf(stderr, "%s| xta XID is '%s'\n", pgm, buffer);
        fclose(xid_file);
        
        /* resume the transaction */
        if (LIXA_RC_OK != (rc = xta_transaction_resume(
                               tx, buffer, TMRESUME))) {
            fprintf(stderr, "%s| xta_transaction_resume returned %d\n",
                    pgm, rc);
            return 1;
        }
    }
    
    if (INITIAL == phase || INTERMEDIATE == phase) {
        /* suspend the transaction */
        if (test_rc != (rc = xta_transaction_suspend(tx, TMMIGRATE))) {
            fprintf(stderr, "%s| xta_transaction_suspend: returned %d instead "
                    "of %d\n", pgm, rc, test_rc);
            return 1;
        }
    } /* if (INITIAL == phase) */

    if (FINAL == phase || NO_PHASE == phase) {
        /* commit the Distributed Transaction */
        if (commit) {
            if (test_rc != (rc = xta_transaction_commit(tx, FALSE))) {
                fprintf(stderr, "%s| xta_transaction_commit: returned %d "
                        "instead of %d\n", pgm, rc, test_rc);
                return 1;
            }
            fprintf(stderr, "%s| XTA commit returned %d as expected\n",
                    pgm, rc);
        } else {
            if (test_rc != (rc = xta_transaction_rollback(tx))) {
                fprintf(stderr, "%s| xta_transaction_rollback: returned %d "
                        "instead of %d\n", pgm, rc, test_rc);
                return 1;
            }
            fprintf(stderr, "%s| XTA rollback returned %d as expected\n",
                    pgm, rc);
        }
    } /* if (FINAL == phase) */
    
    /* close all the resources for Distributed Transactions */
    if (LIXA_RC_OK != (rc = xta_transaction_close(tx))) {
        fprintf(stderr, "%s| xta_transaction_close: returned %d\n", pgm, rc);
        return 1;
    }
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    /*
     * delete native XA Resource objects
     */
    xta_native_xa_resource_delete(native_xa_res0);
    xta_native_xa_resource_delete(native_xa_res1);
    /*
     * end of XTA API calls
     */
    fprintf(stderr, "%s| ...finished\n", pgm);
    return 0;
}
