/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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



#include "xta.h"



/*
 * Trivial case test for XTA: just assuring it can be compiled and executed!
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    xta_transaction_manager_t *tm;
    xta_native_xa_resource_t *xa_res;

    /* activate tracing */
    LIXA_TRACE_INIT;
    
    printf("%s| starting...\n", pgm);
    /*
     * creating a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        printf("%s| xta_transaction_manager_new: returned NULL\n");
        return 1;
    }
    /*
     * creating an XA native resource object linked to the first Resouce
     * Manager configured in LIXA profile
     */
    if (NULL == (xa_res = xta_native_xa_resource_new(0, NULL, NULL))) {
        printf("%s| xta_native_xa_resource_new: returned NULL\n");
        return 1;
    }

    /*
     * deleting XA Resource object
     */
    xta_native_xa_resource_delete(xa_res);
    /*
     * deleting Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    
    /*
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    if (TX_ERROR == rc) {
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
    lixa_monkeyrm_call_cleanup();
    */
    printf("%s| ...finished\n", pgm);
    return 0;
}
