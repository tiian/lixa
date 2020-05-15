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
 * Test xta_config_t and its methods
 */



/*
 * EXIT CODES:
 *  0: OK
 *  1: generic error
 */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    /* XTA variables (objects) */
    xta_transaction_manager_t *tm;
    xta_transaction_t *tx;
    xta_config_t *cfg;
    /* Configuration values to be checked */
    int connection_timeout;

    /* turn ON trace for debugging purpose */
    xta_init();
    
    printf("%s| starting...\n", pgm);
    if (argc < 2) {
        fprintf(stderr, "%s: at least one option must be specified\n",
                argv[0]);
        return 1;
    }
    connection_timeout = (int)strtol(argv[1], NULL, 0);
    
    /*
     * create a Transaction Manager object
     */
    if (NULL == (tm = xta_transaction_manager_new())) {
        printf("%s| xta_transaction_manager_new: returned NULL\n", pgm);
        return 1;
    }
    /* create a new transaction for this thread */
    if (NULL == (tx = xta_transaction_manager_create_transaction(tm))) {
        printf("%s| xta_transaction_manager_get_transaction: returned NULL\n",
                pgm);
        return 1;
    } else {
        printf("%s| xta_transaction_manager_get_transaction: transaction "
               "reference is %p\n", pgm, tx);
    }


    cfg = xta_transaction_get_config(tx);
    printf("%s| xta_config_get_connection_timeout() = %d\n",
           pgm, xta_config_get_connection_timeout(cfg));
    /* check the value from config file or environment variable */
    if (connection_timeout != xta_config_get_connection_timeout(cfg)) {
        printf("%s| expected value = %d, real value = %d\n", pgm,
               connection_timeout, xta_config_get_connection_timeout(cfg));
        return 1;
    }
    /* set a new value and check it */
    xta_config_set_connection_timeout(cfg, 1234);
    if (1234 != xta_config_get_connection_timeout(cfg)) {
        printf("%s| expected value = %d, real value = %d\n", pgm,
               1234, xta_config_get_connection_timeout(cfg));
        return 1;
    }
    
    /*
     * delete Transaction Manager object
     */
    xta_transaction_manager_delete(tm);
    /*
     * end of XTA API calls
     */
    printf("%s| ...finished\n", pgm);
    return 0;
}
