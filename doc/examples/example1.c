/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <stdio.h>
#include <stdlib.h>

#include <tx.h>



/* this basic example shows simple usage of the LIXA software */



int main(int argc, char *argv[])
{
    int rc;
    
    printf("tx_open(): %d\n", rc = tx_open());
    if (TX_OK != rc) exit(1);
    
    printf("tx_begin(): %d\n", rc = tx_begin());
    if (TX_OK != rc) exit(1);
    printf("tx_commit(): %d\n", rc = tx_commit());
    if (TX_OK != rc) exit(1);

    printf("tx_begin(): %d\n", rc = tx_begin());
    if (TX_OK != rc) exit(1);
    printf("tx_rollback(): %d\n", rc = tx_rollback());
    if (TX_OK != rc) exit(1);
    
    printf("tx_close(): %d\n", rc = tx_close());
    if (TX_OK != rc) exit(1);
    return 0;
}
