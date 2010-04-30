/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#include <unistd.h>


#include <tx.h>



/* this is a special case test: it is a basic test for dynamically registered
   resource managers */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    TXINFO info;
    
    printf("%s| starting...\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, tx_open());
    printf("%s| tx_begin(): %d\n", pgm, tx_begin());
    printf("%s| tx_info(): %d\n", pgm, tx_info(&info));
    printf("%s| tx_commit(): %d\n", pgm, tx_commit());
    printf("%s| tx_close(): %d\n", pgm, tx_close());
    printf("%s| ...finished\n", pgm);
    return 0;
}
