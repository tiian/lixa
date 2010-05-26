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



/* this case test is used to verify what's happen if tx_close is called when
   there is an active trasaction */



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    TXINFO info;
    
    printf("%s| starting...\n", pgm);
    printf("%s| tx_open(): %d\n", pgm, rc = tx_open());
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", pgm, rc = tx_begin());
    assert(TX_OK == rc);
    printf("%s| tx_info(): %d\n", pgm, rc = tx_info(&info));
    assert(1 == rc);

    /* emulate callback registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLExecDirect function call */
    lixa_monkeyrm_call_ax_reg(1);
    
    printf("%s| tx_close(): %d\n", pgm, rc = tx_close());
    assert(TX_PROTOCOL_ERROR == rc);
    printf("%s| ...finished\n", pgm);
    return 0;
}
