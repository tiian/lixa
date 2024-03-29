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


/* This case test is for tx_begin() */


int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc, test_rc, mode;
    
    if (argc < 3) {
        fprintf(stderr, "%s: at least two options must be specified\n",
                argv[0]);
        exit (1);
    }

    mode = strtol(argv[1], NULL, 0);
    test_rc = strtol(argv[2], NULL, 0);

    printf("%s| starting...\n", pgm);

    if (mode) {
        printf("%s| lixa_monkeyrm_call_ax_reg(%d): %d\n",
               pgm, 0, rc = lixa_monkeyrm_call_ax_reg(0));
        assert(test_rc == rc);
    } else {
        printf("%s| lixa_monkeyrm_call_ax_unreg(%d): %d\n",
               pgm, 0, rc = lixa_monkeyrm_call_ax_unreg(0));
        assert(test_rc == rc);
    }
    
    printf("%s| ...finished\n", pgm);
    return 0;
}
