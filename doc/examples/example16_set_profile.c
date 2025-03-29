/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
 * Parts Copyright (c) 2021, Lawrence Wilkinson <lawrence.wilkinson@lzlabs.com>
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
/* and the tx_set_profile() function */



int main(int argc, char *argv[])
{
    int rc;
    const char *program_profile = "TEST_PROFILE";
    const char *env_profile = getenv("LIXA_PROFILE");

    printf("LIXA_PROFILE = %s\n", env_profile ? env_profile : "(Not specified)");

    printf("tx_set_profile(%s): %d\n", program_profile, rc = tx_set_profile( program_profile ));
    if (TX_OK != rc) exit(1);

    /* If LIXA_PROFILE is set, its value will override the tx_set_profile() */
    
    printf("tx_open(): %d\n", rc = tx_open());
    if (TX_OK != rc) exit(1);
    
    printf("tx_begin(): %d\n", rc = tx_begin());
    if (TX_OK != rc) exit(1);
    printf("tx_end(TMSUSPEND): %d\n", rc = tx_end(TX_TMSUSPEND));
    if (TX_OK != rc) exit(1);
    TXINFO txinfo;
    printf("tx_info(): %d\n", rc = tx_info(&txinfo));
    if(0 != rc && 1 != rc) exit(1);
    printf("tx_resume(): %d\n", rc = tx_resume(&txinfo.xid));
    if (TX_OK != rc) exit(1);
    printf("tx_commit(): %d\n", rc = tx_commit());
    if (TX_OK != rc) exit(1);

    printf("tx_begin(): %d\n", rc = tx_begin());
    if (TX_OK != rc) exit(1);
    printf("tx_end(TMSUSPEND): %d\n", rc = tx_end(TX_TMSUSPEND));
    if (TX_OK != rc) exit(1);
    printf("tx_info(): %d\n", rc = tx_info(&txinfo));
    if(0 != rc && 1 != rc) exit(1);
    printf("tx_resume(): %d\n", rc = tx_resume(&txinfo.xid));
    if (TX_OK != rc) exit(1);
    printf("tx_rollback(): %d\n", rc = tx_rollback());
    if (TX_OK != rc) exit(1);
    
    printf("tx_close(): %d\n", rc = tx_close());
    if (TX_OK != rc) exit(1);
    return 0;
}
