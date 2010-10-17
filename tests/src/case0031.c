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
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif



#include <tx.h>
#include <liblixamonkey.h>



#define THREAD_NUMBER 100



/* this case test is used for multithread tests */
struct thread_data_s {
    int commit;
    int expected_rc;
    const char *pgm;
};



void *transaction(void *parm);



int main(int argc, char *argv[])
{
    char *pgm = argv[0];
    int rc;
    int commit;
    int expected_rc;
    struct thread_data_s data;
    pthread_t tids[THREAD_NUMBER];
    int i,j,n;

    if (argc < 4) {
        fprintf(stderr, "%s: at least three options must be specified\n",
                argv[0]);
        exit (1);
    }
    if (!strcmp(argv[1], "commit"))
        commit = TRUE;
    else if (!strcmp(argv[1], "rollback"))
        commit = FALSE;
    else {
        fprintf(stderr, "%s: first option must be [commit|rollback]\n",
                argv[0]);
        exit (1);
    }
    expected_rc = (int)strtol(argv[2], NULL, 0);
    n = (int)strtol(argv[3], NULL, 0);
    if (n < 1)
        n = 1;
    else if (n > THREAD_NUMBER)
        n = THREAD_NUMBER;
    printf("%s| starting (%s/%d)...\n", pgm, argv[1], expected_rc);
    data.commit = commit;
    data.expected_rc = expected_rc;
    data.pgm = pgm;
    for (j=0; j<3; ++j) {
        for (i=0; i<n; ++i) {
            rc = pthread_create(tids+i, NULL, transaction, (void *)&data);
            assert(0 == rc);
        }
        for (i=0; i<n; ++i) {    
            rc = pthread_join(tids[i], NULL);
            assert(0 == rc);
        }
        printf("%s| sleeping...\n", pgm);
        sleep(1);
    }
    printf("%s| ...finished\n", pgm);
    return 0;
}



void *transaction(void *parm)
{
    int rc;
    TXINFO info;
    struct timeval to;
    struct thread_data_s *data = (struct thread_data_s *)parm;
    
    printf("%s| tx_open(): %d\n", data->pgm, rc = tx_open());
    assert(TX_OK == rc);
    printf("%s| tx_begin(): %d\n", data->pgm, rc = tx_begin());
    assert(TX_OK == rc);
    printf("%s| tx_info(): %d\n", data->pgm, rc = tx_info(&info));
    assert(1 == rc);

    /* emulate callback registration from resource manager when accessing
     * resource manager owned resources; you may imagine these are the
     * equivalent of a SQLExecDirect function call */
    lixa_monkeyrm_call_ax_reg(2);
    lixa_monkeyrm_call_ax_reg(3);
    
    /* wait 100 milliseconds */
    to.tv_sec = 0;
    to.tv_usec = 50000;
    select(0, NULL, NULL, NULL, &to);
    
    if (data->commit)
        printf("%s| tx_commit(): %d\n", data->pgm, rc = tx_commit());
    else
        printf("%s| tx_rollback(): %d\n", data->pgm, rc = tx_rollback());
    assert(data->expected_rc == rc);
    printf("%s| tx_close(): %d\n", data->pgm, rc = tx_close());
    if (TX_FAIL == data->expected_rc)
        assert(TX_FAIL == rc);
    else
        assert(TX_OK == rc);
    return NULL;
}
