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
#include <stdio.h>
#include <stdlib.h>



#include <lixanonapi.h>



int main(int argc, char *argv[])
{
    int check_lixa_conn = 0;
    int check_lixa_rmid = 0;
    int check_lixa_id = 0;
    int use_lixa_conn = 0;
    int use_lixa_rmid = 0;
    int use_lixa_id = 0;
    int ret_cod;
    
    if (argc != 5) {
        fprintf(stderr, "Usage: string to be parsed, use_lixa_conn(0/1), "
                "use_lixa_rmid(0/1), use_lixa_id(id)\n");
        exit(1);
    }

    check_lixa_conn = (int)strtol(argv[2], NULL, 10);
    check_lixa_rmid = (int)strtol(argv[3], NULL, 10);
    check_lixa_id = (int)strtol(argv[4], NULL, 10);    
    printf("check_lixa_conn=%d, check_lixa_rmid=%d, check_lixa_id=%d\n",
           check_lixa_conn, check_lixa_rmid, check_lixa_id);

    if (0 != (ret_cod = lixa_nonapi_parse_conn_string(
                  argv[1], &use_lixa_conn, &use_lixa_rmid, &use_lixa_id))) {
        fprintf(stderr, "lixa_nonapi_parse_conn_string returned %d instead of "
                "0\n", ret_cod);
        exit(1);
    }

    if (check_lixa_conn != use_lixa_conn) {
        fprintf(stderr, "check_lixa_conn(%d) != use_lixa_conn(%d)\n",
                check_lixa_conn, use_lixa_conn);
        exit(2);
    }
    if (check_lixa_rmid != use_lixa_rmid) {
        fprintf(stderr, "check_lixa_rmid(%d) != use_lixa_rmid(%d)\n",
                check_lixa_rmid, use_lixa_rmid);
        exit(3);
    }
    if (check_lixa_id != use_lixa_id) {
        fprintf(stderr, "check_lixa_id(%d) != use_lixa_id(%d)\n",
                check_lixa_id, use_lixa_id);
        exit(4);
    }
    return 0;
}
