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
#ifndef CASE_TEST_FUNCTIONS_H
# define CASE_TEST_FUNCTIONS_H



#include <config.h>



#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


    /* returns 0 if OK, 1 in the event of errors */
    int msg_receive(const char *fifo_name, char *buffer, int size);

    /* returns 0 if OK, 1 in the event of errors */
    int msg_send(const char *fifo_name, const char *buffer);
    
    
    
#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /* CASE_TEST_FUNCTIONS_H */
