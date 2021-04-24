/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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



#include "case_test_functions.h"



int msg_receive(const char *fifo_name, char *buffer, int size)
{
    FILE *fifo_file = NULL;
    
    /* open the pipe for read operation */
    if (NULL == (fifo_file = fopen(fifo_name, "r"))) {
        fprintf(stderr, "msg_receive|error while opening file '%s'\n",
                fifo_name);
        return 1;
    }
    /* read from fifo_file the message */
    if (NULL == fgets(buffer, size, fifo_file)) {
        fprintf(stderr, "msg_receive|error while retrieving message from "
                "file '%s'\n", fifo_name);
        return 1;
    }
    /* close the pipe */
    fclose(fifo_file);
    
    return 0;
}
    


int msg_send(const char *fifo_name, const char *buffer)
{
    FILE *fifo_file = NULL;
    
    /* open the pipe for write operation */
    if (NULL == (fifo_file = fopen(fifo_name, "w"))) {
        fprintf(stderr, "msg_send|error while opening file '%s'\n", fifo_name);
        return 1;
    }
    /* write to fifo_file the message */
    if (0 > fprintf(fifo_file, "%s", buffer)) {
        fprintf(stderr, "msg_send|error while writing '%s' to file '%s'\n",
                buffer, fifo_name);
        return 1;
    }
    
    /* close the pipe */
    fclose(fifo_file);
    
    return 0;
}



