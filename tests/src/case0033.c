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


/* this case test is related to a possible libxml2 memory leak */


#include <config.h>



#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif



char *XML_STRING="<?xml version=\"1.0\" encoding=\"UTF-8\" ?><foo></foo>";



int main(int argc, char *argv[])
{
    xmlDocPtr doc;
    
    xmlInitParser();
    doc = xmlReadMemory(XML_STRING, strlen(XML_STRING), "buffer.xml", NULL, 0);
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return 0;
}
