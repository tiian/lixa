/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <config.h>



#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif



#include <lixa_config.h>
#include <lixa_errors.h>
#include <lixa_trace.h>
#include <client_config.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_CONFIG



int client_config(struct client_config_s *cc,
                  const char *config_filename)
{
    enum Exception { OPEN_CONFIG_ERROR
                     , CLOSE_ERROR
                     , XML_READ_FILE_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , PARSE_CONFIG_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = 0;
    const char *file_name = NULL;
    xmlDocPtr doc;
    xmlNode *root_element = NULL;
    
    LIXA_TRACE(("client_config\n"));
    TRY {
        /* checking if available the custom config file */
        if (NULL != config_filename &&
            -1 != (fd = open(config_filename, O_RDONLY))) {
            file_name = config_filename;
        } else {
            if (-1 == (fd = open(LIXA_CLIENT_CONFIG_DEFAULT_FILE, O_RDONLY))) {
                LIXA_TRACE(("server_config/file %s is not readable, throwing "
                            "error\n", LIXA_CLIENT_CONFIG_DEFAULT_FILE));
                THROW(OPEN_CONFIG_ERROR);
            } else {
                file_name = LIXA_SERVER_CONFIG_DEFAULT_FILE;
            }
        }
        if (-1 == (ret_cod = close(fd)))
            THROW(CLOSE_ERROR);
        
        /* loading config file */
        if (NULL == (doc = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree */
        if (NULL == (root_element = xmlDocGetRootElement(doc)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_parse(cc, root_element)))
            THROW(PARSE_CONFIG_ERROR);
        
        /* free parsed document */
        xmlFreeDoc(doc);

        /* release libxml2 stuff */
        xmlCleanupParser();

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_CONFIG_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case XML_READ_FILE_ERROR:
                ret_cod = LIXA_RC_XML_READ_FILE_ERROR;
                break;
            case XML_DOC_GET_ROOT_ELEMENT_ERROR:
                ret_cod = LIXA_RC_XML_DOC_GET_ROOT_ELEMENT_ERROR;
                break;
            case PARSE_CONFIG_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse(struct client_config_s *cc,
                 xmlNode *a_node)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_parse\n"));
    TRY {
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

