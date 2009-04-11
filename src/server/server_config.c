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



#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <server_config.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_CONFIG



const char *LIXA_SERVER_CONFIG_DEFAULT_FILE = "/etc/lixad_conf.xml";

const xmlChar *LIXA_XML_CONFIG_LISTENER = (xmlChar *)"listener";
const xmlChar *LIXA_XML_CONFIG_LISTENER_DOMAIN = (xmlChar *)"domain";
const xmlChar *LIXA_XML_CONFIG_LISTENER_ADDRESS = (xmlChar *)"address";
const xmlChar *LIXA_XML_CONFIG_LISTENER_PORT = (xmlChar *)"port";

const xmlChar *LIXA_XML_CONFIG_LISTENER_DOMAIN_AF_INET = (xmlChar *)"AF_INET";



int server_config(struct server_config_s *sc,
                  const char *config_filename)
{
    enum Exception { OPEN_CONFIG_ERROR
                     , XML_READ_FILE_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , PARSE_CONFIG_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = 0;
    const char *file_name = NULL;
    xmlDocPtr doc;
    xmlNode *root_element = NULL;
    
    LIXA_TRACE(("server_config\n"));
    TRY {
        /* checking if available the custom config file */
        if (NULL != config_filename &&
            -1 != (fd = open(config_filename, O_RDONLY))) {
            file_name = config_filename;
        } else {
            if (-1 == (fd = open(LIXA_SERVER_CONFIG_DEFAULT_FILE, O_RDONLY))) {
                LIXA_TRACE(("server_config/file %s is not readable, throwing "
                            "error\n", LIXA_SERVER_CONFIG_DEFAULT_FILE));
                THROW(OPEN_CONFIG_ERROR);
            } else {
                file_name = LIXA_SERVER_CONFIG_DEFAULT_FILE;
            }
        }

        /* loading config file */
        if (NULL == (doc = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree */
        if (NULL == (root_element = xmlDocGetRootElement(doc)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK != (ret_cod = parse_config(sc, root_element)))
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
    LIXA_TRACE(("server_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int parse_config(struct server_config_s *sc,
                 xmlNode *a_node)
{
    enum Exception { PARSE_LISTENER_ERROR
                     , PARSE_CONFIG_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    xmlNode *cur_node = NULL;

    LIXA_TRACE(("parse_config\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("parse_config: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_LISTENER)) {
                    if (LIXA_RC_OK != (ret_cod = parse_config_listener(
                                           sc, cur_node)))
                        THROW(PARSE_LISTENER_ERROR);
                }
            }            
            if (LIXA_RC_OK != (ret_cod = parse_config(sc, cur_node->children)))
                THROW(PARSE_CONFIG_ERROR);
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_LISTENER_ERROR:
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
    LIXA_TRACE(("parse_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int parse_config_listener(struct server_config_s *sc,
                          xmlNode *a_node)
{
    enum Exception { REALLOC_ERROR
                     , DOMAIN_NOT_AVAILABLE_ERROR
                     , INVALID_DOMAIN_ERROR
                     , ADDRESS_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    xmlChar *attr = NULL;
    
    LIXA_TRACE(("parse_config_listener\n"));
    TRY {
        
        /* realloc array */
        if (NULL == (sc->listeners.array = realloc(
                         sc->listeners.array,
                         ++sc->listeners.n * sizeof(struct listener_config_s))))
            THROW(REALLOC_ERROR);
        i = sc->listeners.n - 1;

        /* reset new element */
        sc->listeners.array[i].domain = 0;
        sc->listeners.array[i].address = NULL;
        sc->listeners.array[i].port = 0;

        /* look/check/set listener domain */
        if (NULL == (attr = xmlGetProp(a_node,
                                       LIXA_XML_CONFIG_LISTENER_DOMAIN)))
            THROW(DOMAIN_NOT_AVAILABLE_ERROR);
        if (!xmlStrcmp(attr, LIXA_XML_CONFIG_LISTENER_DOMAIN_AF_INET)) {
            sc->listeners.array[i].domain = AF_INET;
        } else {
            LIXA_TRACE(("parse_config_listener: socket domain '%s' is not "
                        "valid\n", (char *)attr));
            THROW(INVALID_DOMAIN_ERROR);
        }
        xmlFree(attr);
        attr = NULL;
        
        if (NULL == (sc->listeners.array[i].address = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_LISTENER_ADDRESS)))
            THROW(ADDRESS_NOT_AVAILABLE_ERROR);
        if (NULL == (attr = xmlGetProp(a_node,
                                       LIXA_XML_CONFIG_LISTENER_PORT))) {
            THROW(PORT_NOT_AVAILABLE_ERROR);
        } else {
            sc->listeners.array[i].port = (in_port_t)strtoul(
                (char *)attr, NULL, 0);
            xmlFree(attr);
            attr = NULL;
        }
        
        LIXA_TRACE(("parse_config_listener: %s %d, %s = %d, "
                    "%s = '%s', %s = " IN_PORT_T_FORMAT  "\n",
                    (char *)LIXA_XML_CONFIG_LISTENER, i,
                    (char *)LIXA_XML_CONFIG_LISTENER_DOMAIN,
                    sc->listeners.array[i].domain,
                    (char *)LIXA_XML_CONFIG_LISTENER_ADDRESS,
                    sc->listeners.array[i].address,
                    (char *)LIXA_XML_CONFIG_LISTENER_PORT,
                    sc->listeners.array[i].port));
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case DOMAIN_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("parse_config_listener: unable to find listener "
                            "domain for listener %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case INVALID_DOMAIN_ERROR:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case ADDRESS_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("parse_config_listener: unable to find listener "
                            "ip_address for listener %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case PORT_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("parse_config_listener: unable to find listener "
                            "port for listener %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release memory */
        if (NULL != attr)
            xmlFree(attr);
        
    } /* TRY-CATCH */
    LIXA_TRACE(("parse_config_listener/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
void server_config_init(struct server_config_s *sc)
{
    LIXA_TRACE(("server_config_init/start\n"));
    sc->listeners.n = 0;
    sc->listeners.array = NULL;
    LIXA_TRACE(("server_config_init/end\n"));
}
