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
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_config.h>
#include <server_config.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_CONFIG



int server_config(struct server_config_s *sc,
                  struct thread_pipe_array_s *tpa,
                  const char *config_filename)
{
    enum Exception { OPEN_CONFIG_ERROR
                     , CLOSE_ERROR
                     , REALLOC_ERROR
                     , PIPE_ERROR
                     , XML_READ_FILE_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , PARSE_CONFIG_ERROR
                     , ZERO_LISTENTERS
                     , ZERO_MANAGERS
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
        if (-1 == (ret_cod = close(fd)))
            THROW(CLOSE_ERROR);
        
        /* initialize the first fifo: it's for the listeners thread; it may
           be it works in a different position, but for the sake of simplicity
           it's better to use first thread for listeners */
        if (NULL == (tpa->array = realloc(
                         tpa->array, sizeof(struct thread_pipe_s))))
            THROW(REALLOC_ERROR);
        tpa->n++;
        if (0 != pipe(tpa->array[0].pipefd))
            THROW(PIPE_ERROR);
        LIXA_TRACE(("server_config: pipe for listener is [%d,%d]\n",
                    tpa->array[0].pipefd[0], tpa->array[0].pipefd[1]));
        
        /* loading config file */
        if (NULL == (doc = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree */
        if (NULL == (root_element = xmlDocGetRootElement(doc)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK != (ret_cod = server_parse(sc, tpa, root_element)))
            THROW(PARSE_CONFIG_ERROR);
        
        /* free parsed document */
        xmlFreeDoc(doc);

        /* release libxml2 stuff */
        xmlCleanupParser();

        if (sc->listeners.n == 0)
            THROW(ZERO_LISTENTERS);
        if (sc->managers.n == 0)
            THROW(ZERO_MANAGERS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OPEN_CONFIG_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case PIPE_ERROR:
                ret_cod = LIXA_RC_PIPE_ERROR;
                break;
            case XML_READ_FILE_ERROR:
                ret_cod = LIXA_RC_XML_READ_FILE_ERROR;
                break;
            case XML_DOC_GET_ROOT_ELEMENT_ERROR:
                ret_cod = LIXA_RC_XML_DOC_GET_ROOT_ELEMENT_ERROR;
                break;
            case PARSE_CONFIG_ERROR:
                break;
            case ZERO_LISTENTERS:
                LIXA_TRACE(("server_config: 0 listeners configured, this "
                            "server is useless...\n"));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case ZERO_MANAGERS:
                LIXA_TRACE(("server_config: 0 managers configured, this "
                            "server is useless...\n"));
                ret_cod = LIXA_RC_CONFIG_ERROR;
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



int server_parse(struct server_config_s *sc,
                 struct thread_pipe_array_s *tpa,
                 xmlNode *a_node)
{
    enum Exception { PARSE_LISTENER_ERROR
                     , PARSE_MANAGER_ERROR
                     , SERVER_PARSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    xmlNode *cur_node = NULL;

    LIXA_TRACE(("server_parse\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("server_parse: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_LISTENER)) {
                    if (LIXA_RC_OK != (ret_cod = server_parse_listener(
                                           sc, cur_node)))
                        THROW(PARSE_LISTENER_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_MANAGER)) {
                    if (LIXA_RC_OK != (ret_cod = server_parse_manager(
                                           sc, tpa, cur_node)))
                        THROW(PARSE_MANAGER_ERROR);
                }
            }
            if (LIXA_RC_OK != (ret_cod = server_parse(
                                   sc, tpa, cur_node->children)))
                THROW(SERVER_PARSE_ERROR);
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_LISTENER_ERROR:
            case PARSE_MANAGER_ERROR:
            case SERVER_PARSE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_parse/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int server_parse_listener(struct server_config_s *sc,
                          xmlNode *a_node)
{
    enum Exception { REALLOC_ERROR
                     , DOMAIN_NOT_AVAILABLE_ERROR
                     /*
                     , INVALID_DOMAIN_ERROR
                     */
                     , ADDRESS_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    xmlChar *attr = NULL;
    
    LIXA_TRACE(("server_parse_listener\n"));
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
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_domain(
                               a_node, &(sc->listeners.array[i].domain))))
            THROW(DOMAIN_NOT_AVAILABLE_ERROR);
        
        if (NULL == (sc->listeners.array[i].address = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_ADDRESS_PROPERTY)))
            THROW(ADDRESS_NOT_AVAILABLE_ERROR);
        /* retrieve port */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_port(
                               a_node, &(sc->listeners.array[i].port))))
            THROW(PORT_NOT_AVAILABLE_ERROR);
        
        LIXA_TRACE(("server_parse_listener: %s %d, %s = %d, "
                    "%s = '%s', %s = " IN_PORT_T_FORMAT  "\n",
                    (char *)LIXA_XML_CONFIG_LISTENER, i,
                    (char *)LIXA_XML_CONFIG_DOMAIN_PROPERTY,
                    sc->listeners.array[i].domain,
                    (char *)LIXA_XML_CONFIG_ADDRESS_PROPERTY,
                    sc->listeners.array[i].address,
                    (char *)LIXA_XML_CONFIG_PORT_PROPERTY,
                    sc->listeners.array[i].port));
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case DOMAIN_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("server_parse_listener: unable to find listener "
                            "domain for listener %d\n", i));
                break;
            case ADDRESS_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("server_parse_listener: unable to find listener "
                            "ip_address for listener %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case PORT_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("server_parse_listener: unable to find listener "
                            "port for listener %d\n", i));
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
    LIXA_TRACE(("server_parse_listener/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
int server_parse_manager(struct server_config_s *sc,
                         struct thread_pipe_array_s *tpa,
                         xmlNode *a_node)
{
    enum Exception { REALLOC_ERROR1
                     , STATUS_NOT_AVAILABLE_ERROR
                     , REALLOC_ERROR2
                     , PIPE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("server_parse_manager\n"));
    TRY {
        int i = 0, j = 0;

        /* realloc array */
        if (NULL == (sc->managers.array = realloc(
                         sc->managers.array,
                         ++sc->managers.n * sizeof(struct manager_config_s))))
            THROW(REALLOC_ERROR1);
        i = sc->managers.n - 1;

        /* reset new element */
        sc->managers.array[i].status_file = NULL;
        if (NULL == (sc->managers.array[i].status_file = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_MANAGER_STATUS)))
            THROW(STATUS_NOT_AVAILABLE_ERROR);
        LIXA_TRACE(("server_parse_manager: %s %d, %s = %s\n",
                    (char *)LIXA_XML_CONFIG_MANAGER, i,
                    (char *)LIXA_XML_CONFIG_MANAGER_STATUS,
                    sc->managers.array[i].status_file));

        /* [...]
         * check the status file is OK
         */
        
        /* initialize the fifo: it's for the this manager thread */
        if (NULL == (tpa->array = realloc
		     (tpa->array, ++tpa->n * sizeof(struct thread_pipe_s))))
            THROW(REALLOC_ERROR2);
        j = tpa->n - 1;
        if (0 != pipe(tpa->array[j].pipefd))
            THROW(PIPE_ERROR);
        LIXA_TRACE(("server_parse_manager: pipe %d for manager %d is [%d,%d]\n",
                    j, i, tpa->array[j].pipefd[0], tpa->array[j].pipefd[1]));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR1:
            case REALLOC_ERROR2:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case STATUS_NOT_AVAILABLE_ERROR:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case PIPE_ERROR:
                ret_cod = LIXA_RC_PIPE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("server_parse_manager/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


    
void server_config_init(struct server_config_s *sc,
                        struct thread_pipe_array_s *tpa)
{
    LIXA_TRACE(("server_config_init/start\n"));
    sc->listeners.n = 0;
    sc->listeners.array = NULL;
    sc->managers.n = 0;
    sc->managers.array = NULL;
    tpa->n = 0;
    tpa->array = NULL;
    LIXA_TRACE(("server_config_init/end\n"));
}
