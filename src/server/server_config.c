/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifdef HAVE_SYSLOG_H

# include <syslog.h>

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
#include <lixa_syslog.h>
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
    enum Exception
    {
        OPEN_CONFIG_ERROR,
        CLOSE_ERROR,
        REALLOC_ERROR,
        XML_READ_FILE_ERROR,
        XML_DOC_GET_ROOT_ELEMENT_ERROR,
        PARSE_CONFIG_ERROR,
        ZERO_LISTENTERS,
        ZERO_MANAGERS,
        NONE
    } excp;
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
            file_name = LIXA_SERVER_CONFIG_SYSTEM_FILE;
            if (-1 == (fd = open(file_name, O_RDONLY))) {
                LIXA_TRACE(("server_config/file %s is not readable, "
                            "throwing error\n", file_name));
                THROW(OPEN_CONFIG_ERROR);
            }
        }
        if (-1 == (ret_cod = close(fd))) THROW(CLOSE_ERROR);

        /* initialize the first fifo: it's for the listeners thread; it may
           be it works in a different position, but for the sake of simplicity
           it's better to use first thread for listeners */
        if (NULL == (tpa->array = realloc(
                         tpa->array, sizeof(struct thread_pipe_s)))) THROW(REALLOC_ERROR);
        tpa->n++;

        /* loading config file */
        LIXA_TRACE(("server_config/xmlReadFile\n"));
        if (NULL == (doc = xmlReadFile(file_name, NULL, 0))) THROW(
            XML_READ_FILE_ERROR);

        /* walking tree */
        if (NULL == (root_element = xmlDocGetRootElement(doc))) THROW(
            XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK !=
            (ret_cod = server_parse(sc, tpa, root_element))) THROW(
                PARSE_CONFIG_ERROR);

        /* free parsed document */
        LIXA_TRACE(("server_config/xmlFreeDoc\n"));
        xmlFreeDoc(doc);

        if (sc->listeners.n == 0) THROW(ZERO_LISTENTERS);
        if (sc->managers.n == 0) THROW(ZERO_MANAGERS);

        THROW(NONE);
    }
    CATCH
        {
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


int server_cleanup(struct server_config_s *sc,
                   struct thread_pipe_array_s *tpa,
                   struct thread_status_array_s *tsa,
                   srvr_rcvr_tbl_t *srt, server_trans_tbl_t *stt)
{
    enum Exception
    {
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_cleanup\n"));
    TRY {
        int i;

        xmlFree(sc->pid_file);
        sc->pid_file = NULL;
        for (i = 0; i < sc->listeners.n; ++i)
            xmlFree(sc->listeners.array[i].address);
        free(sc->listeners.array);
        sc->listeners.array = NULL;
        for (i = 0; i < sc->managers.n; ++i)
            xmlFree(sc->managers.array[i].status_file);
        free(sc->managers.array);
        sc->managers.array = NULL;

        /* release thread communication pipe array */
        if (NULL != tpa->array) {
            free(tpa->array);
            tpa->array = NULL;
            tpa->n = 0;
        }

        /* release thread status array */
        if (NULL != tsa->array) {
            free(tsa->array);
            tsa->array = NULL;
            tsa->n = 0;
        }

        /* release server recovery table mutex */
        srvr_rcvr_tbl_delete(srt);

        /* release server transaction table mutex */
        server_trans_tbl_delete(stt);

        /* release libxml2 stuff */
        LIXA_TRACE(("server_cleanup/xmlCleanupParser\n"));
        xmlCleanupParser();

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case NONE:
                    ret_cod = LIXA_RC_OK;
                    break;
                default:
                    ret_cod = LIXA_RC_INTERNAL_ERROR;
            } /* switch (excp) */
        } /* TRY-CATCH */
    LIXA_TRACE(("server_cleanup/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}


int server_parse(struct server_config_s *sc,
                 struct thread_pipe_array_s *tpa,
                 xmlNode *a_node)
{
    enum Exception
    {
        PID_FILE_ERROR,
        RETRIEVE_MIN_EST_ERROR,
        RETRIEVE_MAX_EST_ERROR,
        PARSE_LISTENER_ERROR,
        PARSE_MANAGER_ERROR,
        SERVER_PARSE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;

    LIXA_TRACE(("server_parse\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("server_parse: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_SERVER)) {
                    /* retrieve pid_file name */
                    if (NULL == (sc->pid_file = (char *) xmlGetProp(
                                     cur_node, LIXA_XML_CONFIG_SERVER_PID))) {
                        syslog(LOG_ERR, LIXA_SYSLOG_LXD016E,
                               LIXA_XML_CONFIG_SERVER_PID,
                               LIXA_XML_CONFIG_SERVER);
                        THROW(PID_FILE_ERROR);
                    } else {
                        LIXA_TRACE(("server_parse: pid file is '%s'\n",
                                    sc->pid_file));
                        syslog(LOG_INFO, LIXA_SYSLOG_LXD027I,
                               (const char *) LIXA_XML_CONFIG_SERVER_PID,
                               sc->pid_file);
                    }
                    /* retrieve min_elapsed_sync_time */
                    if (LIXA_RC_OK == (
                            ret_cod = lixa_config_retrieve_generic_long(
                                cur_node, LIXA_XML_CONFIG_SERVER_MIN_EST,
                                &(sc->min_elapsed_sync_time)))) {
                        LIXA_TRACE(("server_parse: parameter '%s' is %ld\n",
                                    (const char *)
                                    LIXA_XML_CONFIG_SERVER_MIN_EST,
                                    sc->min_elapsed_sync_time));
                        /* check and fix the parameter if wrong */
                        if (sc->min_elapsed_sync_time < 0) {
                            syslog(LOG_NOTICE, LIXA_SYSLOG_LXD028N,
                                   sc->min_elapsed_sync_time,
                                   (const char *) LIXA_XML_CONFIG_SERVER_MIN_EST,
                                   (long) 0);
                            sc->min_elapsed_sync_time = 0;
                        }
                        syslog(LOG_INFO, LIXA_SYSLOG_LXD026I,
                               (const char *) LIXA_XML_CONFIG_SERVER_MIN_EST,
                               sc->min_elapsed_sync_time);
                    } else if (LIXA_RC_OBJ_NOT_FOUND != ret_cod) THROW(
                        RETRIEVE_MIN_EST_ERROR);
                    /* retrieve max_elapsed_sync_time */
                    if (LIXA_RC_OK == (
                            ret_cod = lixa_config_retrieve_generic_long(
                                cur_node, LIXA_XML_CONFIG_SERVER_MAX_EST,
                                &(sc->max_elapsed_sync_time)))) {
                        LIXA_TRACE(("server_parse: parameter '%s' is %ld\n",
                                    (const char *)
                                    LIXA_XML_CONFIG_SERVER_MAX_EST,
                                    sc->max_elapsed_sync_time));
                        /* check and fix the parameter if wrong */
                        if (sc->max_elapsed_sync_time < 0 ||
                            sc->max_elapsed_sync_time <
                            sc->min_elapsed_sync_time) {
                            syslog(LOG_NOTICE, LIXA_SYSLOG_LXD028N,
                                   sc->max_elapsed_sync_time,
                                   (const char *) LIXA_XML_CONFIG_SERVER_MIN_EST,
                                   sc->min_elapsed_sync_time);
                            sc->max_elapsed_sync_time =
                                sc->min_elapsed_sync_time;
                        }
                        syslog(LOG_INFO, LIXA_SYSLOG_LXD026I,
                               (const char *) LIXA_XML_CONFIG_SERVER_MAX_EST,
                               sc->max_elapsed_sync_time);
                    } else if (LIXA_RC_OBJ_NOT_FOUND != ret_cod) THROW(
                        RETRIEVE_MAX_EST_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_LISTENER)) {
                    if (LIXA_RC_OK != (ret_cod = server_parse_listener(
                                           sc, cur_node))) THROW(PARSE_LISTENER_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_MANAGER)) {
                    if (LIXA_RC_OK != (ret_cod = server_parse_manager(
                                           sc, tpa, cur_node))) THROW(PARSE_MANAGER_ERROR);
                }
            }
            if (LIXA_RC_OK != (ret_cod = server_parse(
                                   sc, tpa, cur_node->children))) THROW(SERVER_PARSE_ERROR);
        }

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case PID_FILE_ERROR:
                case RETRIEVE_MIN_EST_ERROR:
                case RETRIEVE_MAX_EST_ERROR:
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
    enum Exception
    {
        REALLOC_ERROR, DOMAIN_NOT_AVAILABLE_ERROR
        /*
          , INVALID_DOMAIN_ERROR
        */
        , ADDRESS_NOT_AVAILABLE_ERROR, PORT_NOT_AVAILABLE_ERROR, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    xmlChar *attr = NULL;

    LIXA_TRACE(("server_parse_listener\n"));
    TRY {

        /* realloc array */
        if (NULL == (sc->listeners.array = realloc(
                         sc->listeners.array,
                         ++sc->listeners.n *
                         sizeof(struct listener_config_s)))) THROW(REALLOC_ERROR);
        i = sc->listeners.n - 1;

        /* reset new element */
        sc->listeners.array[i].domain = 0;
        sc->listeners.array[i].address = NULL;
        sc->listeners.array[i].port = 0;

        /* look/check/set listener domain */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_domain(
                               a_node, &(sc->listeners.array[i].domain)))) THROW(
                                   DOMAIN_NOT_AVAILABLE_ERROR);

        if (NULL == (sc->listeners.array[i].address = (char *) xmlGetProp(
                         a_node, LIXA_XML_CONFIG_ADDRESS_PROPERTY))) THROW(
                             ADDRESS_NOT_AVAILABLE_ERROR);
        /* retrieve port */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_port(
                               a_node, &(sc->listeners.array[i].port)))) THROW(
                                   PORT_NOT_AVAILABLE_ERROR);

        LIXA_TRACE(("server_parse_listener: %s %d, %s = %d, "
                    "%s = '%s', %s = "
                    IN_PORT_T_FORMAT
                    "\n",
                    (char *) LIXA_XML_CONFIG_LISTENER, i,
                    (char *) LIXA_XML_CONFIG_DOMAIN_PROPERTY,
                    sc->listeners.array[i].domain,
                    (char *) LIXA_XML_CONFIG_ADDRESS_PROPERTY,
                    sc->listeners.array[i].address,
                    (char *) LIXA_XML_CONFIG_PORT_PROPERTY,
                    sc->listeners.array[i].port));
        THROW(NONE);
    }
    CATCH
        {
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
    enum Exception
    {
        REALLOC_ERROR1, STATUS_NOT_AVAILABLE_ERROR, REALLOC_ERROR2, NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("server_parse_manager\n"));
    TRY {
        int i = 0;

        /* realloc array */
        if (NULL == (sc->managers.array = realloc(
                         sc->managers.array,
                         ++sc->managers.n * sizeof(struct manager_config_s)))) THROW(
                             REALLOC_ERROR1);
        i = sc->managers.n - 1;

        /* reset new element */
        sc->managers.array[i].status_file = NULL;
        if (NULL == (sc->managers.array[i].status_file = (char *) xmlGetProp(
                         a_node, LIXA_XML_CONFIG_MANAGER_STATUS))) THROW(
                             STATUS_NOT_AVAILABLE_ERROR);
        LIXA_TRACE(("server_parse_manager: %s %d, %s = %s\n",
                    (char *) LIXA_XML_CONFIG_MANAGER, i,
                    (char *) LIXA_XML_CONFIG_MANAGER_STATUS,
                    sc->managers.array[i].status_file));

        /* [...]
         * check the status file is OK
         */

        /* initialize the fifo: it's for the this manager thread */
        if (NULL == (tpa->array = realloc
                     (tpa->array, ++tpa->n * sizeof(struct thread_pipe_s)))) THROW(
                         REALLOC_ERROR2);

        THROW(NONE);
    }
    CATCH
        {
            switch (excp) {
                case REALLOC_ERROR1:
                case REALLOC_ERROR2:
                    ret_cod = LIXA_RC_REALLOC_ERROR;
                    break;
                case STATUS_NOT_AVAILABLE_ERROR:
                    ret_cod = LIXA_RC_CONFIG_ERROR;
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
    sc->pid_file = NULL;
    sc->min_elapsed_sync_time = sc->max_elapsed_sync_time = 0;
    sc->listeners.n = 0;
    sc->listeners.array = NULL;
    sc->managers.n = 0;
    sc->managers.array = NULL;
    tpa->n = 0;
    tpa->array = NULL;
    LIXA_TRACE(("server_config_init/end\n"));
}
