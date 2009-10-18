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



#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
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
#include <client_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_CONFIG



int client_config_coll_init(client_config_coll_t *ccc)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_coll_init\n"));
    TRY {
        LIXA_TRACE(("client_config_coll_init: initializing sequentialization "
                    "mutex\n"));
        ret_cod = pthread_mutex_init(&(ccc->mutex), NULL);
        LIXA_TRACE(("client_config_coll_init: mutex initialization return "
                    "code: %d\n", ret_cod));
        ccc->configured = FALSE;
        ccc->profile = NULL;
        memset(&ccc->serv_addr, 0, sizeof(struct sockaddr_in));
        ccc->trnmgrs.n = 0;
        ccc->trnmgrs.array = NULL;
        ccc->rsrmgrs.n = 0;
        ccc->rsrmgrs.array = NULL;
     
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
    LIXA_TRACE(("client_config_coll_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



/* must be reviewed!!!
int client_config_coll_get_trnmgr(const client_config_coll_t *ccc,
                                  struct trnmgr_config_s **tc)
{
    enum Exception { OBJ_NOT_FOUND
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_coll_get_trnmgr\n"));
    TRY {
        int i;
        *tc = NULL;
        for (i = 0; i < ccc->trnmgrs.n; ++i) {
            if (0 == strcmp(ccc->profiles.array[i].name,
                            ccc->profile) ||
                0 == strlen(ccc->profile)) {
                LIXA_TRACE(("client_config_coll_get_profile: profile '%s' "
                            "matches with profile # %d\n",
                            ccc->profile, i));
                break;
            }
        }
        if (i == ccc->trnmgrs.n)
            THROW(OBJ_NOT_FOUND);
        *tc = &(ccc->trnmgrs.array[i]);
            
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OBJ_NOT_FOUND:
                LIXA_TRACE(("client_config_coll_get_profile: profile '%s' "
                            "does not match any transaction manager\n",
                            ccc->profile));
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        }
    } 
    LIXA_TRACE(("client_config_coll_get_profile/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
*/

    
int client_config(client_config_coll_t *ccc)
{
    enum Exception { PTHREAD_MUTEX_LOCK_ERROR
                     , ALREADY_CONFIGURED
                     , STRDUP_ERROR
                     , OPEN_CONFIG_ERROR
                     , CLOSE_ERROR
                     , XML_READ_FILE_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , PARSE_CONFIG_ERROR
                     , GET_TRNMGR_ERROR
                     , GETADDRINFO_ERROR
                     , PTHREAD_MUTEX_UNLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = 0;
    const char *file_name = NULL;
    xmlDocPtr doc = NULL;
    xmlNode *root_element = NULL;
    struct trnmgr_config_s *tc;
    struct addrinfo hints, *res;
    
    LIXA_TRACE(("client_config\n"));
    TRY {
        const char *tmp_str;
        /* lock mutex to start configuration activity */
        if (0 != (ret_cod = pthread_mutex_lock(&ccc->mutex)))
            THROW(PTHREAD_MUTEX_LOCK_ERROR);

        if (ccc->configured) {
            LIXA_TRACE(("client_config: already configured, skipping...\n"));
            THROW(ALREADY_CONFIGURED);
        }
        
        if (NULL == (tmp_str = getenv(LIXA_PROFILE_ENV_VAR))) {
            /* use empty string instead of NULL to avoid allocation issues */
            tmp_str = "";
            LIXA_TRACE(("client_config: '%s' environment variable not found, "
                        "using default profile for this client\n",
                        LIXA_PROFILE_ENV_VAR));
        }
        LIXA_TRACE(("client_config: using transactional profile '%s' for "
                    "subsequent operations\n", tmp_str));        

        if (NULL == (ccc->profile = strdup(tmp_str)))
            THROW(STRDUP_ERROR);

        /* checking if available the custom config file */
        tmp_str = getenv(LIXA_CONFIG_FILE_ENV_VAR);
        if (NULL != tmp_str && -1 != (fd = open(tmp_str, O_RDONLY))) {
            file_name = tmp_str;
        } else {
            tmp_str = LIXA_CLIENT_CONFIG_DEFAULT_FILE;
            LIXA_TRACE(("client_config: '%s' environment variable not found, "
                        "using default config file '%s' for this client\n",
                        LIXA_CONFIG_FILE_ENV_VAR, tmp_str));
            if (-1 == (fd = open(tmp_str, O_RDONLY))) {
                LIXA_TRACE(("client_config: file %s is not readable, throwing "
                            "error\n", LIXA_CLIENT_CONFIG_DEFAULT_FILE));
                THROW(OPEN_CONFIG_ERROR);
            } else {
                file_name = tmp_str;
            }
        }
        if (-1 == (ret_cod = close(fd)))
            THROW(CLOSE_ERROR);
        
        /* loading config file */
        if (NULL == (doc = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree & parse */
        if (NULL == (root_element = xmlDocGetRootElement(doc)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_parse(ccc, root_element)))
            THROW(PARSE_CONFIG_ERROR);

        /* free parsed document */
        xmlFreeDoc(doc);
        doc = NULL;
        
        /* release libxml2 stuff */
        xmlCleanupParser();

        /* search connection parameters */
        if (LIXA_RC_OK != (ret_cod = client_config_coll_get_trnmgr(ccc, &tc)))
            THROW(GET_TRNMGR_ERROR);

        /* resolve address */
        LIXA_TRACE(("client_config: resolving address for '%s'\n",
                    tc->address));
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_flags = AI_CANONNAME;
        hints.ai_family = tc->domain;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_protocol = IPPROTO_TCP;
        
        if (0 != getaddrinfo(tc->address, NULL, &hints, &res))
            THROW(GETADDRINFO_ERROR);
        /* set port */
        memcpy(&ccc->serv_addr, (struct sockaddr_in *)res->ai_addr,
               sizeof(struct sockaddr_in));
        ccc->serv_addr.sin_port = htons(tc->port);
        
        /* now the client is CONFIGURED */
        ccc->configured = TRUE;

        /* unlock mutex to start configuration activity */
        if (0 != (ret_cod = pthread_mutex_unlock(&ccc->mutex)))
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PTHREAD_MUTEX_LOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_LOCK_ERROR;
                break;
            case ALREADY_CONFIGURED:
                ret_cod = LIXA_RC_OK;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
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
            case GET_TRNMGR_ERROR:
                break;
            case GETADDRINFO_ERROR:
                ret_cod = LIXA_RC_GETADDRINFO_ERROR;
                break;
            case PTHREAD_MUTEX_UNLOCK_ERROR:
                ret_cod = LIXA_RC_PTHREAD_MUTEX_UNLOCK_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        if (NONE != excp)
            LIXA_TRACE(("client_config: values before recovery "
                        "actions excp=%d/ret_cod=%d/errno=%d\n",
                        excp, ret_cod, errno));
        if (excp > PTHREAD_MUTEX_LOCK_ERROR &&
            excp < PTHREAD_MUTEX_UNLOCK_ERROR) {
            if (0 != pthread_mutex_unlock(&ccc->mutex))
                LIXA_TRACE(("client_config/pthread_mutex_unlock: "
                            "errno=%d\n", errno));
        }
        if (excp < NONE && doc != NULL) {
            /* free parsed document */
            xmlFreeDoc(doc);
            /* release libxml2 stuff */
            xmlCleanupParser();
        }
        /* free memory allocated by getadrinfo function */
        if (excp > GETADDRINFO_ERROR)
            freeaddrinfo(res);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse(struct client_config_coll_s *ccc,
                 xmlNode *a_node)
{
    enum Exception { CLIENT_PARSE_ERROR
                     , PARSE_TRNMGRS_ERROR
                     , PARSE_RSRMGRS_ERROR
                     , PARSE_PROFILES_ERROR
                     , UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_CLIENT)) {
                    /* parse child */
                    if (LIXA_RC_OK != (ret_cod = client_parse(
                                           ccc, cur_node->children)))
                        THROW(CLIENT_PARSE_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_TRNMGRS)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_trnmgrs(
                                           ccc, cur_node->children)))
                        THROW(PARSE_TRNMGRS_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_RSRMGRS)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_rsrmgrs(
                                           ccc, cur_node->children)))
                        THROW(PARSE_RSRMGRS_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_PROFILES)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_profiles(
                                           ccc, cur_node->children)))
                        THROW(PARSE_PROFILES_ERROR);
                } else
                    THROW(UNRECOGNIZED_TAG);
            }
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_PARSE_ERROR:
            case PARSE_TRNMGRS_ERROR:
            case PARSE_RSRMGRS_ERROR:
            case PARSE_PROFILES_ERROR:
                break;
            case UNRECOGNIZED_TAG:
                ret_cod = LIXA_RC_XML_UNRECOGNIZED_TAG;
                break;
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



int client_parse_trnmgrs(struct client_config_coll_s *ccc,
                         xmlNode *a_node)
{
    enum Exception { PARSE_TRNMGR_ERROR
                     , CLIENT_PARSE_TRNMGRS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_trnmgrs\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_trnmgrs: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_TRNMGR)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_trnmgr(
                                           ccc, cur_node)))
                        THROW(PARSE_TRNMGR_ERROR);
                }
            }
            if (LIXA_RC_OK != (ret_cod = client_parse_trnmgrs(
                                   ccc, cur_node->children)))
                THROW(CLIENT_PARSE_TRNMGRS_ERROR);
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_TRNMGR_ERROR:
            case CLIENT_PARSE_TRNMGRS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_trnmgrs/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_trnmgr(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { REALLOC_ERROR
                     , DOMAIN_NOT_AVAILABLE_ERROR
                     , ADDRESS_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    
    LIXA_TRACE(("client_parse_trnmgr\n"));
    TRY {
        /* expand array */
        if (NULL == (ccc->trnmgrs.array = realloc(
                         ccc->trnmgrs.array,
                         ++ccc->trnmgrs.n * sizeof(struct trnmgr_config_s))))
            THROW(REALLOC_ERROR);
        i = ccc->trnmgrs.n - 1;

        /* reset new element */
        ccc->trnmgrs.array[i].domain = 0;
        ccc->trnmgrs.array[i].address = NULL;
        ccc->trnmgrs.array[i].port = 0;

        /* look/check/set listener domain */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_domain(
                               a_node, &(ccc->trnmgrs.array[i].domain))))
            THROW(DOMAIN_NOT_AVAILABLE_ERROR);
        /* retrieve address */
        if (NULL == (ccc->trnmgrs.array[i].address = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_ADDRESS_PROPERTY)))
            THROW(ADDRESS_NOT_AVAILABLE_ERROR);
        /* retrieve port */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_port(
                               a_node, &(ccc->trnmgrs.array[i].port))))
            THROW(PORT_NOT_AVAILABLE_ERROR);

        LIXA_TRACE(("client_parse_trnmgr: %s %d, %s = %d, "
                    "%s = '%s', %s = " IN_PORT_T_FORMAT  "\n",
                    (char *)LIXA_XML_CONFIG_TRNMGR, i,
                    (char *)LIXA_XML_CONFIG_DOMAIN_PROPERTY,
                    ccc->trnmgrs.array[i].domain,
                    (char *)LIXA_XML_CONFIG_ADDRESS_PROPERTY,
                    ccc->trnmgrs.array[i].address,
                    (char *)LIXA_XML_CONFIG_PORT_PROPERTY,
                    ccc->trnmgrs.array[i].port));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case DOMAIN_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_trnmgr: unable to find trnmgr "
                            "domain for trnmgr %d\n", i));
                break;
            case ADDRESS_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_trnmgr: unable to find trnmgr "
                            "ip_address for trnmgr %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case PORT_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_trnmgr: unable to find trnmgr "
                            "port for trnmgr %d\n", i));
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_trnmgr/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_rsrmgrs(struct client_config_coll_s *ccc,
                         xmlNode *a_node)
{
    enum Exception { PARSE_RSRMGR_ERROR
                     , CLIENT_PARSE_RSRMGRS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_rsrmgrs\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_rsrmgrs: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_RSRMGR)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_rsrmgr(
                                           ccc, cur_node)))
                        THROW(PARSE_RSRMGR_ERROR);
                }
            }
            if (LIXA_RC_OK != (ret_cod = client_parse_rsrmgrs(
                                   ccc, cur_node->children)))
                THROW(CLIENT_PARSE_RSRMGRS_ERROR);
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_RSRMGR_ERROR:
            case CLIENT_PARSE_RSRMGRS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_rsrmgrs/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_rsrmgr(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { REALLOC_ERROR
                     , NAME_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , SWITCH_FILE_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    
    LIXA_TRACE(("client_parse_rsrmgr\n"));
    TRY {
        /* expand array */
        if (NULL == (ccc->rsrmgrs.array = realloc(
                         ccc->rsrmgrs.array,
                         ++ccc->rsrmgrs.n * sizeof(struct rsrmgr_config_s))))
            THROW(REALLOC_ERROR);
        i = ccc->rsrmgrs.n - 1;

        /* reset new element */
        ccc->rsrmgrs.array[i].name = NULL;
        ccc->rsrmgrs.array[i].switch_file = NULL;

        /* retrieve name */
        if (NULL == (ccc->rsrmgrs.array[i].name = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);
        /* retrieve switch_file */
        if (NULL == (ccc->rsrmgrs.array[i].switch_file = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY)))
            THROW(SWITCH_FILE_NOT_AVAILABLE_ERROR);

        LIXA_TRACE(("client_parse_rsrmgr: %s %d, "
                    "%s = '%s', %s = '%s'\n",
                    (char *)LIXA_XML_CONFIG_RSRMGR, i,
                    (char *)LIXA_XML_CONFIG_NAME_PROPERTY,
                    ccc->rsrmgrs.array[i].name,
                    (char *)LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY,
                    ccc->rsrmgrs.array[i].switch_file));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case NAME_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_rsrmgr: unable to find rsrmgr "
                            "name for rsrmgr %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case SWITCH_FILE_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_rsrmgr: unable to find rsrmgr "
                            "switch_file for rsrmgr %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_rsrmgr/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_profiles(struct client_config_coll_s *ccc,
                         xmlNode *a_node)
{
    enum Exception { PARSE_PROFILE_ERROR
                     , CLIENT_PARSE_PROFILES_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_profiles\n"));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_profiles: tag %s\n", cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_PROFILE)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_profile(
                                           ccc, cur_node)))
                        THROW(PARSE_PROFILE_ERROR);
                }
            }
            if (LIXA_RC_OK != (ret_cod = client_parse_profiles(
                                   ccc, cur_node->children)))
                THROW(CLIENT_PARSE_PROFILES_ERROR);
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_PROFILE_ERROR:
            case CLIENT_PARSE_PROFILES_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_profiles/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_profile(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { REALLOC_ERROR
                     , NAME_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , SWITCH_FILE_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    
    LIXA_TRACE(("client_parse_profile\n"));
    TRY {
        /* expand array */
        if (NULL == (ccc->profiles.array = realloc(
                         ccc->profiles.array,
                         ++ccc->profiles.n * sizeof(struct profile_config_s))))
            THROW(REALLOC_ERROR);
        i = ccc->profiles.n - 1;

        /* reset new element */
        ccc->profiles.array[i].name = NULL;

        /* retrieve name */
        if (NULL == (ccc->profiles.array[i].name = (char *)xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);

        LIXA_TRACE(("client_parse_profile: %s %d, "
                    "%s = '%s'\n",
                    (char *)LIXA_XML_CONFIG_PROFILE, i,
                    (char *)LIXA_XML_CONFIG_NAME_PROPERTY,
                    ccc->profiles.array[i].name));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case NAME_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_profile: unable to find profile "
                            "name for profile %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_profile/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



