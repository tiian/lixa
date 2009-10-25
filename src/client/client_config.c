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
#include <xa.h>



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
        ccc->actconf.trnmgr = NULL;
        ccc->actconf.rsrmgrs = g_array_new(
            FALSE, FALSE, sizeof(struct act_rsrmgr_config_s));
        ccc->trnmgrs = g_array_new(FALSE, FALSE, sizeof(
                                       struct trnmgr_config_s));
        ccc->rsrmgrs = g_array_new(FALSE, FALSE, sizeof(
                                       struct rsrmgr_config_s));
        ccc->profiles = g_array_new(FALSE, FALSE, sizeof(
                                        struct profile_config_s));
     
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
                     , CLIENT_CONFIG_DISPLAY_ERROR
                     , CLIENT_CONFIG_VALIDATE_ERROR
                     , GETADDRINFO_ERROR
                     , CLIENT_CONFIG_LOAD_SWITCH_ERROR
                     , PTHREAD_MUTEX_UNLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = 0;
    const char *file_name = NULL;
    xmlNode *root_element = NULL;
    struct addrinfo hints, *res;

    ccc->lixac_conf = NULL;
    
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
        if (NULL == (ccc->lixac_conf = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree & parse */
        if (NULL == (root_element = xmlDocGetRootElement(ccc->lixac_conf)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_parse(ccc, root_element)))
            THROW(PARSE_CONFIG_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_config_display(ccc)))
            THROW(CLIENT_CONFIG_DISPLAY_ERROR);
        
        if (LIXA_RC_OK != (ret_cod = client_config_validate(ccc)))
            THROW(CLIENT_CONFIG_VALIDATE_ERROR);

        /* free parsed document */
        xmlFreeDoc(ccc->lixac_conf);
        ccc->lixac_conf = NULL;
        
        /* release libxml2 stuff */
        xmlCleanupParser();

        /* resolve address */
        LIXA_TRACE(("client_config: resolving address for '%s'\n",
                    ccc->actconf.trnmgr->address));
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_flags = AI_CANONNAME;
        hints.ai_family = ccc->actconf.trnmgr->domain;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_protocol = IPPROTO_TCP;
        
        if (0 != getaddrinfo((char *)ccc->actconf.trnmgr->address, NULL,
                             &hints, &res))
            THROW(GETADDRINFO_ERROR);
        /* set port */
        memcpy(&ccc->serv_addr, (struct sockaddr_in *)res->ai_addr,
               sizeof(struct sockaddr_in));
        ccc->serv_addr.sin_port = htons(ccc->actconf.trnmgr->port);

        if (LIXA_RC_OK != (ret_cod = client_config_load_switch(ccc)))
            THROW(CLIENT_CONFIG_LOAD_SWITCH_ERROR);
        
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
            case CLIENT_CONFIG_DISPLAY_ERROR:
            case CLIENT_CONFIG_VALIDATE_ERROR:
                break;
            case GETADDRINFO_ERROR:
                ret_cod = LIXA_RC_GETADDRINFO_ERROR;
                break;
            case CLIENT_CONFIG_LOAD_SWITCH_ERROR:
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
        if (excp < NONE && ccc->lixac_conf != NULL) {
            /* free parsed document */
            xmlFreeDoc(ccc->lixac_conf);
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



int client_config_validate(client_config_coll_t *ccc)
{
    enum Exception { TRNMGR_NOT_DEFINED
                     , TRNMGR_NOT_FOUND
                     , RSRMGR_NOT_FOUND
                     , PROFILE_NOT_FOUND
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_validate\n"));
    TRY {
        guint i, j, k;

        /* look-up profile */
        for (i=0; i<ccc->profiles->len; ++i) {
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i);
            xmlChar *prof_trnmgr;
            if (0 == strlen(ccc->profile) ||
                0 == xmlStrcmp(profile->name,
                               (const xmlChar *)ccc->profile)) {
                LIXA_TRACE(("client_config_validate: profile '%s' "
                            "matches with profile # %u ('%s')\n",
                            ccc->profile, i, profile->name));
                /* pick-up the (list of) transaction manager(s) */
                if (profile->trnmgrs->len > 1)
                    LIXA_TRACE(("client_config_validate: profile '%s' "
                                "specifies more than one transaction manager "
                                "(%u) but only one is supported in the "
                                "current version\n",
                                profile->name, profile->trnmgrs->len));
                else if (profile->trnmgrs->len == 0) {
                    LIXA_TRACE(("client_config_validate: profile '%s' "
                                "does not specify any transaction manager\n",
                                profile->name));
                    THROW(TRNMGR_NOT_DEFINED);
                }
                prof_trnmgr = g_array_index(profile->trnmgrs, xmlChar *, 0);
                LIXA_TRACE(("client_config_validate: associated "
                            "transaction manager name is '%s'\n",
                            prof_trnmgr));
                /* look-up transaction manager */
                for (j=0; j<ccc->trnmgrs->len; ++j) {
                    struct trnmgr_config_s *trnmgr = &g_array_index(
                        ccc->trnmgrs, struct trnmgr_config_s, j);
                    if (0 == xmlStrcmp(prof_trnmgr, trnmgr->name)) {
                        LIXA_TRACE(("client_config_validate: "
                                    "transaction manager '%s' found at "
                                    "position # %u\n",
                                    prof_trnmgr, j));
                        ccc->actconf.trnmgr = trnmgr;
                        break;
                    }
                }
                if (j == ccc->trnmgrs->len) {
                    LIXA_TRACE(("client_config_validate: "
                                "transaction manager '%s' not found\n",
                                prof_trnmgr));
                    THROW(TRNMGR_NOT_FOUND);
                }
                /* scan the resource managers defined for the profile */
                for (j=0; j<profile->rsrmgrs->len; ++j) {
                    xmlChar *rsrmgr = g_array_index(
                        profile->rsrmgrs, xmlChar *, j);
                    LIXA_TRACE(("client_config_validate: this profile "
                                "requires resource manager '%s' at pos %u\n",
                                rsrmgr, j));
                    for (k=0; k<ccc->rsrmgrs->len; ++k) {
                        struct rsrmgr_config_s *conf_rsrmgr = &g_array_index(
                            ccc->rsrmgrs, struct rsrmgr_config_s, k);
                        if (0 == xmlStrcmp(rsrmgr, conf_rsrmgr->name)) {
                            struct act_rsrmgr_config_s record;
                            record.generic = conf_rsrmgr;
                            record.module = NULL;
                            record.xa_switch = NULL;
                            g_array_append_val(ccc->actconf.rsrmgrs,
                                               record);
                            LIXA_TRACE(("client_config_validate: resource "
                                        "manager '%s' found at pos %u in "
                                        "global list\n",
                                        conf_rsrmgr->name, k));
                            break;
                        }
                    }
                    if (k == ccc->rsrmgrs->len) {
                        LIXA_TRACE(("client_config_validate: resource manager "
                                    "'%s' was not previously defined and can "
                                    "not be associated to a profile\n",
                                    rsrmgr));
                        THROW(RSRMGR_NOT_FOUND);
                    }
                }
                break;
            }
        }
        if (i == ccc->profiles->len) {
            LIXA_TRACE(("client_config_validate: profile '%s' "
                        "does not match any configured profiles\n",
                        ccc->profile));
            THROW(PROFILE_NOT_FOUND);
        }
                
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case TRNMGR_NOT_DEFINED:
            case TRNMGR_NOT_FOUND:
            case RSRMGR_NOT_FOUND:
            case PROFILE_NOT_FOUND:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_validate/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_config_load_switch(const client_config_coll_t *ccc)   
{
    enum Exception { G_MODULE_OPEN_ERROR
                     , G_MODULE_SYMBOL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_load_switch\n"));
    TRY {
        GModule *module;
        lixa_get_xa_switch_f xa_switch;
        guint i;

        /* scan all the resource manager of the actual config */
        for (i=0; i<ccc->actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                ccc->actconf.rsrmgrs, struct act_rsrmgr_config_s, i);

            LIXA_TRACE(("client_config_load_switch: resource manager # %u, "
                        "name='%s', switch_file='%s'\n", i,
                        act_rsrmgr->generic->name,
                        act_rsrmgr->generic->switch_file));
            if (NULL == (module = g_module_open(
                             (gchar *)act_rsrmgr->generic->switch_file,
                             G_MODULE_BIND_LAZY))) {
                LIXA_TRACE(("client_config_load_switch: switch_file='%s', "
                            "g_module_error='%s'\n",
                            act_rsrmgr->generic->switch_file,
                            g_module_error()));
                THROW(G_MODULE_OPEN_ERROR);
            }
            if (!g_module_symbol(module, "lixa_get_xa_switch",
                                 (gpointer *)&xa_switch)) {
                LIXA_TRACE(("client_config_load_switch: symbol='%s', "
                            "g_module_error='%s'\n", "lixa_get_xa_switch",
                            g_module_error()));
                THROW(G_MODULE_SYMBOL_ERROR);
            } else {
                LIXA_TRACE(("client_config_load_switch: module address %p, "
                            "function xa_switch found at address %p\n",
                            module, xa_switch));
                LIXA_TRACE(("client_config_laod_switch: "
                            "xa_switch()->name = '%s', "
                            "xa_switch()->flags = %ld\n",
                            xa_switch()->name,
                            xa_switch()->flags));
                act_rsrmgr->module = module;
                act_rsrmgr->xa_switch = xa_switch();
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_MODULE_OPEN_ERROR:
                ret_cod = LIXA_RC_G_MODULE_OPEN_ERROR;
                break;
            case G_MODULE_SYMBOL_ERROR:
                ret_cod = LIXA_RC_G_MODULE_SYMBOL_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_load_switch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_config_unload_switch(const client_config_coll_t *ccc)   
{
    enum Exception { G_MODULE_CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_unload_switch\n"));
    TRY {
        guint i;

        for (i=0; i<ccc->actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                ccc->actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            LIXA_TRACE(("client_config_unload_switch: resource manager # %u, "
                        "defined in config as '%s', module address %p, "
                        "xa_switch->name='%s', xa_switch->flags=%ld\n", i,
                        act_rsrmgr->generic->name,
                        act_rsrmgr->module,
                        act_rsrmgr->xa_switch->name,
                        act_rsrmgr->xa_switch->flags));
            if (!g_module_close(act_rsrmgr->module)) {
                LIXA_TRACE(("client_config_unload_switch: "
                            "g_module_error='%s'\n",
                            g_module_error()));
                THROW(G_MODULE_CLOSE_ERROR);
            }
            act_rsrmgr->module = NULL;
            act_rsrmgr->xa_switch = NULL;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_MODULE_CLOSE_ERROR:
                ret_cod = LIXA_RC_G_MODULE_CLOSE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_unload_switch/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_config_display(client_config_coll_t *ccc)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_display\n"));
    TRY {
        guint i;
        
        /* dump configuration */
        for (i=0; i<ccc->trnmgrs->len; ++i) {
            LIXA_TRACE(("client_config_display: transaction manager # %u, "
                        "name='%s', domain=%d, switch_file='%s', port="
                        IN_PORT_T_FORMAT "\n",
                        i, g_array_index(ccc->trnmgrs,
                                         struct trnmgr_config_s,
                                         i).name,
                        g_array_index(ccc->trnmgrs,
                                      struct trnmgr_config_s,
                                      i).domain,
                        g_array_index(ccc->trnmgrs,
                                      struct trnmgr_config_s,
                                      i).address,
                        g_array_index(ccc->trnmgrs,
                                      struct trnmgr_config_s,
                                      i).port));
        }
        for (i=0; i<ccc->rsrmgrs->len; ++i) {
            LIXA_TRACE(("client_config_display: resource manager # %u, "
                        "name='%s', switch_file='%s'\n",
                        i, g_array_index(ccc->rsrmgrs,
                                         struct rsrmgr_config_s,
                                         i).name,
                        g_array_index(ccc->rsrmgrs,
                                      struct rsrmgr_config_s,
                                      i).switch_file));
        }
        for (i=0; i<ccc->profiles->len; ++i) {
            guint j;
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i); 
            LIXA_TRACE(("client_config_display: profile # %u, name='%s'\n",
                        i, profile->name));
            for (j=0; j<profile->trnmgrs->len; ++j) {
                LIXA_TRACE(("client_config_display: transaction manager # %u, "
                            "name='%s'\n",
                            j, g_array_index(profile->trnmgrs, xmlChar *,
                                             j)));
            }
            for (j=0; j<profile->rsrmgrs->len; ++j) {
                LIXA_TRACE(("client_config_display: resource manager # %u, "
                            "name='%s'\n",
                            j, g_array_index(profile->rsrmgrs, xmlChar *,
                                             j)));
            }
        }
                
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
    LIXA_TRACE(("client_config_display/excp=%d/"
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
    
    LIXA_TRACE(("client_parse/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse/%p: tag %s\n",
                            a_node, cur_node->name));
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
    LIXA_TRACE(("client_parse/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
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
    
    LIXA_TRACE(("client_parse_trnmgrs/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_trnmgrs/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_TRNMGR)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_trnmgr(
                                           ccc, cur_node)))
                        THROW(PARSE_TRNMGR_ERROR);
                }
            }
            if (NULL != cur_node->children &&
                LIXA_RC_OK != (ret_cod = client_parse_trnmgrs(
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
    LIXA_TRACE(("client_parse_trnmgrs/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_trnmgr(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { NAME_NOT_AVAILABLE_ERROR
                     , DOMAIN_NOT_AVAILABLE_ERROR
                     , ADDRESS_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    
    LIXA_TRACE(("client_parse_trnmgr/%p\n", a_node));
    TRY {
        struct trnmgr_config_s record;
        
        /* reset new element */
        record.name = NULL;
        record.domain = 0;
        record.address = NULL;
        record.port = 0;

        /* retrieve transaction manager name */
        if (NULL == (record.name = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);
        /* look/check/set listener domain */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_domain(
                               a_node, &(record.domain))))
            THROW(DOMAIN_NOT_AVAILABLE_ERROR);
        /* retrieve address */
        if (NULL == (record.address = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_ADDRESS_PROPERTY)))
            THROW(ADDRESS_NOT_AVAILABLE_ERROR);
        /* retrieve port */
        if (LIXA_RC_OK != (ret_cod = lixa_config_retrieve_port(
                               a_node, &(record.port))))
            THROW(PORT_NOT_AVAILABLE_ERROR);

        g_array_append_val(ccc->trnmgrs, record);
        
        LIXA_TRACE(("client_parse_trnmgr/%p: %s %d, %s = '%s', %s = %d, "
                    "%s = '%s', %s = " IN_PORT_T_FORMAT  "\n", a_node,
                    (char *)LIXA_XML_CONFIG_TRNMGR, ccc->trnmgrs->len-1,
                    (char *)LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *)record.name,
                    (char *)LIXA_XML_CONFIG_DOMAIN_PROPERTY,
                    record.domain,
                    (char *)LIXA_XML_CONFIG_ADDRESS_PROPERTY,
                    (char *)record.address,
                    (char *)LIXA_XML_CONFIG_PORT_PROPERTY,
                    record.port));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NAME_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_trnmgr: unable to find trnmgr "
                            "name for trnmgr %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
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
    LIXA_TRACE(("client_parse_trnmgr/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
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
    
    LIXA_TRACE(("client_parse_rsrmgrs/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_rsrmgrs/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_RSRMGR)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_rsrmgr(
                                           ccc, cur_node)))
                        THROW(PARSE_RSRMGR_ERROR);
                }
            }
            if (NULL != cur_node->children &&
                LIXA_RC_OK != (ret_cod = client_parse_rsrmgrs(
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
    LIXA_TRACE(("client_parse_rsrmgrs/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_rsrmgr(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { NAME_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , SWITCH_FILE_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    
    LIXA_TRACE(("client_parse_rsrmgr/%p\n", a_node));
    TRY {
        struct rsrmgr_config_s record;
        
        /* reset new element */
        record.name = NULL;
        record.switch_file = NULL;

        /* retrieve resource manager name */
        if (NULL == (record.name = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);
        /* retrieve switch_file */
        if (NULL == (record.switch_file = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY)))
            THROW(SWITCH_FILE_NOT_AVAILABLE_ERROR);

        g_array_append_val(ccc->rsrmgrs, record);
        
        LIXA_TRACE(("client_parse_rsrmgr/%p: %s %d, "
                    "%s = '%s', %s = '%s'\n", a_node,
                    (char *)LIXA_XML_CONFIG_RSRMGR, ccc->rsrmgrs->len-1,
                    (char *)LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *)record.name,
                    (char *)LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY,
                    (char *)record.switch_file));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
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
    LIXA_TRACE(("client_parse_rsrmgr/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_profiles(struct client_config_coll_s *ccc,
                         xmlNode *a_node)
{
    enum Exception { PARSE_PROFILE_ERROR
                     /* , CLIENT_PARSE_PROFILES_ERROR */
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_profiles/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_profiles/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_PROFILE)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_profile(
                                           ccc, cur_node)))
                        THROW(PARSE_PROFILE_ERROR);
                }
            }
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_PROFILE_ERROR:
                /*
            case CLIENT_PARSE_PROFILES_ERROR:
                */
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_profiles/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_profile(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { NAME_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , SWITCH_FILE_NOT_AVAILABLE_ERROR
                     , PARSE_PROFILE_TRNMGRS_ERROR
                     , PARSE_PROFILE_RSRMGRS_ERROR
                     , UNRECOGNIZED_TAG
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;    
    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_profile/%p\n", a_node));
    TRY {
        struct profile_config_s record;
        
        /* reset new element */
        record.name = NULL;
        record.trnmgrs = g_array_new(FALSE, FALSE, sizeof(xmlChar *));
        record.rsrmgrs = g_array_new(FALSE, FALSE, sizeof(xmlChar *));

        /* retrieve name */
        if (NULL == (record.name = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);

        g_array_append_val(ccc->profiles, record);
        
        LIXA_TRACE(("client_parse_profile/%p: %s %d, "
                    "%s = '%s'\n", a_node,
                    (char *)LIXA_XML_CONFIG_PROFILE, ccc->profiles->len-1,
                    (char *)LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *)record.name));

        for (cur_node = a_node->children; cur_node;
             cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_profile/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name,
                               LIXA_XML_CONFIG_TRNMGRS)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_profile_trnmgrs(
                                           ccc, cur_node->children,
                                           record.trnmgrs)))
                        THROW(PARSE_PROFILE_TRNMGRS_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_RSRMGRS)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_profile_rsrmgrs(
                                           ccc, cur_node->children,
                                           record.rsrmgrs)))
                        THROW(PARSE_PROFILE_RSRMGRS_ERROR);
                } else
                    THROW(UNRECOGNIZED_TAG);
            }
        }        

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NAME_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_profile: unable to find profile "
                            "name for profile %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case PARSE_PROFILE_TRNMGRS_ERROR:
            case PARSE_PROFILE_RSRMGRS_ERROR:
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
    LIXA_TRACE(("client_parse_profile/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_profile_trnmgrs(struct client_config_coll_s *ccc,
                                 xmlNode *a_node, GArray *trnmgrs)
{
    enum Exception { CLIENT_PARSE_PROFILE_TRNMGRS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_profile_trnmgrs/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_profile_trnmgrs/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_TRNMGR)) {
                    xmlChar *key= xmlNodeListGetString(
                        ccc->lixac_conf, cur_node->xmlChildrenNode, 1);
                    LIXA_TRACE(("client_parse_profile_trnmgrs/%p: key='%s'\n",
                                a_node, key));
                    g_array_append_val(trnmgrs, key);
                }
            }
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_PARSE_PROFILE_TRNMGRS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_profile_trnmgrs/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_profile_rsrmgrs(struct client_config_coll_s *ccc,
                                 xmlNode *a_node, GArray *rsrmgrs)
{
    enum Exception { CLIENT_PARSE_PROFILE_RSRMGRS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_profile_rsrmgrs/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_profile_rsrmgrs/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_RSRMGR)) {
                    xmlChar *key = xmlNodeListGetString(
                        ccc->lixac_conf, cur_node->xmlChildrenNode, 1);
                    LIXA_TRACE(("client_parse_profile_rsrmgrs/%p: key='%s'\n",
                                a_node, key));
                    g_array_append_val(rsrmgrs, key);
                }
            }
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_PARSE_PROFILE_RSRMGRS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_profile_rsrmgrs/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



