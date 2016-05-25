/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
 *
 * This file is part of the libraries provided by LIXA.
 * In addition, as a special exception, the copyright holders of LIXA gives
 * Globetom Holdings (Pty) Ltd / 92 Regency Drive / Route 21 Corporate Park /
 * Nellmapius Drive / Centurion / South Africa
 * the permission to redistribute this file and/or modify it under the terms
 * of the GNU Lesser General Public License version 2.1 as published
 * by the Free Software Foundation.
 * The above special grant is perpetual and restricted to
 * Globetom Holdings (Pty) Ltd: IN NO WAY it can be automatically transferred
 * to a different company or to different people. "In no way" contemplates:
 * merge, acquisition and any other possible form of corportate change.
 * IN NO WAY, the above special grant can be assimilated to a patent or
 * any other form of asset.
 */
#include <config.h>



#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYSLOG_H
# include <syslog.h>
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
#include <lixa_utils.h>
#include <lixa_xid.h>
#include <lixa_syslog.h>
#include <client_config.h>
#include <client_status.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_CONFIG



int client_config(client_config_coll_t *ccc)
{
    enum Exception { G_HASH_TABLE_NEW_ERROR
                     , STRDUP_ERROR
                     , OPEN_CONFIG_ERROR
                     , XML_READ_FILE_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , PARSE_CONFIG_ERROR
                     , CLIENT_CONFIG_DISPLAY_ERROR
                     , CLIENT_CONFIG_VALIDATE_ERROR
                     , LIXA_CONFIG_DIGEST_ERROR
                     , CLOSE_ERROR
                     , GETADDRINFO_ERROR
                     , CLIENT_CONFIG_LOAD_SWITCH_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = LIXA_NULL_FD;
    const char *file_name = NULL;
    xmlNode *root_element = NULL;
    struct addrinfo hints, *res = NULL;

    LIXA_TRACE(("client_config\n"));
    TRY {
        char *tmp_str;
        static char program_name[31];
        pthread_t tid;
        int configured = FALSE;
        
        /* lock mutex to start configuration activity */
        LIXA_TRACE(("client_config: acquiring exclusive mutex\n"));
        g_static_mutex_lock(&ccc->mutex);

        /* is the process already configured? */
        if (NULL == ccc->config_threads) {
            if (NULL == (ccc->config_threads = g_hash_table_new(
                             g_direct_hash, g_direct_equal)))
                THROW(G_HASH_TABLE_NEW_ERROR);
        } else
            configured = TRUE;

        /* is the thread already "opened"? */
        tid = pthread_self();
        if (NULL == g_hash_table_lookup(ccc->config_threads,
                                        (gconstpointer)tid)) {
            g_hash_table_insert(ccc->config_threads, (gpointer)tid,
                                (gpointer)1);
            LIXA_TRACE(("client_config: registering this thread\n"));
        }
        
        if (configured) {
            LIXA_TRACE(("client_config: already configured (%u), "
                        "skipping...\n",
                        g_hash_table_size(ccc->config_threads)));
            THROW(NONE);
        } 

        /* reset XML document root */
        ccc->lixac_conf = NULL;
    
        /* initialize libxml2 library */
        LIXA_TRACE(("client_config/xmlInitParser\n"));
        xmlInitParser();
        
        /* open system log */
        lixa_get_program_name(program_name, sizeof(program_name));
        openlog(program_name, LOG_PID, LOG_DAEMON);
        syslog(LOG_INFO, LIXA_SYSLOG_LXC000I,
               LIXA_PACKAGE_NAME, LIXA_PACKAGE_VERSION);
        
        memset(&ccc->serv_addr, 0, sizeof(struct sockaddr_in));
        if (NULL == ccc->actconf.rsrmgrs)
            ccc->actconf.rsrmgrs = g_array_new(
                FALSE, FALSE, sizeof(struct act_rsrmgr_config_s));
        if (NULL == ccc->sttsrvs)
            ccc->sttsrvs = g_array_new(FALSE, FALSE, sizeof(
                                           struct sttsrv_config_s));
        if (NULL == ccc->rsrmgrs)
            ccc->rsrmgrs = g_array_new(FALSE, FALSE, sizeof(
                                           struct rsrmgr_config_s));
        if (NULL == ccc->profiles)
            ccc->profiles = g_array_new(FALSE, FALSE, sizeof(
                                            struct profile_config_s));
     
        if (NULL == (tmp_str = getenv(LIXA_PROFILE_ENV_VAR))) {
            /* use empty string instead of NULL to avoid allocation issues */
            ccc->profile = tmp_str;
            LIXA_TRACE(("client_config: '%s' environment variable not found, "
                        "using default profile for this client\n",
                        LIXA_PROFILE_ENV_VAR));
        } else {
            LIXA_TRACE(("client_config: using transactional profile '%s' for "
                        "subsequent operations\n", tmp_str));        
            if (NULL == (ccc->profile = strdup(tmp_str)))
                THROW(STRDUP_ERROR);
        }

        /* checking if available the custom config file */
        tmp_str = getenv(LIXA_CONFIG_FILE_ENV_VAR);
        if (NULL != tmp_str && -1 != (fd = open(tmp_str, O_RDONLY))) {
            file_name = tmp_str;
        } else {
            file_name = LIXA_CLIENT_CONFIG_SYSTEM_FILE;
            LIXA_TRACE(("client_config: '%s' environment variable not found, "
                        "using default installation config file '%s' for this "
                        "client\n", LIXA_CONFIG_FILE_ENV_VAR, file_name));
            if (-1 == (fd = open(file_name, O_RDONLY))) {
                LIXA_TRACE(("client_config: file %s is not readable, "
                            "throwing error\n", file_name));
                THROW(OPEN_CONFIG_ERROR);
            }
        }
        ccc->lixac_conf_filename = file_name;
        
        /* loading config file */
        LIXA_TRACE(("client_config/xmlReadFile\n"));
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
        
        if (LIXA_RC_OK != (ret_cod = lixa_config_digest(
                               fd, ccc->profile, ccc->config_digest)))
            THROW(LIXA_CONFIG_DIGEST_ERROR);
        lixa_xid_set_global_bqual(ccc->config_digest);
        if (-1 == (ret_cod = close(fd)))
            THROW(CLOSE_ERROR);
        fd = LIXA_NULL_FD;
        
        /* free parsed document */
        LIXA_TRACE(("client_config/xmlFreeDoc\n"));
        xmlFreeDoc(ccc->lixac_conf);
        ccc->lixac_conf = NULL;
        
        /* resolve address */
        LIXA_TRACE(("client_config: resolving address for '%s'\n",
                    ccc->actconf.sttsrv->address));
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_flags = AI_CANONNAME;
        hints.ai_family = ccc->actconf.sttsrv->domain;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_protocol = IPPROTO_TCP;
        
        if (0 != getaddrinfo((char *)ccc->actconf.sttsrv->address, NULL,
                             &hints, &res))
            THROW(GETADDRINFO_ERROR);
        /* set port */
        memcpy(&ccc->serv_addr, (struct sockaddr_in *)res->ai_addr,
               sizeof(struct sockaddr_in));
        ccc->serv_addr.sin_port = htons(ccc->actconf.sttsrv->port);
        
        if (LIXA_RC_OK != (ret_cod = client_config_load_switch(&global_ccc)))
            THROW(CLIENT_CONFIG_LOAD_SWITCH_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_HASH_TABLE_NEW_ERROR:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
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
            case CLIENT_CONFIG_DISPLAY_ERROR:
            case CLIENT_CONFIG_VALIDATE_ERROR:
                break;
            case LIXA_CONFIG_DIGEST_ERROR:
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case GETADDRINFO_ERROR:
                ret_cod = LIXA_RC_GETADDRINFO_ERROR;
                break;
            case CLIENT_CONFIG_LOAD_SWITCH_ERROR:
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
        if (LIXA_NULL_FD != fd)
            close(fd);
        if (excp < NONE && ccc->lixac_conf != NULL) {
            /* free parsed document */
            LIXA_TRACE(("client_config/xmlFreeDoc\n"));
            xmlFreeDoc(ccc->lixac_conf);
            ccc->lixac_conf = NULL;
        }
        /* free memory allocated by getadrinfo function */
        if (NULL != res)
            freeaddrinfo(res);
        /* unlock mutex (locked for configuration activity) */
        LIXA_TRACE(("client_config: releasing exclusive mutex\n"));
        g_static_mutex_unlock(&ccc->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_config_job(client_config_coll_t *ccc, int fd)
{
    enum Exception { MALLOC_ERROR
                     , JOB_SET_SOURCE_IP
                     , JOB_SET_RAW
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    lixa_job_t *tmp_job = NULL;
    
    LIXA_TRACE(("client_config_job\n"));
    TRY {
        const char *tmp_str;
        
        /* lock mutex to start configuration activity */
        LIXA_TRACE(("client_config_job: acquiring exclusive mutex\n"));
        g_static_mutex_lock(&ccc->mutex);

        if (NULL != ccc->job) {
            LIXA_TRACE(("client_config_job: already configured, "
                        "skipping...\n"));
            THROW(NONE);
        }

        /* create the memory necessary to store the object */
        if (NULL == (tmp_job = (lixa_job_t *)malloc(sizeof(lixa_job_t))))
            THROW(MALLOC_ERROR);
        /* checking if available the job environment variable */
        if (NULL == (tmp_str = getenv(LIXA_JOB_ENV_VAR))) {
            LIXA_TRACE(("client_config_job: '%s' environment variable not "
                        "found, computing job string...\n", LIXA_JOB_ENV_VAR));
            lixa_job_reset(tmp_job);
            lixa_job_set_config_digest(tmp_job, ccc->config_digest);
            if (LIXA_RC_OK != (ret_cod = lixa_job_set_source_ip(tmp_job, fd)))
                THROW(JOB_SET_SOURCE_IP);
        } else {
            int rc;
            LIXA_TRACE(("client_config_job: '%s' environment variable value "
                        "is '%s'\n", LIXA_JOB_ENV_VAR, tmp_str));
            rc = lixa_job_set_raw(tmp_job, tmp_str);
            if (LIXA_RC_TRUNCATION_OCCURRED == rc) {
                LIXA_TRACE(("client_config_job: environment variable value is "
                            "too long; job was truncated\n"));
                rc = LIXA_RC_OK;
            }
            if (LIXA_RC_OK != rc)
                THROW(JOB_SET_RAW);
        }
        ccc->job = tmp_job;
        LIXA_TRACE(("client_config_job: job value for this process is '%s'\n",
                    lixa_job_get_raw(ccc->job)));
        tmp_job = NULL;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MALLOC_ERROR:
                ret_cod = LIXA_RC_MALLOC_ERROR;
                break;
            case JOB_SET_SOURCE_IP:
            case JOB_SET_RAW:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release resources */
        if (NULL != tmp_job)
            free(tmp_job);
        /* unlock mutex (locked for configuration activity) */
        LIXA_TRACE(("client_config_job: releasing exclusive mutex\n"));
        g_static_mutex_unlock(&ccc->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_job/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_config_validate(client_config_coll_t *ccc)
{
    enum Exception { STRDUP_ERROR
                     , STTSRV_NOT_DEFINED
                     , STTSRV_NOT_FOUND
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
            xmlChar *prof_sttsrv;
            if (NULL == ccc->profile ||
                0 == xmlStrcmp(profile->name, (const xmlChar *)ccc->profile)) {
                if (NULL == ccc->profile) {
                    if (NULL == (ccc->profile = strdup((char *)profile->name)))
                        THROW(STRDUP_ERROR);
                    LIXA_TRACE(("client_config_validate: profile set to "
                                "default value ('%s')\n", ccc->profile));
                }

                LIXA_TRACE(("client_config_validate: profile '%s' "
                            "matches with profile # %u ('%s')\n",
                            ccc->profile, i, profile->name));
                /* pick-up the (list of) transaction manager(s) */
                if (profile->sttsrvs->len > 1)
                    LIXA_TRACE(("client_config_validate: profile '%s' "
                                "specifies more than one transaction manager "
                                "(%u) but only one is supported in the "
                                "current version\n",
                                profile->name, profile->sttsrvs->len));
                else if (profile->sttsrvs->len == 0) {
                    LIXA_TRACE(("client_config_validate: profile '%s' "
                                "does not specify any transaction manager\n",
                                profile->name));
                    THROW(STTSRV_NOT_DEFINED);
                }
                prof_sttsrv = g_array_index(profile->sttsrvs, xmlChar *, 0);
                LIXA_TRACE(("client_config_validate: associated "
                            "transaction manager name is '%s'\n",
                            prof_sttsrv));
                /* look-up transaction manager */
                for (j=0; j<ccc->sttsrvs->len; ++j) {
                    struct sttsrv_config_s *sttsrv = &g_array_index(
                        ccc->sttsrvs, struct sttsrv_config_s, j);
                    if (0 == xmlStrcmp(prof_sttsrv, sttsrv->name)) {
                        LIXA_TRACE(("client_config_validate: "
                                    "transaction manager '%s' found at "
                                    "position # %u\n",
                                    prof_sttsrv, j));
                        ccc->actconf.sttsrv = sttsrv;
                        break;
                    }
                }
                if (j == ccc->sttsrvs->len) {
                    LIXA_TRACE(("client_config_validate: "
                                "transaction manager '%s' not found\n",
                                prof_sttsrv));
                    THROW(STTSRV_NOT_FOUND);
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
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
            case STTSRV_NOT_DEFINED:
            case STTSRV_NOT_FOUND:
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
                             G_MODULE_BIND_LOCAL | G_MODULE_BIND_LAZY))) {
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
                            "function lixa_get_xa_switch found at "
                            "address %p\n",
                            module, xa_switch));
                LIXA_TRACE(("client_config_load_switch: "
                            "lixa_getxa_switch()->name = '%s', "
                            "lixa_get_xa_switch()->flags = %ld\n",
                            xa_switch()->name,
                            xa_switch()->flags));
                LIXA_TRACE(("client_config_load_switch: resource manager "
                            "dynamically registers: %s\n",
                            xa_switch()->flags & TMREGISTER ?
                            "true" : "false"));
                LIXA_TRACE(("client_config_load_switch: resource manager "
                            "does not support association migration: %s\n",
                            xa_switch()->flags & TMNOMIGRATE ?
                            "true" : "false"));
                LIXA_TRACE(("client_config_load_switch: resource manager "
                            "supports asynchronous operations: %s\n",
                            xa_switch()->flags & TMUSEASYNC ?
                            "true" : "false"));
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



int client_unconfig(client_config_coll_t *ccc)
{
    enum Exception { OUT_OF_RANGE
                     , CLIENT_CONFIG_UNLOAD_SWITCH_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_unconfig\n"));
    TRY {
        guint i, j;
        pthread_t tid = pthread_self();

        /* lock mutex to start deconfiguration activity */
        LIXA_TRACE(("client_unconfig: acquiring exclusive mutex\n"));
        g_static_mutex_lock(&ccc->mutex);

        if (NULL != ccc->config_threads) {
            g_hash_table_remove(ccc->config_threads, (gconstpointer)tid);
            if (g_hash_table_size(ccc->config_threads) != 0) {
                LIXA_TRACE(("client_unconfig: can not unconfigure (%u), "
                            "skipping...\n",
                            g_hash_table_size(ccc->config_threads)));
                THROW(NONE);
            } else {
                g_hash_table_destroy(ccc->config_threads);
                ccc->config_threads = NULL;
            }
        } else {
            LIXA_TRACE(("client_unconfig: can not unconfigure (%p), "
                        "skipping...\n", ccc->config_threads));
            THROW(NONE);
        }
        
        if (LIXA_RC_OK != (ret_cod = client_config_unload_switch(&global_ccc)))
            THROW(CLIENT_CONFIG_UNLOAD_SWITCH_ERROR);

        if (NULL != ccc->job) {
            free(ccc->job);
            ccc->job = NULL;
        }
        
        ccc->lixac_conf_filename = NULL;
        if (NULL != ccc->profile) {
            free(ccc->profile);
            ccc->profile = NULL;
        }

        if (NULL != ccc->actconf.rsrmgrs) {
            g_array_free(ccc->actconf.rsrmgrs, TRUE);
            ccc->actconf.rsrmgrs = NULL;
        }
        
        for (i=0; i<ccc->profiles->len; ++i) {
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i);
            xmlFree(profile->name);
            for (j=0; j<profile->sttsrvs->len; ++j) {
                xmlChar *key = g_array_index(
                    profile->sttsrvs, xmlChar *, j);
                xmlFree(key);
            }
            g_array_free(profile->sttsrvs, TRUE);
            for (j=0; j<profile->rsrmgrs->len; ++j) {
                xmlChar *key = g_array_index(
                    profile->rsrmgrs, xmlChar *, j);
                xmlFree(key);
            }            
            g_array_free(profile->rsrmgrs, TRUE);
        }
        g_array_free(ccc->profiles, TRUE);
        ccc->profiles = NULL;

        for (i=0; i<ccc->rsrmgrs->len; ++i) {
            struct rsrmgr_config_s *rsrmgr = &g_array_index(
                ccc->rsrmgrs, struct rsrmgr_config_s, i);
            xmlFree(rsrmgr->name);
            xmlFree(rsrmgr->switch_file);
        }
        g_array_free(ccc->rsrmgrs, TRUE);
        ccc->rsrmgrs = NULL;

        for (i=0; i<ccc->sttsrvs->len; ++i) {
            struct sttsrv_config_s *sttsrv = &g_array_index(
                ccc->sttsrvs, struct sttsrv_config_s, i);
            xmlFree(sttsrv->name);
            xmlFree(sttsrv->address);
        }
        g_array_free(ccc->sttsrvs, TRUE);
        ccc->sttsrvs = NULL;

        if (NULL != ccc->lixac_conf) {
            LIXA_TRACE(("client_unconfig/xmlFreeDoc\n"));
            xmlFreeDoc(ccc->lixac_conf);
            ccc->lixac_conf = NULL;
        }

        /* release libxml2 stuff */
        LIXA_TRACE(("client_unconfig/xmlCleanupParser\n"));
        xmlCleanupParser();

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case OUT_OF_RANGE:
                ret_cod = LIXA_RC_OUT_OF_RANGE;
                break;
            case CLIENT_CONFIG_UNLOAD_SWITCH_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* unlock mutex (locked for deconfiguration activity) */
        LIXA_TRACE(("client_unconfig: releasing exclusive mutex\n"));
        g_static_mutex_unlock(&ccc->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_unconfig/excp=%d/"
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
        for (i=0; i<ccc->sttsrvs->len; ++i) {
            LIXA_TRACE(("client_config_display: transaction manager # %u, "
                        "name='%s', domain=%d, address='%s', port="
                        IN_PORT_T_FORMAT "\n",
                        i, g_array_index(ccc->sttsrvs,
                                         struct sttsrv_config_s,
                                         i).name,
                        g_array_index(ccc->sttsrvs,
                                      struct sttsrv_config_s,
                                      i).domain,
                        g_array_index(ccc->sttsrvs,
                                      struct sttsrv_config_s,
                                      i).address,
                        g_array_index(ccc->sttsrvs,
                                      struct sttsrv_config_s,
                                      i).port));
        }
        for (i=0; i<ccc->rsrmgrs->len; ++i) {
            struct rsrmgr_config_s *rsrmgr = &g_array_index(
                ccc->rsrmgrs, struct rsrmgr_config_s, i);
            LIXA_TRACE(("client_config_display: resource manager # %u, "
                        "name='%s', switch_file='%s', xa_open_info='%s', "
                        "xa_close_info='%s'\n",
                        i, rsrmgr->name, rsrmgr->switch_file,
                        rsrmgr->xa_open_info, rsrmgr->xa_close_info));
        }
        for (i=0; i<ccc->profiles->len; ++i) {
            guint j;
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i); 
            LIXA_TRACE(("client_config_display: profile # %u, name='%s'\n",
                        i, profile->name));
            for (j=0; j<profile->sttsrvs->len; ++j) {
                LIXA_TRACE(("client_config_display: transaction manager # %u, "
                            "name='%s'\n",
                            j, g_array_index(profile->sttsrvs, xmlChar *,
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
                     , PARSE_STTSRVS_ERROR
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
                                      LIXA_XML_CONFIG_STTSRVS)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_sttsrvs(
                                           ccc, cur_node->children)))
                        THROW(PARSE_STTSRVS_ERROR);
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
            case PARSE_STTSRVS_ERROR:
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



int client_parse_sttsrvs(struct client_config_coll_s *ccc,
                         xmlNode *a_node)
{
    enum Exception { PARSE_STTSRV_ERROR
                     , CLIENT_PARSE_STTSRVS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_sttsrvs/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_sttsrvs/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_STTSRV)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_sttsrv(
                                           ccc, cur_node)))
                        THROW(PARSE_STTSRV_ERROR);
                }
            }
            if (NULL != cur_node->children &&
                LIXA_RC_OK != (ret_cod = client_parse_sttsrvs(
                                   ccc, cur_node->children)))
                THROW(CLIENT_PARSE_STTSRVS_ERROR);
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PARSE_STTSRV_ERROR:
            case CLIENT_PARSE_STTSRVS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_sttsrvs/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    return ret_cod;
}



int client_parse_sttsrv(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { NAME_NOT_AVAILABLE_ERROR
                     , DOMAIN_NOT_AVAILABLE_ERROR
                     , ADDRESS_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    int i = 0;
    
    LIXA_TRACE(("client_parse_sttsrv/%p\n", a_node));
    TRY {
        struct sttsrv_config_s record;
        
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

        g_array_append_val(ccc->sttsrvs, record);
        
        LIXA_TRACE(("client_parse_sttsrv/%p: %s %d, %s = '%s', %s = %d, "
                    "%s = '%s', %s = " IN_PORT_T_FORMAT  "\n", a_node,
                    (char *)LIXA_XML_CONFIG_STTSRV, ccc->sttsrvs->len-1,
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
                LIXA_TRACE(("client_parse_sttsrv: unable to find sttsrv "
                            "name for sttsrv %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case DOMAIN_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_sttsrv: unable to find sttsrv "
                            "domain for sttsrv %d\n", i));
                break;
            case ADDRESS_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_sttsrv: unable to find sttsrv "
                            "ip_address for sttsrv %d\n", i));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case PORT_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_sttsrv: unable to find sttsrv "
                            "port for sttsrv %d\n", i));
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_sttsrv/%p/excp=%d/"
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
                     , XA_OPEN_INFO_NOT_AVAILABLE_ERROR
                     , XA_CLOSE_INFO_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_parse_rsrmgr/%p\n", a_node));
    TRY {
        struct rsrmgr_config_s record;
        xmlChar *tmp;
        
        /* reset new element */
        record.name = NULL;
        record.switch_file = NULL;
        record.xa_open_info[0] = '\0';
        record.xa_close_info[0] = '\0';

        /* retrieve resource manager name */
        if (NULL == (record.name = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);
        /* retrieve switch_file */
        if (NULL == (record.switch_file = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY)))
            THROW(SWITCH_FILE_NOT_AVAILABLE_ERROR);
        /* retrieve xa_open_info */
        if (NULL == (tmp = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY)))
            THROW(XA_OPEN_INFO_NOT_AVAILABLE_ERROR);
        strncpy(record.xa_open_info, (char *)tmp, MAXINFOSIZE);
        record.xa_open_info[MAXINFOSIZE - 1] = '\0';
        xmlFree(tmp);
        /* retrieve xa_close_info */
        if (NULL == (tmp = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY)))
            THROW(XA_CLOSE_INFO_NOT_AVAILABLE_ERROR);
        strncpy(record.xa_close_info, (char *)tmp, MAXINFOSIZE);
        record.xa_close_info[MAXINFOSIZE - 1] = '\0';
        xmlFree(tmp);
        /* add the record to the array */
        g_array_append_val(ccc->rsrmgrs, record);
        
        LIXA_TRACE(("client_parse_rsrmgr/%p: %s %d, "
                    "%s = '%s', %s = '%s', %s = '%s', %s = '%s'\n",
                    a_node,
                    (char *)LIXA_XML_CONFIG_RSRMGR, ccc->rsrmgrs->len-1,
                    (char *)LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *)record.name,
                    (char *)LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY,
                    (char *)record.switch_file,
                    (char *)LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY,
                    record.xa_open_info,
                    (char *)LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY,
                    record.xa_close_info));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NAME_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_rsrmgr: unable to find rsrmgr "
                            "property '%s' for current  resource manager\n",
                            LIXA_XML_CONFIG_NAME_PROPERTY));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case SWITCH_FILE_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_rsrmgr: unable to find rsrngr "
                            "property '%s' for current resource manager\n",
                            LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case XA_OPEN_INFO_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_rsrmgr: unable to find rsrmgr "
                            "property '%s' for current resource manager\n",
                            LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                
            case XA_CLOSE_INFO_NOT_AVAILABLE_ERROR:
                LIXA_TRACE(("client_parse_rsrmgr: unable to find rsrmgr "
                            "property '%s' for current resource manager\n",
                            LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY));
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
                     , PARSE_PROFILE_STTSRVS_ERROR
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
        record.sttsrvs = g_array_new(FALSE, FALSE, sizeof(xmlChar *));
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
                               LIXA_XML_CONFIG_STTSRVS)) {
                    if (LIXA_RC_OK != (ret_cod = client_parse_profile_sttsrvs(
                                           ccc, cur_node->children,
                                           record.sttsrvs)))
                        THROW(PARSE_PROFILE_STTSRVS_ERROR);
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
            case PARSE_PROFILE_STTSRVS_ERROR:
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



int client_parse_profile_sttsrvs(struct client_config_coll_s *ccc,
                                 xmlNode *a_node, GArray *sttsrvs)
{
    enum Exception { CLIENT_PARSE_PROFILE_STTSRVS_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlNode *cur_node = NULL;
    
    LIXA_TRACE(("client_parse_profile_sttsrvs/%p\n", a_node));
    TRY {
        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
            if (cur_node->type == XML_ELEMENT_NODE) {
                LIXA_TRACE(("client_parse_profile_sttsrvs/%p: tag %s\n",
                            a_node, cur_node->name));
                if (!xmlStrcmp(cur_node->name, LIXA_XML_CONFIG_STTSRV)) {
                    xmlChar *key= xmlNodeListGetString(
                        ccc->lixac_conf, cur_node->xmlChildrenNode, 1);
                    LIXA_TRACE(("client_parse_profile_sttsrvs/%p: key='%s'\n",
                                a_node, key));
                    g_array_append_val(sttsrvs, key);
                }
            }
        }        
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLIENT_PARSE_PROFILE_STTSRVS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_profile_sttsrvs/%p/excp=%d/"
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



