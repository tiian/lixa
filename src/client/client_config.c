/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "config.h"



#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_REGEX_H
# include <regex.h>
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



int client_config(client_config_coll_t *ccc, int global_config)
{
    enum Exception { G_HASH_TABLE_NEW_ERROR
                     , G_STRDUP_ERROR
                     , OPEN_CONFIG_ERROR
                     , STTSRVS_ENVVAR
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

        /* initialize the mutex */
        if (!global_config) {
            LIXA_TRACE(("client_config: initializing the mutex...\n"));
            g_mutex_init(&ccc->mutex);
            ccc->mutex_cleared = FALSE;
            LIXA_TRACE(("client_config: ...mutex initialized!\n"));
        }
        /* lock mutex to start configuration activity */
        LIXA_TRACE(("client_config: acquiring exclusive mutex...\n"));
        g_mutex_lock(&ccc->mutex);
        LIXA_TRACE(("client_config: ...exclusive mutex acquired!\n"));

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
                                        (gconstpointer) tid)) {
            g_hash_table_insert(ccc->config_threads, (gpointer) tid,
                                (gpointer) 1);
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
        LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXC000I,
                     LIXA_PACKAGE_NAME, LIXA_PACKAGE_VERSION));

        memset(&ccc->serv_addr, 0, sizeof(struct sockaddr_in));
        if (NULL == ccc->actconf.rsrmgrs)
            ccc->actconf.rsrmgrs = g_array_new(
                FALSE, FALSE, sizeof(struct act_rsrmgr_config_s));
        if (NULL == ccc->sttsrvs)
            ccc->sttsrvs = g_array_new(FALSE, FALSE, sizeof(
                                           struct sttsrv_config_s));
        if (NULL == ccc->rsrmgrs)
            ccc->rsrmgrs = g_ptr_array_new();
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
            if (NULL == (ccc->profile = g_strdup(tmp_str)))
                THROW(G_STRDUP_ERROR);
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

        /* checking if available state servers environment variable */
        if (NULL == (tmp_str = getenv(LIXA_STTSRVS_ENV_VAR))) {
            LIXA_TRACE(("client_config: '%s' environment variable not found, "
                        "state servers will be picked up from config file\n",
                        LIXA_STTSRVS_ENV_VAR));
        } else {
            LIXA_TRACE(("client_config: using state servers '%s' for "
                        "subsequent operations\n", tmp_str));
            if (LIXA_RC_OK != (ret_cod = client_parse_sttsrvs_envvar(
                                   ccc, tmp_str)))
                THROW(STTSRVS_ENVVAR);
        }        

        /* checking if available client timeout environment variable */
        ccc->connection_timeout = LIXA_CLIENT_CONNECTION_TIMEOUT_NULL;
        if (NULL == (tmp_str = getenv(
                         LIXA_CLIENT_CONNECTION_TIMEOUT_ENV_VAR))) {
            LIXA_TRACE(("client_config: '%s' environment variable not found, "
                        "state servers will be picked up from config file\n",
                        LIXA_CLIENT_CONNECTION_TIMEOUT_ENV_VAR));
        } else {
            int timeout = (int)strtol(tmp_str, NULL, 10);
            LIXA_TRACE(("client_config: using client connection timeout %d "
                        "for subsequent operations (env var value)\n",
                        timeout));
            client_config_set_connection_timeout(ccc, timeout);
        }        
        
        /* loading config file */
        LIXA_TRACE(("client_config/xmlReadFile\n"));
        if (NULL == (ccc->lixac_conf = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree & parse */
        if (NULL ==
            (root_element = xmlDocGetRootElement(ccc->lixac_conf)))
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
        lixa_xid_set_bqual(ccc->config_digest, lixa_xid_global_bqual);
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

        if (0 != getaddrinfo((char *) ccc->actconf.sttsrv->address, NULL,
                             &hints, &res))
            THROW(GETADDRINFO_ERROR);
        /* fix client connection timeout if necessary */
        if (LIXA_CLIENT_CONNECTION_TIMEOUT_NULL ==
            client_config_get_connection_timeout(ccc)) {
            client_config_set_connection_timeout(
                ccc, LIXA_CLIENT_CONNECTION_TIMEOUT_DEFAULT);
            LIXA_TRACE(("client_config: client connection timeout can not be "
                        "%d, default value (%d) will be used\n",
                        LIXA_CLIENT_CONNECTION_TIMEOUT_NULL,
                        client_config_get_connection_timeout(ccc)));
            LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXC035I,
                         LIXA_CLIENT_CONNECTION_TIMEOUT_NULL,
                         client_config_get_connection_timeout(ccc)));
        }
        
        /* set port */
        memcpy(&ccc->serv_addr, (struct sockaddr_in *) res->ai_addr,
               sizeof(struct sockaddr_in));
        ccc->serv_addr.sin_port = htons(ccc->actconf.sttsrv->port);

        if (LIXA_RC_OK !=
            (ret_cod = client_config_load_all_switch_files(ccc)))
            THROW(CLIENT_CONFIG_LOAD_SWITCH_ERROR);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_HASH_TABLE_NEW_ERROR:
                ret_cod = LIXA_RC_G_RETURNED_NULL;
                break;
            case G_STRDUP_ERROR:
                ret_cod = LIXA_RC_G_STRDUP_ERROR;
                break;
            case OPEN_CONFIG_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case STTSRVS_ENVVAR:
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
        LIXA_TRACE(("client_config: releasing exclusive mutex...\n"));
        g_mutex_unlock(&ccc->mutex);
        LIXA_TRACE(("client_config: ...exclusive mutex released!\n"));
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_dup(const client_config_coll_t *source,
                      client_config_coll_t *target)
{
    enum Exception { G_HASH_TABLE_NEW_ERROR
                     , G_TRY_MALLOC_ERROR
                     , XML_STRDUP_ERROR1
                     , XML_STRDUP_ERROR2
                     , LOAD_SWITCH_FILE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_dup\n"));
    TRY {
        GHashTableIter iter;
        gpointer key, value;
        guint i;

        /* reset the memory area */
        memset(target, 0, sizeof(client_config_coll_t));
        /* create config_threads hash table */
        if (NULL == (target->config_threads = g_hash_table_new(
                         g_direct_hash, g_direct_equal)))
            THROW(G_HASH_TABLE_NEW_ERROR);
        /* copy config_threads hash table content */
        g_hash_table_iter_init (&iter, source->config_threads);
        while (g_hash_table_iter_next (&iter, &key, &value)) {
            g_hash_table_insert(target->config_threads, key, value);
        } /* while (g_hash_table_iter_next (&iter, &key, &value)) */
        /* copy IP config */
        memcpy(&target->serv_addr, &source->serv_addr,
               sizeof(struct sockaddr_in));
        /* create config arrays */
        target->actconf.rsrmgrs = g_array_new(
            FALSE, FALSE, sizeof(struct act_rsrmgr_config_s));
        target->sttsrvs = g_array_new(FALSE, FALSE, sizeof(
                                          struct sttsrv_config_s));
        target->rsrmgrs = g_ptr_array_new();
        target->profiles = g_array_new(FALSE, FALSE, sizeof(
                                           struct profile_config_s));
        /* duplicate profile name */
        target->profile = g_strdup(source->profile);

        /* duplicate job */
        target->job = (lixa_job_t *)g_memdup(source->job, sizeof(lixa_job_t));
        
        /* copy config file name reference */
        target->lixac_conf_filename = source->lixac_conf_filename;

        /* copy digest */
        memcpy(&target->config_digest, &source->config_digest,
               sizeof(md5_digest_hex_t));

        /* copy the state server configuration: it does not need to be
         * duplicated because it does not change (XML content) */
        target->actconf.sttsrv = source->actconf.sttsrv;
        
        /* copy array actual_config_s : act_rsrmgr_config_s[] */
        for (i=0; i<source->actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *source_arc = &g_array_index(
                source->actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            struct act_rsrmgr_config_s target_arc;
            struct rsrmgr_config_s *target_rc = NULL;
            /* duplicate the source record to build the target one */
            if (NULL == (target_rc = g_try_malloc0(
                             sizeof(struct rsrmgr_config_s))))
                THROW(G_TRY_MALLOC_ERROR);
            /* duplicate the dynamically allocated strings */
            if (NULL == (target_rc->name = xmlStrdup(
                             source_arc->generic->name)))
                THROW(XML_STRDUP_ERROR1);
            if (NULL == (target_rc->switch_file = xmlStrdup(
                             source_arc->generic->switch_file)))
                THROW(XML_STRDUP_ERROR2);
            /* copy static strings */
            memcpy(target_rc->xa_open_info, source_arc->generic->xa_open_info,
                   MAXINFOSIZE);
            memcpy(target_rc->xa_close_info,
                   source_arc->generic->xa_close_info, MAXINFOSIZE);
            /* link the records */
            target_arc.generic = target_rc;
            if (LIXA_RC_OK != (ret_cod = client_config_load_switch_file(
                                   &target_arc,
                                   source_arc->dynamically_defined)))
                THROW(LOAD_SWITCH_FILE_ERROR);
            /* add the records to the arrays */
            g_array_append_val(target->actconf.rsrmgrs, target_arc);
            g_ptr_array_add(target->rsrmgrs, target_rc);
        } /* for (i=0; i<source->actconf.rsrmgrs->len; ++i) */
        
        /* copy connection timeout */
        target->connection_timeout = source->connection_timeout;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_HASH_TABLE_NEW_ERROR:
                ret_cod = LIXA_RC_G_HASH_TABLE_NEW_ERROR;
                break;
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
            case XML_STRDUP_ERROR1:
            case XML_STRDUP_ERROR2:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
                break;
            case LOAD_SWITCH_FILE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_dup/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_job(client_config_coll_t *ccc, int fd)
{
    enum Exception { G_TRY_MALLOC_ERROR
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
        g_mutex_lock(&ccc->mutex);

        if (NULL != ccc->job) {
            LIXA_TRACE(("client_config_job: already configured, "
                        "skipping...\n"));
            THROW(NONE);
        }

        /* create the memory necessary to store the object */
        if (NULL ==
            (tmp_job = (lixa_job_t *)g_try_malloc(sizeof(lixa_job_t))))
            THROW(G_TRY_MALLOC_ERROR);
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
        } /* if (NULL == (tmp_str = getenv(LIXA_JOB_ENV_VAR))) */
        ccc->job = tmp_job;
        LIXA_TRACE(("client_config_job: job value for this process is '%s'\n",
                    lixa_job_get_raw(ccc->job)));
        tmp_job = NULL;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
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
            g_free(tmp_job);
        /* unlock mutex (locked for configuration activity) */
        LIXA_TRACE(("client_config_job: releasing exclusive mutex\n"));
        g_mutex_unlock(&ccc->mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_job/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_validate(client_config_coll_t *ccc)
{
    enum Exception {
        G_STRDUP_ERROR,
        STTSRV_NOT_DEFINED,
        STTSRV_NOT_FOUND,
        RSRMGR_NOT_FOUND,
        PROFILE_NOT_FOUND,
        NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_config_validate\n"));
    TRY {
        guint i, j, k;

        /* look-up profile */
        for (i = 0; i < ccc->profiles->len; ++i) {
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i);
            xmlChar *prof_sttsrv;
            if (NULL == ccc->profile ||
                0 == xmlStrcmp(profile->name,
                               (const xmlChar *) ccc->profile)) {
                if (NULL == ccc->profile) {
                    if (NULL ==
                        (ccc->profile = g_strdup((char *) profile->name)))
                        THROW(G_STRDUP_ERROR);
                    LIXA_TRACE(("client_config_validate: profile set to "
                                "default value ('%s')\n", ccc->profile));
                }
                
                LIXA_TRACE(("client_config_validate: profile '%s' "
                            "matches with profile # %u ('%s')\n",
                            ccc->profile, i, profile->name));
                /* pick-up the (list of) transaction manager(s) */
                if (profile->sttsrvs->len > 1)
                    LIXA_TRACE(("client_config_validate: profile '%s' "
                                "specifies more than one state server "
                                "(%u) but only one is supported in the "
                                "current version\n",
                                profile->name, profile->sttsrvs->len));
                else if (profile->sttsrvs->len == 0) {
                    LIXA_TRACE(("client_config_validate: profile '%s' "
                                "does not specify any state server\n",
                                profile->name));
                    THROW(STTSRV_NOT_DEFINED);
                }
                prof_sttsrv = g_array_index(profile->sttsrvs, xmlChar *, 0);
                LIXA_TRACE(("client_config_validate: associated "
                            "state server name is '%s'\n",
                            prof_sttsrv));
                /* look-up transaction manager */
                for (j = 0; j < ccc->sttsrvs->len; ++j) {
                    struct sttsrv_config_s *sttsrv = &g_array_index(
                        ccc->sttsrvs, struct sttsrv_config_s, j);
                    if (0 == xmlStrcmp(prof_sttsrv, sttsrv->name)) {
                        LIXA_TRACE(("client_config_validate: "
                                    "state server '%s' found at "
                                    "position # %u\n",
                                    prof_sttsrv, j));
                        ccc->actconf.sttsrv = sttsrv;
                        break;
                    }
                }
                if (j == ccc->sttsrvs->len) {
                    LIXA_TRACE(("client_config_validate: "
                                "state server '%s' not found\n",
                                prof_sttsrv));
                    THROW(STTSRV_NOT_FOUND);
                }
                /* scan the resource managers defined for the profile */
                for (j = 0; j < profile->rsrmgrs->len; ++j) {
                    xmlChar *rsrmgr = g_array_index(
                        profile->rsrmgrs, xmlChar *, j);
                    LIXA_TRACE(("client_config_validate: this profile "
                                "requires resource manager '%s' at pos %u\n",
                                rsrmgr, j));
                    for (k = 0; k < ccc->rsrmgrs->len; ++k) {
                        struct rsrmgr_config_s *conf_rsrmgr =
                            (struct rsrmgr_config_s *)
                            g_ptr_array_index(ccc->rsrmgrs, k);
                        if (0 == xmlStrcmp(rsrmgr, conf_rsrmgr->name)) {
                            struct act_rsrmgr_config_s record;
                            record.generic = conf_rsrmgr;
                            record.module = NULL;
                            memset(&record.lixa_iface, 0,
                                   sizeof(lixa_iface_t));
                            record.dynamically_defined = FALSE;
                            client_config_append_rsrmgr(ccc, NULL, &record);
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
            case G_STRDUP_ERROR:
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



void client_config_append_rsrmgr(client_config_coll_t *ccc,
                                 const struct rsrmgr_config_s     *rsrmgr,
                                 const struct act_rsrmgr_config_s *act_rsrmgr)
{
    LIXA_TRACE(("client_config_append_rsrmgr\n"));
    
    if (NULL != rsrmgr) {
        g_ptr_array_add(ccc->rsrmgrs, (gpointer)rsrmgr);
    } else {
        LIXA_TRACE(("client_config_append_rsrmgr: rsrmgr=NULL; skipping\n"));
    }
    if (NULL != act_rsrmgr) {
        g_array_append_val(ccc->actconf.rsrmgrs, *act_rsrmgr);
    } else {
        LIXA_TRACE(("client_config_append_rsrmgr: act_rsrmgr=NULL; "
                    "skipping\n"));
    }
    return;
}



int client_config_load_all_switch_files(client_config_coll_t *ccc)
{
    enum Exception { LOAD_SWITCH_FILE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_config_load_all_switch_files\n"));
    TRY {
        guint i;

        /* scan all the resource manager of the actual config */
        for (i = 0; i < ccc->actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                ccc->actconf.rsrmgrs, struct act_rsrmgr_config_s, i);

            LIXA_TRACE(("client_config_load_all_switch_files: "
                        "resource manager # %u, "
                        "name='%s', switch_file='%s'\n", i,
                        act_rsrmgr->generic->name,
                        act_rsrmgr->generic->switch_file));
            if (LIXA_RC_OK != (ret_cod = client_config_load_switch_file(
                                   act_rsrmgr, FALSE)))
                THROW(LOAD_SWITCH_FILE_ERROR);
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
    LIXA_TRACE(("client_config_load_all_switch_files/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_load_switch_file(struct act_rsrmgr_config_s *act_rsrmgr,
                                   int dynamically_defined)
{
    enum Exception { G_MODULE_OPEN_ERROR
                     , G_MODULE_SYMBOL_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_load_switch_file\n"));
    TRY {
        lixa_get_xa_switch_f xa_switch;
        GModule *module;
        
        if (NULL == (module = g_module_open(
                         (gchar *) act_rsrmgr->generic->switch_file,
                         G_MODULE_BIND_LOCAL | G_MODULE_BIND_LAZY))) {
            LIXA_TRACE(("client_config_load_switch_file: "
                        "switch_file='%s', g_module_error='%s'\n",
                        act_rsrmgr->generic->switch_file,
                        g_module_error()));
            THROW(G_MODULE_OPEN_ERROR);
        }
        if (!g_module_symbol(module, "lixa_get_xa_switch",
                             (gpointer *) &xa_switch)) {
            LIXA_TRACE(("client_config_load_switch_file: "
                        "symbol='%s', "
                        "g_module_error='%s'\n", "lixa_get_xa_switch",
                        g_module_error()));
            THROW(G_MODULE_SYMBOL_ERROR);
        } else {
            LIXA_TRACE(("client_config_load_switch_file: "
                        "module address %p, "
                        "function lixa_get_xa_switch found at "
                        "address %p\n",
                        module, xa_switch));
            LIXA_TRACE(("client_config_load_switch_file: "
                        "lixa_getxa_switch()->name = '%s', "
                        "lixa_get_xa_switch()->flags = %ld\n",
                        xa_switch()->name,
                        xa_switch()->flags));
            LIXA_TRACE(("client_config_load_switch_file: "
                        "resource manager "
                        "dynamically registers: %s\n",
                        xa_switch()->flags & TMREGISTER ?
                        "true" : "false"));
            LIXA_TRACE(("client_config_load_switch_file: "
                        "resource manager "
                        "does not support association migration: %s\n",
                        xa_switch()->flags & TMNOMIGRATE ?
                        "true" : "false"));
            LIXA_TRACE(("client_config_load_switch_file: "
                        "resource manager "
                        "supports asynchronous operations: %s\n",
                        xa_switch()->flags & TMUSEASYNC ?
                        "true" : "false"));
            act_rsrmgr->module = module;
            lixa_iface_set_std(&act_rsrmgr->lixa_iface, xa_switch());
            act_rsrmgr->dynamically_defined = dynamically_defined;
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
    LIXA_TRACE(("client_config_load_switch_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_unconfig(client_config_coll_t *ccc, int global_config)
{
    enum Exception { MUTEX_ALREADY_CLEARED
                     , OUT_OF_RANGE
                     , CLIENT_CONFIG_UNLOAD_SWITCH_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_unconfig\n"));
    TRY {
        guint i, j;
        pthread_t tid = pthread_self();

        if (!global_config && ccc->mutex_cleared) {
            LIXA_TRACE(("client_unconfig: mutex already cleared, "
                        "leaving...\n"));
            THROW(MUTEX_ALREADY_CLEARED);
        }
        
        /* lock mutex to start deconfiguration activity */
        LIXA_TRACE(("client_unconfig: acquiring exclusive mutex...\n"));
        g_mutex_lock(&ccc->mutex);
        LIXA_TRACE(("client_unconfig: ...exclusive mutex acquired!\n"));

        if (NULL != ccc->config_threads) {
            LIXA_TRACE(("client_unconfig: removing current thread from "
                        "hash table...\n"));
            g_hash_table_remove(ccc->config_threads, (gconstpointer) tid);
            if (g_hash_table_size(ccc->config_threads) != 0) {
                LIXA_TRACE(("client_unconfig: can not unconfigure (%u), "
                            "skipping...\n",
                            g_hash_table_size(ccc->config_threads)));
                THROW(NONE);
            } else {
                LIXA_TRACE(("client_unconfig: destroying hash table...\n"));
                g_hash_table_destroy(ccc->config_threads);
                ccc->config_threads = NULL;
            }
        } else {
            LIXA_TRACE(("client_unconfig: can not unconfigure (%p), "
                        "skipping...\n", ccc->config_threads));
            THROW(NONE);
        }

        if (LIXA_RC_OK !=
            (ret_cod = client_config_unload_all_switch_files(ccc)))
            THROW(CLIENT_CONFIG_UNLOAD_SWITCH_ERROR);

        g_free(ccc->job);
        ccc->job = NULL;

        ccc->lixac_conf_filename = NULL;
        g_free(ccc->profile);
        ccc->profile = NULL;

        if (NULL != ccc->actconf.rsrmgrs) {
            g_array_free(ccc->actconf.rsrmgrs, TRUE);
            ccc->actconf.rsrmgrs = NULL;
        }

        for (i = 0; i < ccc->profiles->len; ++i) {
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i);
            xmlFree(profile->name);
            for (j = 0; j < profile->sttsrvs->len; ++j) {
                xmlChar *key = g_array_index(
                    profile->sttsrvs, xmlChar *, j);
                xmlFree(key);
            }
            g_array_free(profile->sttsrvs, TRUE);
            for (j = 0; j < profile->rsrmgrs->len; ++j) {
                xmlChar *key = g_array_index(
                    profile->rsrmgrs, xmlChar *, j);
                xmlFree(key);
            }
            g_array_free(profile->rsrmgrs, TRUE);
        }
        g_array_free(ccc->profiles, TRUE);
        ccc->profiles = NULL;

        for (i = 0; i < ccc->rsrmgrs->len; ++i) {
            struct rsrmgr_config_s *rsrmgr =
                (struct rsrmgr_config_s *)
                g_ptr_array_index(ccc->rsrmgrs, i);
            xmlFree(rsrmgr->name);
            xmlFree(rsrmgr->switch_file);
            g_free(rsrmgr);
        }
        g_ptr_array_free(ccc->rsrmgrs, TRUE);
        ccc->rsrmgrs = NULL;

        for (i = 0; i < ccc->sttsrvs->len; ++i) {
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
            case MUTEX_ALREADY_CLEARED:
                ret_cod = LIXA_RC_OK;
                break;
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
        if (MUTEX_ALREADY_CLEARED != excp) {
            /* unlock mutex (locked for deconfiguration activity) */
            LIXA_TRACE(("client_unconfig: releasing exclusive mutex...\n"));
            g_mutex_unlock(&ccc->mutex);
            LIXA_TRACE(("client_unconfig: ...exclusive mutex released!\n"));
            /* clear the mutex */
            if (!global_config && !ccc->mutex_cleared) {
                LIXA_TRACE(("client_unconfig: clearing the mutex...\n"));
                g_mutex_clear(&ccc->mutex);
                ccc->mutex_cleared = TRUE;
                LIXA_TRACE(("client_unconfig: ...mutex cleared!\n"));
            } /* clear the mutex */
        } /* if (MUTEX_ALREADY_CLEARED != excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_unconfig/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_unload_all_switch_files(client_config_coll_t *ccc)
{
    enum Exception { UNLOAD_SWITCH_FILE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("client_config_unload_all_switch_files\n"));
    TRY {
        guint i;

        LIXA_TRACE(("client_config_unload_all_switch_files: total configured "
                    "resource managers: %u\n", ccc->actconf.rsrmgrs->len));
        for (i = 0; i < ccc->actconf.rsrmgrs->len; ++i) {
            struct act_rsrmgr_config_s *act_rsrmgr = &g_array_index(
                ccc->actconf.rsrmgrs, struct act_rsrmgr_config_s, i);
            client_config_display_rsrmgr(act_rsrmgr);
            if (!act_rsrmgr->dynamically_defined) {
                LIXA_TRACE(("client_config_unload_all_switch_files: "
                            "resource manager # %u, "
                            "defined in config as '%s', module address %p, "
                            "xa_switch->name='%s', xa_switch->flags=%ld\n", i,
                            act_rsrmgr->generic->name,
                            act_rsrmgr->module,
                            lixa_iface_get_name(&act_rsrmgr->lixa_iface),
                            lixa_iface_get_flags(&act_rsrmgr->lixa_iface)));
                if (LIXA_RC_OK != (ret_cod = client_config_unload_switch_file(
                                       act_rsrmgr)))
                    THROW(UNLOAD_SWITCH_FILE_ERROR);
            } else {
                LIXA_TRACE(("client_config_unload_all_switch_files: "
                            "resource manager # %u has been dynamically "
                            "defined, skipping it...\n", i));
            } /* if (act_rsrmgr->dynamically_defined) */
        } /* for (i = 0; i < ccc->actconf.rsrmgrs->len; ++i) */

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case UNLOAD_SWITCH_FILE_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_unload_all_switch_files/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_unload_switch_file(struct act_rsrmgr_config_s *act_rsrmgr)
{
    enum Exception { G_MODULE_CLOSE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_unload_switch_file\n"));
    TRY {
        if (NULL == act_rsrmgr->module) {
            LIXA_TRACE(("client_config_unload_switch_file: module is NULL, "
                        "skipping...\n"));
        } else if (!g_module_close(act_rsrmgr->module)) {
            LIXA_TRACE(("client_config_unload_switch_file: "
                        "g_module_error='%s'\n", g_module_error()));
            THROW(G_MODULE_CLOSE_ERROR);
        }
        act_rsrmgr->module = NULL;
        memset(&act_rsrmgr->lixa_iface, 0, sizeof(lixa_iface_t));
        
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
    LIXA_TRACE(("client_config_unload_switch_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
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
        LIXA_TRACE(("client_config_display: connection_timeout=%d\n",
                    ccc->connection_timeout));
        for (i = 0; i < ccc->sttsrvs->len; ++i) {
            LIXA_TRACE(("client_config_display: transaction manager # %u, "
                        "name='%s', domain=%d, address='%s', port="
                        IN_PORT_T_FORMAT
                        "\n",
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
        for (i = 0; i < ccc->rsrmgrs->len; ++i) {
            struct rsrmgr_config_s *rsrmgr =
                (struct rsrmgr_config_s *)g_ptr_array_index(ccc->rsrmgrs, i);
            LIXA_TRACE(("client_config_display: resource manager # %u, "
                        "name='%s', switch_file='%s', xa_open_info='%s', "
                        "xa_close_info='%s'\n",
                        i, rsrmgr->name, rsrmgr->switch_file,
                        rsrmgr->xa_open_info, rsrmgr->xa_close_info));
        }
        for (i = 0; i < ccc->profiles->len; ++i) {
            guint j;
            struct profile_config_s *profile = &g_array_index(
                ccc->profiles, struct profile_config_s, i);
            LIXA_TRACE(("client_config_display: profile # %u, name='%s'\n",
                        i, profile->name));
            for (j = 0; j < profile->sttsrvs->len; ++j) {
                LIXA_TRACE(("client_config_display: transaction manager # %u, "
                            "name='%s'\n",
                            j, g_array_index(profile->sttsrvs, xmlChar * ,
                                             j)));
            }
            for (j = 0; j < profile->rsrmgrs->len; ++j) {
                LIXA_TRACE(("client_config_display: resource manager # %u, "
                            "name='%s'\n",
                            j, g_array_index(profile->rsrmgrs, xmlChar * ,
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



void client_config_display_rsrmgr(const struct act_rsrmgr_config_s *arc)
{
    LIXA_TRACE(("client_config_display_rsrmgr: generic->name = '%s'\n",
                STRORNULL(arc->generic->name)));
    LIXA_TRACE(("client_config_display_rsrmgr: generic->switch_file = '%s'\n",
                STRORNULL(arc->generic->switch_file)));
    LIXA_TRACE(("client_config_display_rsrmgr: generic->xa_open_info = '%s'\n",
                arc->generic->xa_open_info));
    LIXA_TRACE(("client_config_display_rsrmgr: generic->xa_close_info ="
                " '%s'\n", arc->generic->xa_close_info));
    LIXA_TRACE(("client_config_display_rsrmgr: module = %p\n", arc->module));
    LIXA_TRACE(("client_config_display_rsrmgr: dynamically_defined = %d\n",
                arc->dynamically_defined));
}



gchar *client_config_tostring_rsrmgr(const struct act_rsrmgr_config_s *arc)
{
    enum Exception { NULL_OBJECT
                     , G_STRCONCAT_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    gchar *tostring = NULL;
    
    LIXA_TRACE(("client_config_tostring_rsrmgr\n"));
    TRY {
        if (NULL == arc)
            THROW(NULL_OBJECT);
        tostring = g_strconcat(
            STRORNULL(arc->generic->name), "|",
            STRORNULL(arc->generic->switch_file), "|",
            STRORNULL(arc->generic->xa_open_info), "|",
            STRORNULL(arc->generic->xa_close_info), "|",
            NULL);
        if (NULL == tostring)
            THROW(G_STRCONCAT_ERROR);
        LIXA_TRACE(("client_config_tostring_rsrmgr: tostring is '%s'\n",
                    tostring));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case G_STRCONCAT_ERROR:
                ret_cod = LIXA_RC_G_STRCONCAT_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_tostring_rsrmgr/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return tostring;
}



int client_config_rsrmgr_dup(const struct act_rsrmgr_config_s *arc,
                             struct rsrmgr_config_s     *rsrmgr,
                             struct act_rsrmgr_config_s *act_rsrmgr)
{
    enum Exception { NULL_OBJECT
                     , XML_STRDUP_ERROR1
                     , XML_STRDUP_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_rsrmgr_dup\n"));
    TRY {
        if (NULL == arc)
            THROW(NULL_OBJECT);
        if (NULL != rsrmgr) {
            *rsrmgr = *arc->generic;
            rsrmgr->name = NULL;
            rsrmgr->switch_file = NULL;
            if (NULL != arc->generic->name &&
                NULL == (rsrmgr->name = xmlStrdup(arc->generic->name)))
                THROW(XML_STRDUP_ERROR1);
            if (NULL != arc->generic->switch_file &&
                NULL == (rsrmgr->switch_file = xmlStrdup(
                             arc->generic->switch_file)))
                THROW(XML_STRDUP_ERROR2);
        } else {
            LIXA_TRACE(("client_config_rsrmgr_dup: rsrmgr=NULL, skipping\n"));
        } /* if (NULL != rsrmgr) */

        if (NULL != act_rsrmgr) {
            *act_rsrmgr = *arc;
        } else {
            LIXA_TRACE(("client_config_rsrmgr_dup: act_rsrmgr=NULL, "
                        "skipping\n"));
        } /* if (NULL != act_rsrmgr) */
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case XML_STRDUP_ERROR1:
            case XML_STRDUP_ERROR2:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recover memory in the event of an error */
        if (NONE > excp) {
            if (NULL != rsrmgr->name)
                xmlFree(rsrmgr->name);
            if (NULL != rsrmgr->switch_file)
                xmlFree(rsrmgr->switch_file);
        } /* if (NONE > excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_rsrmgr_dup/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_config_set_connection_timeout(client_config_coll_t *ccc, int value)
{
    enum Exception { INVALID_OPTION
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_config_set_connection_timeout\n"));
    TRY {
        if (LIXA_CLIENT_CONNECTION_TIMEOUT_NULL == value) {
            LIXA_TRACE(("client_config_set_connection_timeout: "
                        "client connection timeout can not be %d\n",
                        LIXA_CLIENT_CONNECTION_TIMEOUT_NULL));
            THROW(INVALID_OPTION);
        } else
            ccc->connection_timeout = value;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_OPTION:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config_set_connection_timeout/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_parse(struct client_config_coll_s *ccc,
                 xmlNode *a_node)
{
    enum Exception { RETRIEVE_CONNECTION_TIMEOUT
                     , CLIENT_PARSE_ERROR
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
                    long connection_timeout;
                    /* retrieve connection_timeout */
                    if (LIXA_RC_OK == (
                            ret_cod = lixa_config_retrieve_generic_long(
                                cur_node,
                                LIXA_XML_CONFIG_CLIENT_CONNECTION_TIMEOUT,
                                &connection_timeout))) {
                        LIXA_TRACE(("client_parse: parameter '%s' is %ld\n",
                                    (const char *)
                                    LIXA_XML_CONFIG_CLIENT_CONNECTION_TIMEOUT,
                                    connection_timeout));
                        if (LIXA_CLIENT_CONNECTION_TIMEOUT_NULL ==
                            client_config_get_connection_timeout(ccc)) {
                            LIXA_TRACE(("client_parse: environment var not "
                                        "set, using value %ld for "
                                        "connection_timeout parameter\n",
                                        connection_timeout));
                            client_config_set_connection_timeout(
                                ccc, connection_timeout);
                        }
                    } else if (LIXA_RC_OBJ_NOT_FOUND != ret_cod)
                        THROW(RETRIEVE_CONNECTION_TIMEOUT);
                    /* parse child */
                    if (LIXA_RC_OK != (ret_cod = client_parse(
                                           ccc, cur_node->children)))
                        THROW(CLIENT_PARSE_ERROR);
                } else if (!xmlStrcmp(cur_node->name,
                                      LIXA_XML_CONFIG_STTSRVS)) {
                    /* skip if already set by env var */
                    if (0 == ccc->sttsrvs->len)
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
            case RETRIEVE_CONNECTION_TIMEOUT:
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
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
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
                    "%s = '%s', %s = "
                    IN_PORT_T_FORMAT
                    "\n", a_node,
                    (char *) LIXA_XML_CONFIG_STTSRV, ccc->sttsrvs->len -
                    1,
                    (char *) LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *) record.name,
                    (char *) LIXA_XML_CONFIG_DOMAIN_PROPERTY,
                    record.domain,
                    (char *) LIXA_XML_CONFIG_ADDRESS_PROPERTY,
                    (char *) record.address,
                    (char *) LIXA_XML_CONFIG_PORT_PROPERTY,
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
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_parse_rsrmgr(struct client_config_coll_s *ccc,
                        xmlNode *a_node)
{
    enum Exception { G_TRY_MALLOC_ERROR
                     , NAME_NOT_AVAILABLE_ERROR
                     , PORT_NOT_AVAILABLE_ERROR
                     , SWITCH_FILE_NOT_AVAILABLE_ERROR
                     , XA_OPEN_INFO_NOT_AVAILABLE_ERROR
                     , XA_CLOSE_INFO_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    struct rsrmgr_config_s *record = NULL;

    LIXA_TRACE(("client_parse_rsrmgr/%p\n", a_node));
    TRY {
        xmlChar *tmp;

        /* allocate memory for a new record */
        if (NULL == (record = g_try_malloc(sizeof(struct rsrmgr_config_s))))
            THROW(G_TRY_MALLOC_ERROR);
        
        /* reset new element */
        record->name = NULL;
        record->switch_file = NULL;
        record->xa_open_info[0] = '\0';
        record->xa_close_info[0] = '\0';

        /* retrieve resource manager name */
        if (NULL == (record->name = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY)))
            THROW(NAME_NOT_AVAILABLE_ERROR);
        /* retrieve switch_file */
        if (NULL == (record->switch_file = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY)))
            THROW(SWITCH_FILE_NOT_AVAILABLE_ERROR);
        /* retrieve xa_open_info */
        if (NULL == (tmp = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY)))
            THROW(XA_OPEN_INFO_NOT_AVAILABLE_ERROR);
        strncpy(record->xa_open_info, (char *) tmp, MAXINFOSIZE);
        record->xa_open_info[MAXINFOSIZE - 1] = '\0';
        xmlFree(tmp);
        /* retrieve xa_close_info */
        if (NULL == (tmp = xmlGetProp(
                         a_node, LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY)))
            THROW(XA_CLOSE_INFO_NOT_AVAILABLE_ERROR);
        strncpy(record->xa_close_info, (char *) tmp, MAXINFOSIZE);
        record->xa_close_info[MAXINFOSIZE - 1] = '\0';
        xmlFree(tmp);
        LIXA_TRACE(("client_parse_rsrmgr/%p: %s %d, "
                    "%s = '%s', %s = '%s', %s = '%s', %s = '%s'\n",
                    a_node,
                    (char *) LIXA_XML_CONFIG_RSRMGR, ccc->rsrmgrs->len - 1,
                    (char *) LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *) record->name,
                    (char *) LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY,
                    (char *) record->switch_file,
                    (char *) LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY,
                    record->xa_open_info,
                    (char *) LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY,
                    record->xa_close_info));
        /* add the record to the array */
        g_ptr_array_add(ccc->rsrmgrs, record);
        record = NULL;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_TRY_MALLOC_ERROR:
                ret_cod = LIXA_RC_G_TRY_MALLOC_ERROR;
                break;
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
        /* memory recovery */
        if (NULL != record) {
            LIXA_TRACE(("client_parse_rsrmgr: recoverying record memory\n"));
            g_free(record);
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_rsrmgr/%p/excp=%d/"
                "ret_cod=%d/errno=%d\n", a_node, excp, ret_cod, errno));
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
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
                         a_node, LIXA_XML_CONFIG_NAME_PROPERTY))) THROW(
                             NAME_NOT_AVAILABLE_ERROR);

        g_array_append_val(ccc->profiles, record);

        LIXA_TRACE(("client_parse_profile/%p: %s %d, "
                    "%s = '%s'\n", a_node,
                    (char *) LIXA_XML_CONFIG_PROFILE, ccc->profiles->len - 1,
                    (char *) LIXA_XML_CONFIG_NAME_PROPERTY,
                    (char *) record.name));

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
    LIXA_TRACE_STACK();
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
                    xmlChar *key = xmlNodeListGetString(
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
    LIXA_TRACE_STACK();
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
    LIXA_TRACE_STACK();
    return ret_cod;
}



int client_parse_sttsrvs_envvar(client_config_coll_t *ccc,
                                const char *envvar_str)
{
    enum Exception { REGCOMP_ERROR
                     , STRDUP_ERROR1
                     , REGEXEC_NO_MATCH
                     , REGEXEC_ERROR
                     , XML_STRDUP_ERROR1
                     , CONFIG_ERROR1
                     , STRDUP_ERROR2
                     , CONFIG_ERROR2
                     , XML_STRDUP_ERROR2
                     , CONFIG_ERROR3
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    regex_t preg;
    char *str = NULL;
    xmlChar *address = NULL;
    xmlChar *name = NULL;
    
    LIXA_TRACE(("client_parse_sttsrvs_envvar\n"));
    TRY {
        char *saveptr = NULL;
        char *uri = NULL;
        char *p;
        int reg_error;
        char reg_errbuf[500];
        regmatch_t pmatch[5];
        size_t nmatch = sizeof(pmatch)/sizeof(regmatch_t);
        struct sttsrv_config_s record;
        
        /* reset new element */
        record.name = NULL;
        record.domain = AF_INET; /* tcp:// it's only INET */
        record.address = NULL;
        record.port = 0;

        /* compile the regular expression used to match and split the URI */
        if (0 != (reg_error = regcomp(
                      &preg, "^tcp:\\/\\/([^:\\/?#]+):([0-9]+)\\/([^\\/?#]*)$",
                      REG_EXTENDED|REG_ICASE))) {
            regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
            LIXA_TRACE(("client_parse_sttsrvs_envvar: error while compiling "
                        "the regular expression ('%s')\n", reg_errbuf));
            THROW(REGCOMP_ERROR);            
        }
        /* duplicate environment variable because the parameter is read only */
        if (NULL == (str = strdup(envvar_str)))
            THROW(STRDUP_ERROR1);
        
        p =str;
        while (NULL != (uri = strtok_r(p, " ", &saveptr))) {
            LIXA_TRACE(("client_parse_sttsrvs_envvar: uri='%s'\n", uri));
            p = NULL;
            /* analyze URI */
            reg_error = regexec(&preg, uri, nmatch, pmatch, 0);
            if (REG_NOMATCH == reg_error) {
                regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
                LIXA_TRACE(("client_parse_sttsrvs_envvar: URI does "
                            "not match regular expression ('%s')\n",
                            reg_errbuf));
                THROW(REGEXEC_NO_MATCH);
            }
            if (0 != reg_error) {
                regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
                LIXA_TRACE(("client_parse_sttsrvs_envvar: error while "
                            "matching regular expression ('%s')\n",
                            reg_errbuf));
                THROW(REGEXEC_ERROR);
            }
            /* address part or the URI */
            if (pmatch[1].rm_so > 0 && pmatch[1].rm_eo > 0) {
                if (NULL == (address = xmlCharStrndup(
                                 uri+pmatch[1].rm_so,
                                 pmatch[1].rm_eo - pmatch[1].rm_so)))
                    THROW(XML_STRDUP_ERROR1);
                record.address = address;
                address = NULL;
                LIXA_TRACE(("client_parse_sttsrvs_envvar: address='%s'\n",
                            record.address));
            } else
                THROW(CONFIG_ERROR1);
            /* port part of the URI */
            if (pmatch[2].rm_so > 0 && pmatch[2].rm_eo > 0) {
                char *port_str = NULL;
                if (NULL == (port_str = strndup(
                                 uri+pmatch[2].rm_so,
                                 pmatch[2].rm_eo - pmatch[2].rm_so)))
                    THROW(STRDUP_ERROR2);
                record.port = (in_port_t)strtoul(port_str, NULL, 0);
                LIXA_TRACE(("client_parse_sttsrvs_envvar: port="
                            IN_PORT_T_FORMAT "\n", record.port));
            } else
                THROW(CONFIG_ERROR2);
            /* name part or the URI */
            if (pmatch[3].rm_so > 0 && pmatch[3].rm_eo > 0) {
                name = xmlCharStrndup(uri+pmatch[3].rm_so,
                                      pmatch[3].rm_eo - pmatch[3].rm_so);
                record.name = name;
                name = NULL;
                LIXA_TRACE(("client_parse_sttsrvs_envvar: name='%s'\n",
                            record.name));
            } else
                THROW(CONFIG_ERROR3);
            g_array_append_val(ccc->sttsrvs, record);
        }
            
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case REGCOMP_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case STRDUP_ERROR1:
            case STRDUP_ERROR2:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
            case REGEXEC_ERROR:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case XML_STRDUP_ERROR1:
            case XML_STRDUP_ERROR2:
                ret_cod = LIXA_RC_XML_STRDUP_ERROR;
                break;
            case REGEXEC_NO_MATCH:
            case CONFIG_ERROR1:
            case CONFIG_ERROR2:
            case CONFIG_ERROR3:
                LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXC033E,
                             LIXA_STTSRVS_ENV_VAR, envvar_str));
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release memory */
        if (NULL != str) {
            free(str);
            str = NULL;
        }
        if (NULL != address)
            xmlFree(address);
        if (NULL != name)
            xmlFree(name);
        if (excp > REGCOMP_ERROR)
            regfree(&preg);
    } /* TRY-CATCH */
    LIXA_TRACE(("client_parse_sttsrvs_envvar/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
    LIXA_TRACE_STACK();
}

