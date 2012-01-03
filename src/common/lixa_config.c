/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif



#include <lixa_config.h>
#include <lixa_errors.h>
#include <lixa_trace.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_CONFIG



const xmlChar *LIXA_XML_CONFIG_LISTENER = (xmlChar *)"listener";
const xmlChar *LIXA_XML_CONFIG_CLIENT = (xmlChar *)"client";
const xmlChar *LIXA_XML_CONFIG_SERVER = (xmlChar *)"server";
const xmlChar *LIXA_XML_CONFIG_SERVER_PID = (xmlChar *)"pid_file";
const xmlChar *LIXA_XML_CONFIG_DOMAIN_PROPERTY = (xmlChar *)"domain";
const xmlChar *LIXA_XML_CONFIG_ADDRESS_PROPERTY = (xmlChar *)"address";
const xmlChar *LIXA_XML_CONFIG_PORT_PROPERTY = (xmlChar *)"port";
const xmlChar *LIXA_XML_CONFIG_DOMAIN_AF_INET_VALUE = (xmlChar *)"AF_INET";
const xmlChar *LIXA_XML_CONFIG_MANAGER = (xmlChar *)"manager";
const xmlChar *LIXA_XML_CONFIG_MANAGER_STATUS = (xmlChar *)"status_file";
const xmlChar *LIXA_XML_CONFIG_STTSRV = (xmlChar *)"sttsrv";
const xmlChar *LIXA_XML_CONFIG_STTSRVS = (xmlChar *)"sttsrvs";
const xmlChar *LIXA_XML_CONFIG_RSRMGR = (xmlChar *)"rsrmgr";
const xmlChar *LIXA_XML_CONFIG_RSRMGRS = (xmlChar *)"rsrmgrs";
const xmlChar *LIXA_XML_CONFIG_PROFILE_PROPERTY = (xmlChar *)"profile";
const xmlChar *LIXA_XML_CONFIG_PROFILE = (xmlChar *)"profile";
const xmlChar *LIXA_XML_CONFIG_PROFILES = (xmlChar *)"profiles";
const xmlChar *LIXA_XML_CONFIG_NAME_PROPERTY = (xmlChar *)"name";
const xmlChar *LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY = (xmlChar *)"switch_file";
const xmlChar *LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY =
    (xmlChar *)"xa_close_info";
const xmlChar *LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY =
    (xmlChar *)"xa_open_info";



int lixa_config_retrieve_domain(xmlNode *cur_node, int *domain)
{
    enum Exception { DOMAIN_NOT_AVAILABLE_ERROR
                     , INVALID_DOMAIN_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    xmlChar *attr = NULL;
    
    LIXA_TRACE(("lixa_config_retrieve_domain\n"));
    TRY {
        if (NULL == (attr = xmlGetProp(cur_node,
                                       LIXA_XML_CONFIG_DOMAIN_PROPERTY)))
            THROW(DOMAIN_NOT_AVAILABLE_ERROR);
        if (!xmlStrcmp(attr, LIXA_XML_CONFIG_DOMAIN_AF_INET_VALUE)) {
            *domain = AF_INET;
        } else {
            LIXA_TRACE(("lixa_config_retrieve_domain: socket domain '%s' "
                        "is not valid\n", (char *)attr));
            THROW(INVALID_DOMAIN_ERROR);
        }
        xmlFree(attr);
        attr = NULL;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case DOMAIN_NOT_AVAILABLE_ERROR:
            case INVALID_DOMAIN_ERROR:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release useless memory */
        if (NULL != attr)
            xmlFree(attr);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_config_retrieve_domain/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_config_retrieve_port(xmlNode *cur_node, in_port_t *port)
{
    enum Exception { PORT_NOT_AVAILABLE_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    xmlChar *attr = NULL;
    
    LIXA_TRACE(("lixa_config_retrieve_port\n"));
    TRY {
        if (NULL == (attr = xmlGetProp(cur_node,
                                       LIXA_XML_CONFIG_PORT_PROPERTY))) {
            THROW(PORT_NOT_AVAILABLE_ERROR);
        } else {
            *port = (in_port_t)strtoul((char *)attr, NULL, 0);
            xmlFree(attr);
            attr = NULL;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PORT_NOT_AVAILABLE_ERROR:
                ret_cod = LIXA_RC_CONFIG_ERROR;
                break;                                
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* release useless memory */
        if (NULL != attr)
            xmlFree(attr);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_config_retrieve_port/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_config_digest(int fd, const char *profile, md5_digest_hex_t digest)
{
    enum Exception { FSTAT_ERROR
                     , MMAP_ERROR
                     , G_CHECKSUM_NEW_ERROR
                     , G_CHECKSUM_GET_STRING_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    struct stat buf;
    guchar *content = NULL;
    GChecksum *checksum = NULL;
    
    LIXA_TRACE(("lixa_config_digest\n"));
    TRY {
        const gchar *tmp = NULL;
        long hostid = gethostid();
        
        /* retrieve file statistics */
        if (0 != fstat(fd, &buf))
            THROW(FSTAT_ERROR);
        /* map memory to file for checksum computation */
        if (NULL == (content =
                     mmap(NULL, buf.st_size, PROT_READ, MAP_SHARED, fd, 0)))
            THROW(MMAP_ERROR);
        /* create a new checksum */
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR);
        /* config file content digest */
        g_checksum_update(checksum, content, buf.st_size);
        /* profile used inside the config file, digest */
        g_checksum_update(checksum, (guchar *)profile, strlen(profile));
        /* "unique" id (maybe default IP4 address), digest */
        g_checksum_update(checksum, (guchar *)&hostid, sizeof(hostid));
        if (NULL == (tmp = g_checksum_get_string(checksum)))
            THROW(G_CHECKSUM_GET_STRING_ERROR);
        strncpy(digest, (const char *)tmp, MD5_DIGEST_LENGTH * 2);
        digest[MD5_DIGEST_LENGTH * 2] = '\0';
        LIXA_TRACE(("lixa_config_digest: config file digest is '%s'\n",
                    digest));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case G_CHECKSUM_NEW_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
            case G_CHECKSUM_GET_STRING_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_GET_STRING_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* recovery allocated resources */
        if (NULL != content)
            munmap(content, buf.st_size);
        if (NULL != checksum)
            g_checksum_free(checksum);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_config_digest/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_job_set_raw(lixa_job_t *job, const char *raw_job)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int truncated = FALSE;
    
    LIXA_TRACE(("lixa_job_set_raw\n"));
    TRY {
        size_t raw_size = strlen(raw_job);
        
        if (raw_size >= LIXA_JOB_RAW_LEN) {
            raw_size = LIXA_JOB_RAW_LEN - 1;
            truncated = TRUE;
        }
              
        lixa_job_reset(job);
        strncpy(job->raw, raw_job, raw_size);
        job->raw[LIXA_JOB_RAW_LEN-1] = '\0';

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = truncated ? LIXA_RC_TRUNCATION_OCCURRED : LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_job_set_raw/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_job_set_source_ip(lixa_job_t *job, int fd)
{
    enum Exception { GETSOCKNAME_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_job_set_source_ip\n"));
    TRY {
        struct sockaddr_in local_sock_addr;
        socklen_t serv_addr_len;
        static GStaticMutex mutex = G_STATIC_MUTEX_INIT;
        char *tmp_source_ip;
        size_t source_ip_len = 0;
        
        serv_addr_len = sizeof(struct sockaddr_in);
        if (0 != getsockname(fd, (struct sockaddr *)&local_sock_addr,
                             &serv_addr_len))
            THROW(GETSOCKNAME_ERROR);        

        memset(job->fields.source_ip, ' ', LIXA_JOB_SOURCE_IP_LEN);
        g_static_mutex_lock(&mutex);
        tmp_source_ip = inet_ntoa(local_sock_addr.sin_addr);
        source_ip_len = strlen(tmp_source_ip);
        if (source_ip_len > LIXA_JOB_SOURCE_IP_LEN)
            source_ip_len = LIXA_JOB_SOURCE_IP_LEN;
        strncpy(job->fields.source_ip, tmp_source_ip, source_ip_len);
        /*
        free(tmp_source_ip);
        */
        g_static_mutex_unlock(&mutex);
        job->fields.separator = LIXA_PATH_SEPARATOR;
        job->fields.terminator = '\0';
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GETSOCKNAME_ERROR:
                ret_cod = LIXA_RC_GETSOCKNAME_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_job_set_source_ip/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

