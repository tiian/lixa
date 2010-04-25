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
#ifndef LIXA_CONFIG_H
# define LIXA_CONFIG_H



#include <config.h>



#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_inst_conf.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_CONFIG



/**
 * Number of bytes necessary to store an MD5 digest
 */
#define MD5_DIGEST_LENGTH   16



/**
 * IP address len: 3 + 1 + 3 + 1 + 3 + 1 + 3
 */
#define LIXA_JOB_SOURCE_IP_LEN  15



/**
 * All the job string length
 */
#define LIXA_JOB_RAW_LEN  (MD5_DIGEST_LENGTH * 2 + 1 + \
                           LIXA_JOB_SOURCE_IP_LEN + 1)



/**
 * Label used to specify "listener" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_LISTENER;

/**
 * Label used to specify "client" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_CLIENT;

/**
 * Label used to specify "server" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_SERVER;

/**
 * Label used to specify "pid_file" property in "server" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_SERVER_PID;

/**
 * Label used to specify "domain" property
 */
extern const xmlChar *LIXA_XML_CONFIG_DOMAIN_PROPERTY;

/**
 * Label used to specify "address" property
 */
extern const xmlChar *LIXA_XML_CONFIG_ADDRESS_PROPERTY;

/**
 * Label used to specify "port" property
 */
extern const xmlChar *LIXA_XML_CONFIG_PORT_PROPERTY;

/**
 * Label used to specify "AF_INET" value for "domain" property
 */
extern const xmlChar *LIXA_XML_CONFIG_DOMAIN_AF_INET_VALUE;

/**
 * Label used to specify "manager" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_MANAGER;

/**
 * Label used to specify "status_file" property in "manager" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_MANAGER_STATUS;

/**
 * Label used to specify "trnmgr" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_TRNMGR;

/**
 * Label used to specify "trnmgrs" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_TRNMGRS;

/**
 * Label used to specify "rsrmgr" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_RSRMGR;

/**
 * Label used to specify "rsrmgrs" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_RSRMGRS;

/**
 * Label used to specify "profile" property in "trnmgr" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_PROFILE_PROPERTY;

/**
 * Label used to specify "profile" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_PROFILE;

/**
 * Label used to specify "profiles" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_PROFILES;

/**
 * Label used to specify "name" property
 */
extern const xmlChar *LIXA_XML_CONFIG_NAME_PROPERTY;

/**
 * Label used to specify "switch_file" property
 */
extern const xmlChar *LIXA_XML_CONFIG_SWITCH_FILE_PROPERTY;

/**
 * Label used to specify "xa_close_info" property
 */
extern const xmlChar *LIXA_XML_CONFIG_XA_CLOSE_INFO_PROPERTY;

/**
 * Label used to specify "xa_open_info" property
 */
extern const xmlChar *LIXA_XML_CONFIG_XA_OPEN_INFO_PROPERTY;



/**
 * This type is used to store a binary raw MD5 digest 
 */
typedef guint8 md5_digest_t[MD5_DIGEST_LENGTH];
/**
 * This type is used to store an hex printable MD5 digest
 */
typedef char md5_digest_hex_t[MD5_DIGEST_LENGTH * 2 + 1];



/**
 * The union is used to access every single field or all the string; it
 * contains the encoded string of a job
 */
union lixa_job_u {
    struct {
        char config_digest[MD5_DIGEST_LENGTH * 2];
        char separator;
        char source_ip[LIXA_JOB_SOURCE_IP_LEN];
        char terminator;
    }    fields;
    char raw[LIXA_JOB_RAW_LEN];
};

/**
 * This type is defined because @ref lixa_job_u must be used as an opaque
 * object
 */
typedef union lixa_job_u lixa_job_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve the value of the "domain" property from configuration
     * @param cur_node IN a valid node of the XML tree
     * @param domain OUT the domain value retrieved by the function
     * @return a standardized return code
     */
    int lixa_config_retrieve_domain(xmlNode *cur_node, int *domain);



    /**
     * Retrieve the value of the "port" property from configuration
     * @param cur_node IN a valid node of the XML tree
     * @param port OUT the domain value retrieved by the function
     * @return a standardized return code
     */
    int lixa_config_retrieve_port(xmlNode *cur_node, in_port_t *port);



    /**
     * Compute the digest of the file associated to a passed file descriptor
     * @param fd IN file descriptor
     * @param profile IN the string containing the profile assigned to the
     *        process
     * @param digest OUT computed digest
     * @return a standardized return code
     */
    int lixa_config_digest(int fd, const char *profile,
                           md5_digest_hex_t digest);

    

    /**
     * Reset the content of the object
     * @param job IN/OUT reference to object
     */
    static inline void lixa_job_reset(lixa_job_t *job) {
        memset(job->raw, ' ', LIXA_JOB_RAW_LEN - 2);
        job->raw[LIXA_JOB_RAW_LEN - 1] = '\0'; }



    /**
     * Retrieve the raw string of the job; the object must initialized with
     * @ref lixa_job_reset before this method can be called!
     * @param job IN reference to object
     * @return the raw job string
     */
    static inline const char *lixa_job_get_raw(const lixa_job_t *job) {
        return job->raw; }

    

    /**
     * Set the job specifying the raw string (this can be used when the job
     * is not computed, but retrieved from an environment var)
     * @param job IN/OUT reference to object
     * @param raw_job IN the raw string for job
     * @return a standardized return code
     */
    int lixa_job_set_raw(lixa_job_t *job, const char *raw_job);


    
    /**
     * Set path and profile
     * @param job IN/OUT reference to object
     * @param config_digest IN config file (lixac_conf.xml) & profile digest
     * @return a standardized return code
     */
    static inline void lixa_job_set_config_digest(
        lixa_job_t *job, const char *config_digest) {
        strncpy(job->fields.config_digest, config_digest,
                MD5_DIGEST_LENGTH * 2);
        job->fields.terminator = '\0';
    }


    
    /**
     * Set source ip
     * @param job IN/OUT reference to object
     * @param fd IN file descriptor of the socket connected to the server
     * @return a standardized return code
     */
    int lixa_job_set_source_ip(lixa_job_t *job, int fd);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_CONFIG_H */
