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
 *
 * August 1st, 2016: Christian Ferrari thanks Globetom Holdings (Pty) Ltd
 * for its donation to Emergency NGO, an international charity that promotes
 * a culture of peace, solidarity and respect for human rights providing free,
 * high quality medical and surgical treatment to the victims of war, landmines
 * and poverty.
 */
#ifndef CLIENT_CONFIG_H
# define CLIENT_CONFIG_H



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
#ifdef HAVE_GMODULE_H
# include <gmodule.h>
#endif
#ifdef HAVE_UUID_H
# include <uuid.h>
#endif
#ifdef HAVE_UUID_UUID_H
# include <uuid/uuid.h>
#endif



#include <lixa_trace.h>
#include <lixa_config.h>
#include <xa.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_CONFIG



/**
 * Name of the environment variable must be used to specify the profile
 */
#define LIXA_PROFILE_ENV_VAR "LIXA_PROFILE"



/**
 * Name of the environment variable must be uset to specify the client config
 * file name
 */
#define LIXA_CONFIG_FILE_ENV_VAR "LIXA_CONFIG_FILE"



/**
 * Name of the environment variable must be uset to specify the job
 */
#define LIXA_JOB_ENV_VAR "LIXA_JOB"



/**
 * It contains the configuration of a transaction manager (how to reach and
 * use it)
 */
struct sttsrv_config_s {
    /**
     * Name associated to the transaction manager
     */
    xmlChar *name;
    /**
     * Socket domain for the socket connection
     */
    int domain;
    /**
     * Address used to reach the transaction manager
     */
    xmlChar *address;
    /**
     * Port used to reach the transaction manager
     */
    in_port_t port;
};



/**
 * It contains the configuration of a resource manager (how to reach and
 * use it)
 */
struct rsrmgr_config_s {
    /**
     * Name associated to the resource manager
     */
    xmlChar *name;
    /**
     * Switch file must be used to interact with the resource manager
     */
    xmlChar *switch_file;
    /**
     * xa_info string must be passed to the resource manager at xa_open
     * invocation
     */
    char     xa_open_info[MAXINFOSIZE];
    /**
     * xa_info string must be passed to the resource manager at xa_close
     * invocation
     */
    char     xa_close_info[MAXINFOSIZE];
};



/**
 * It contains the properties of a resource manager has been specified inside
 * the actual transactional profile; this is a reacher struct than
 * @ref rsrmgr_config_s
 */
struct act_rsrmgr_config_s {
    /**
     * This is a reference to the struct as parsed from XML config file
     */
    struct rsrmgr_config_s *generic;
    /**
     * This is a pointer to the dynamically loaded module containing the
     * interface wrapped by @ref xa_switch
     */
    GModule                *module;
    /**
     * Pointer to the structure must be used to interface the resource
     * manager
     */
    struct xa_switch_t     *xa_switch;
};



/**
 * It contains the configuration of a profile
 */
struct profile_config_s {
    /**
     * Name associated to the resource manager
     */
    xmlChar *name;
    /**
     * Array of transaction managers defined for the profile
     */
    GArray  *sttsrvs;
    /**
     * Array of resource managers will be used for the transactions associated
     * to this profile
     */
    GArray  *rsrmgrs;
};



/**
 * This struct is used to keep the configuration stuff retrived from
 * config file and restricted to stuff related to the current profile.
 * The struct is a set of pointers to data already stored in the global
 * config
 */
struct actual_config_s {
    /**
     * Current transaction manager
     */
    struct sttsrv_config_s *sttsrv;
    /**
     * Current resource managers
     */
    GArray                 *rsrmgrs;
};



/**
 * It contains the configuration for the client
 * if (profile == NULL) the configuration must be loaded
 * else the configuration has already been loaded
 */
struct client_config_coll_s {
    /**
     * This mutex is used to assure only the first thread load the
     * configuration for all the following threads. It must be statically
     * initialized because this code is a library fetched by something else.
     */
    GStaticMutex                 mutex;
    /**
     * Set of thread are "opened" and "configured"
     */
    GHashTable                  *config_threads;
    /**
     * Transactional profile associated to the threads of this process (it
     * must be an heap allocated variable)
     */
    char                        *profile;
    /**
     * Transactional job associated to the threads of this process (retrieved
     * from environement variable @ref LIXA_JOB_ENV_VAR or computed at runtime)
     */
    lixa_job_t                  *job;
    /**
     * Path used to load lixac_conf file
     */
    const char                  *lixac_conf_filename;
    /**
     * XML document representing lixac_conf.xml file
     */
    xmlDocPtr                    lixac_conf;
    /**
     * Hex format of the MD5 digest of lixac_conf file concatenated to profile
     */
    md5_digest_hex_t             config_digest;
    /**
     * It contains the subset of actual configuration for this client
     */
    struct actual_config_s       actconf;
    /**
     * Transaction managers' configuration
     */
    GArray                      *sttsrvs;
    /**
     * Resource managers' configuration
     */
    GArray                      *rsrmgrs;
    /**
     * Profiles' configuration
     */
    GArray                      *profiles;
    /**
     * These are the parameters will be used by all the clients of this
     * process to reach the transaction manager.
     * This structure is filled once, and all the connection from all the
     * thread inside the same process, are using it:
     * 1 process -> 1 value for environment variabile LIXA_PROFILE ->
     * 1 transaction manager will serve
     * It's useless to resolve the transaction manager address every time
     * a client want to connect to it.
     * If the DNS is updated, the client process must be recycled in order to
     * reach the server process (transaction manager): this is not so
     * strange.
     */
    struct sockaddr_in           serv_addr;
};



typedef struct client_config_coll_s client_config_coll_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve a reference to the current transaction manager properties
     * @param ccc IN configuration reference
     * @return a pointer to the current transaction manager
     */
    static inline struct sttsrv_config_s *client_config_get_sttsrv(
        const client_config_coll_t *ccc) {
        return ccc->actconf.sttsrv; }


    
    /**
     * Load configuration from environment vars and XML files
     * @param ccc OUT the object will contain the client configuration
     * @return a standardized return code
     */
    int client_config(client_config_coll_t *ccc);

    

    /**
     * Perform an extra configuration step can not be performed when
     * @ref client_config is called
     * @param ccc IN/OUT the object that contains the client configuration
     * @param fd IN the file descriptor associated to the server connected
     *        socket
     * @return a standardized return code
     */
    int client_config_job(client_config_coll_t *ccc, int fd);

    
    
    /**
     * Validate configuration: fix the transaction and resource managers
     * @param ccc IN/OUT the object will contain the client configuration
     * @return a standardized return code
     */
    int client_config_validate(client_config_coll_t *ccc);

    

    /**
     * Load the configured switch file
     * @param ccc IN configuration object reference
     * @return a standardized return code
     */
    int client_config_load_switch(const client_config_coll_t *ccc);



    /**
     * Unload configuration and release memory
     * @param ccc OUT the object that contains the client configuration
     * @return a standardized return code
     */
    int client_unconfig(client_config_coll_t *ccc);

    

    /**
     * Unload the configured switch file
     * @param ccc IN configuration object reference
     * @return a standardized return code
     */
    int client_config_unload_switch(const client_config_coll_t *ccc);


    
    /**
     * Display configuration read from XML config file
     * @param ccc IN the object will contain the client configuration
     * @return a standardized return code
     */
    int client_config_display(client_config_coll_t *ccc);

    

    /**
     * Parse the configuration tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse(struct client_config_coll_s *ccc,
                     xmlNode *a_node);


 
    /**
     * Parse a "sttsrvs" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_sttsrvs(struct client_config_coll_s *ccc,
                             xmlNode *a_node);



   /**
     * Parse a "sttsrv" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_sttsrv(struct client_config_coll_s *ccc,
                            xmlNode *a_node);



    /**
     * Parse a "rsrmgrs" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_rsrmgrs(struct client_config_coll_s *ccc,
                             xmlNode *a_node);



    /**
     * Parse a "rsrmgr" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_rsrmgr(struct client_config_coll_s *ccc,
                            xmlNode *a_node);



    /**
     * Parse a "profiles" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_profiles(struct client_config_coll_s *ccc,
                             xmlNode *a_node);



    /**
     * Parse a "profile" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_profile(struct client_config_coll_s *ccc,
                             xmlNode *a_node);



    /**
     * Parse a "sttsrvs" node inside a "profile" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @param sttsrvs IN/OUT the array with all the transaction managers
     *                       defined for a profile
     * @return a standardized return code
     */
    int client_parse_profile_sttsrvs(struct client_config_coll_s *ccc,
                                     xmlNode *a_node, GArray *sttsrvs);



    /**
     * Parse a "rsrmgrs" node inside a "profile" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @param rsrmgrs IN/OUT the array with all the resource managers
                             defined for a profile
     * @return a standardized return code
     */
    int client_parse_profile_rsrmgrs(struct client_config_coll_s *ccc,
                                     xmlNode *a_node, GArray *rsrmgrs);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* CLIENT_CONFIG_H */
