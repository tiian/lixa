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
 * Name of the environment variable must be uset to specify che client config
 * file name
 */
#define LIXA_CONFIG_FILE_ENV_VAR "LIXA_CONFIG_FILE"



/**
 * It contains the configuration of a transaction manager (how to reach and
 * use it)
 */
struct trnmgr_config_s {
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
    xmlChar *xa_open_info;
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
    GArray  *trnmgrs;
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
    struct trnmgr_config_s *trnmgr;
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
     * TRUE if the configuration has been successfully loaded, FALSE otherwise
     */
    int                          configured;
    /**
     * This mutex is used to assure only the first thread load the
     * configuration for all the following threads. It must be statically
     * initialized because this code is a library fetched by something else.
     */
    GStaticMutex                 mutex;
    /**
     * Transactional profile associated to the threads of this process (it
     * must be an heap allocated variable)
     */
    char                        *profile;
    /**
     * XML document representing lixac_conf.xml file
     */
    xmlDocPtr                    lixac_conf;
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
    /**
     * It contains the subset of actual configuration for this client
     */
    struct actual_config_s       actconf;
    /**
     * Transaction managers' configuration
     */
    GArray                      *trnmgrs;
    /**
     * Resource managers' configuration
     */
    GArray                      *rsrmgrs;
    /**
     * Profiles' configuration
     */
    GArray                      *profiles;
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
    static inline struct trnmgr_config_s *client_config_get_trnmgr(
        const client_config_coll_t *ccc) {
        return ccc->actconf.trnmgr; }


    
    /**
     * Initialize a new "object" of type client config
     * @param ccc OUT object reference
     * @return a standardized return code
     */
    int client_config_coll_init(client_config_coll_t *ccc);

    

    /**
     * Load configuration from environment vars and XML files
     * @param ccc OUT the object will contain the client configuration
     * @return a standardized return code
     */
    int client_config(client_config_coll_t *ccc);

    

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
     * Parse a "trnmgrs" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_trnmgrs(struct client_config_coll_s *ccc,
                             xmlNode *a_node);



   /**
     * Parse a "trnmgr" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_trnmgr(struct client_config_coll_s *ccc,
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
     * Parse a "trnmgrs" node inside a "profile" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @param trnmgrs IN/OUT the array with all the transaction managers
     *                       defined for a profile
     * @return a standardized return code
     */
    int client_parse_profile_trnmgrs(struct client_config_coll_s *ccc,
                                     xmlNode *a_node, GArray *trnmgrs);



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
