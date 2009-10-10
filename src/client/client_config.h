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
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
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
     * Socket domain for the socket connection
     */
    int domain;
    /**
     * Address used to reach the transaction manager
     */
    char *address;
    /**
     * Port used to reach the transaction manager
     */
    in_port_t port;
    /**
     * Transactional profile associated to this entry
     */
    char *profile;
};

/**
 * It contains the configuration of all the transaction managers
 */
struct trnmgr_config_array_s {
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct trnmgr_config_s *array;
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
     * configuration for all the following threads
     */
    pthread_mutex_t              mutex;
    /**
     * Transactional profile associated to the threads of this process (it
     * must be an heap allocated variable)
     */
    char                        *profile;
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
     * Transaction managers' configuration
     */
    struct trnmgr_config_array_s trnmgrs;
};

typedef struct client_config_coll_s client_config_coll_t;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize a new "object" of type client config
     * @param cc OUT object reference
     * @return a standardized return code
     */
    int client_config_coll_init(client_config_coll_t *ccc);

    

    /**
     * The retrieve the configuration of the transaction manager must be
     * used by the calling thread
     * @param ccc IN configuration object reference
     * @param tc OUT reference to the configuration of the transaction manager
     * @return a standardized return code
     */
    int client_config_coll_get_trnmgr(const client_config_coll_t *ccc,
                                      struct trnmgr_config_s **tc);


    
    /**
     * Load configuration from environment vars and XML files
     * @param ccc OUT the object will contain the client configuration
     * @return a standardized return code
     */
    int client_config(client_config_coll_t *ccc);

    

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
     * Parse a "profiles" node tree
     * @param ccc OUT server configuration structure
     * @param a_node IN the current subtree must be parsed
     * @return a standardized return code
     */
    int client_parse_profiles(struct client_config_coll_s *ccc,
                             xmlNode *a_node);



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
