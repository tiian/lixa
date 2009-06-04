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



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_CONFIG



/**
 * This is the path of the default server config file to search for (system
 * configuration)
 */
extern const char *LIXA_SERVER_CONFIG_DEFAULT_FILE;

/**
 * This is the path of the default client config file to search for (system
 * configuration)
 */
extern const char *LIXA_CLIENT_CONFIG_DEFAULT_FILE;

/**
 * Label used to specify "listener" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_LISTENER;

/**
 * Label used to specify "server" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_SERVER;

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
 * Label used to specify "profile" property in "trnmgr" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_PROFILE_PROPERTY;

/**
 * Label used to specify "profile" tag
 */
extern const xmlChar *LIXA_XML_CONFIG_PROFILE;

/**
 * Label used to specify "name" property
 */
extern const xmlChar *LIXA_XML_CONFIG_NAME_PROPERTY;



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
