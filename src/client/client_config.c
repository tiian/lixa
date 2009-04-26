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
        LIXA_TRACE(("client_status_coll_init: mutex initialization return "
                    "code: %d\n", ret_cod));
        ccc->profile = NULL;
        ccc->trnmgrs.n = 0;
        ccc->trnmgrs.array = NULL;
     
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


    
int client_config(void)
{
    enum Exception { PTHREAD_MUTEX_LOCK_ERROR
                     , ALREADY_CONFIGURED
                     , STRDUP_ERROR
                     , OPEN_CONFIG_ERROR
                     , CLOSE_ERROR
                     , XML_READ_FILE_ERROR
                     , XML_DOC_GET_ROOT_ELEMENT_ERROR
                     , PARSE_CONFIG_ERROR
                     , PTHREAD_MUTEX_UNLOCK_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int fd = 0;
    const char *file_name = NULL;
    xmlDocPtr doc;
    xmlNode *root_element = NULL;
    
    LIXA_TRACE(("client_config\n"));
    TRY {
        char *profile;
        /* lock mutex to start configuration activity */
        if (0 != (ret_cod = pthread_mutex_lock(&global_ccc.mutex)))
            THROW(PTHREAD_MUTEX_LOCK_ERROR);

        if (NULL != global_ccc.profile)
            THROW(ALREADY_CONFIGURED);
        
        if (NULL == (profile = getenv(LIXA_PROFILE_ENV_VAR))) {
            /* use empty string instead of NULL to avoid allocation issues */
            profile = "";
            LIXA_TRACE(("client_init: '%s' environment variable not found, "
                        "using default profile for this client\n",
                        LIXA_PROFILE_ENV_VAR));
        }
        LIXA_TRACE(("client_init: using transactional profile '%s' for "
                    "subsequent operations\n", profile));        

        if (NULL == (global_ccc.profile = strdup(profile)))
            THROW(STRDUP_ERROR);
        
        /* checking if available the custom config file */
        if (NULL != config_filename &&
            -1 != (fd = open(config_filename, O_RDONLY))) {
            file_name = config_filename;
        } else {
            if (-1 == (fd = open(LIXA_CLIENT_CONFIG_DEFAULT_FILE, O_RDONLY))) {
                LIXA_TRACE(("server_config/file %s is not readable, throwing "
                            "error\n", LIXA_CLIENT_CONFIG_DEFAULT_FILE));
                THROW(OPEN_CONFIG_ERROR);
            } else {
                file_name = LIXA_SERVER_CONFIG_DEFAULT_FILE;
            }
        }
        if (-1 == (ret_cod = close(fd)))
            THROW(CLOSE_ERROR);
        
        /* loading config file */
        if (NULL == (doc = xmlReadFile(file_name, NULL, 0)))
            THROW(XML_READ_FILE_ERROR);

        /* walking tree */
        if (NULL == (root_element = xmlDocGetRootElement(doc)))
            THROW(XML_DOC_GET_ROOT_ELEMENT_ERROR);

        if (LIXA_RC_OK != (ret_cod = client_parse(global_ccc, root_element)))
            THROW(PARSE_CONFIG_ERROR);
        
        /* unlock mutex to start configuration activity */
        if (0 != (ret_cod = pthread_mutex_unlock(&global_ccc.mutex)))
            THROW(PTHREAD_MUTEX_UNLOCK_ERROR);

        /* free parsed document */
        xmlFreeDoc(doc);

        /* release libxml2 stuff */
        xmlCleanupParser();

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
            ret_cod = pthread_mutex_unlock(&global_ccc.mutex);
            if (0 != ret_cod)
                LIXA_TRACE(("client_config/pthread_mutex_unlock: "
                            "ret_cod=%d/errno=%d\n", ret_cod, errno));
        }
    } /* TRY-CATCH */
    LIXA_TRACE(("client_config/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int client_parse(struct client_config_coll_s *ccc,
                 xmlNode *a_node)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("client_parse\n"));
    TRY {
        
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
    LIXA_TRACE(("client_parse/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

