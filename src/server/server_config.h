/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef SERVER_CONFIG_H
# define SERVER_CONFIG_H



#include "config.h"


#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif



#include "server_status.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_CONFIG



/**
 * Possible operating models of the state engine inside the lixad daemon
 */
enum server_config_state_engine_e {
    /** msync based synchronization */
    STATE_ENGINE_TRADITIONAL,
    /** asynchronous journal */
    STATE_ENGINE_JOURNAL,
    /** migrate from TRADITIONAL to JOURNAL */
    STATE_ENGINE_MIGRATE
};



/**
 * Operating mode of the state engine
 */
extern enum server_config_state_engine_e SERVER_CONFIG_STATE_ENGINE;



/**
 * It contains the configuration of a listener
 */
struct listener_config_s
{
    /**
     * Socket domain for the listener
     */
    int domain;
    /**
     * Address used to listen by this listener
     */
    char *address;
    /**
     * Port used to listen by this listener
     */
    in_port_t port;
};



/**
 * It contains the configuration of all listeners
 */
struct listener_config_array_s
{
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct listener_config_s *array;
};



/**
 * It contains the configuration of a manager
 */
struct manager_config_s
{
    /**
     * Prefix path for the state files
     */
    char  *status_file;
};



/**
 * It contains the configuration of all managers
 */
struct manager_config_array_s
{
    /**
     * Number of elements
     */
    int n;
    /**
     * Elements
     */
    struct manager_config_s *array;
};



/**
 * It contains the configuration of a whole server
 */
struct server_config_s
{
    /**
     * Path of the file will contain the server pid
     */
    char                                *pid_file;
    /**
     * Minimum number of microseconds should elapse between two successive
     * synchronizations of the state file
     */
    long                                 min_elapsed_sync_time;
    /**
     * Maximum number of microseconds should not be exceeded between two
     * successive synchronizations of the state file
     */
    long                                 max_elapsed_sync_time;
    /**
     * Desired size for log files on disk (bytes): it's a soft limit
     * and it can be exceeded if necessary. It's used only by the "superfast"
     * state engine: traditional engine does not use it.
     */
    off_t                                log_size;
    /**
     * Use O_DIRECT flag for state log file open()
     */
    int                                  log_o_direct;
    /**
     * Use O_DSYNC flag for state log file open()
     */
    int                                  log_o_dsync;
    /**
     * Use O_RSYNC flag for state log file open()
     */
    int                                  log_o_rsync;
    /**
     * Use O_SYNC flag for state log file open()
     */
    int                                  log_o_sync;
    /**
     * Maximum size for buffer logs (bytes): it's an hard limit and it uses
     * RAM allocated by the server manager thread. It's used only by the
     * "superfast" state engine: traditional engine does not use it.
     */
    size_t                               max_buffer_log_size;
    /**
     * Listeners' configuration
     */
    struct listener_config_array_s       listeners;
    /**
     * Managers' configuration
     */
    struct manager_config_array_s        managers;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    

    /**
     * Read and parse server config file
     * @param[out] sc the object containing the server configuration
     * @param[out] tpa thread pipe array
     * @param[in] config_filename a filename PATH must looked at before
     *                           searching default system config file
     *                           default = NULL
     * @return a standardized return code
     */
    int server_config(struct server_config_s *sc,
                      struct thread_pipe_array_s *tpa,
                      const char *config_filename);


    
    /**
     * Unconfig the server; the primary use of this function is to clean-up
     * memory and to avoid false memory leak notification when inspecting the
     * run time behavior
     * @param[out] sc the object containing the server configuration
     * @param[out] tpa threads communication pipes
     * @param[out] tsa status of all threads
     * @param[out] srt reference to the recovery table object
     * @param stt reference to the server transaction table
     * @return a standardized return code
     */
    int server_cleanup(struct server_config_s *sc,
                       struct thread_pipe_array_s *tpa,
                       struct thread_status_array_s *tsa,
                       srvr_rcvr_tbl_t *srt, server_trans_tbl_t *stt);

    

    /**
     * Parse the configuration tree
     * @param[out] sc server configuration structure
     * @param[out] tpa thread pipe array
     * @param[in] a_node the current subtree must be parsed
     * @return a standardized return code
     */
    int server_parse(struct server_config_s *sc,
                     struct thread_pipe_array_s *tpa,
                     xmlNode *a_node);


    
    /**
     * Parse a "listener" node tree
     * @param[in,out] sc configuration structure
     * @param[in] a_node listener node
     * @return a standardized return code
     */
    int server_parse_listener(struct server_config_s *sc,
                              xmlNode *a_node);

    

    /**
     * Parse a "manager" node tree
     * @param[in,out] sc configuration structure
     * @param[in,out] tpa thread pipe array
     * @param[in] a_node listener node
     * @return a standardized return code
     */
    int server_parse_manager(struct server_config_s *sc,
                             struct thread_pipe_array_s *tpa,
                             xmlNode *a_node);


    
    /**
     * Initialize the configuration of the server
     * @param[out] sc the object must be initialized
     * @param[out] tpa the array of pipes used for thread communication
     */
    void server_config_init(struct server_config_s *sc,
                            struct thread_pipe_array_s *tpa);

    

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */


#endif /* SERVER_CONFIG_H */
