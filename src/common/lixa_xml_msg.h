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
#ifndef LIXA_XML_MSG_H
# define LIXA_XML_MSG_H



#include <config.h>



#ifdef HAVE_LIBXML_TREE_H
# include <libxml/tree.h>
#endif
#ifdef HAVE_LIBXML_PARSER_H
# include <libxml/parser.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <lixa_config.h>
#include <xa.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_XML_MSG



/**
 * Default buffer size for XML messages (used for serialization/
 * deserialization)
 **/
#define LIXA_MSG_XML_BUFFER_SIZE 4096
/**
 * Initial array size for resource managers related arrays; this is not the
 * max fixed size, only an initial size to avoid reallocation for 1, 2 & 3
 * configured resource managers
 */
#define LIXA_MSG_XML_DEFAULT_RSRMGRS  3
/**
 * Number of digits prefix an XML message
 */
#define LIXA_MSG_XML_PREFIX_DIGITS  6



/**
 * Current protocol level; it's used to recognize incompatible client/server
 * configuration at run-time
 */
#define LIXA_MSG_LEVEL          0
/**
 * Id reserved for a null message
 */
#define LIXA_MSG_VERB_NULL      0
/**
 * Id assigned to verb "open"
 */
#define LIXA_MSG_VERB_OPEN      1
/**
 * Id assigned to verb "open"
 */
#define LIXA_MSG_VERB_CLOSE     2
/**
 * Id assigned to verb "start"
 */
#define LIXA_MSG_VERB_START     3
/**
 * Id assigned to verb "end"
 */
#define LIXA_MSG_VERB_END       4
/**
 * Id assigned to verb "prepare"
 */
#define LIXA_MSG_VERB_PREPARE   5
/**
 * Id assigned to verb "commit"
 */
#define LIXA_MSG_VERB_COMMIT    6
/**
 * Id assigned to verb "rollback"
 */
#define LIXA_MSG_VERB_ROLLBACK  7
/**
 * Id assigned to verb "query recovery"
 */
#define LIXA_MSG_VERB_QRCVR     8

/**
 * Default increment for message step
 */
#define LIXA_MSG_STEP_INCR      8



/**
 * Label used to specify initial XML header
 */
extern const xmlChar *LIXA_XML_MSG_HEADER;
/**
 * Label used to specify "commit" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_COMMIT;
/**
 * Label used to specify "config_digest" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_CONFIG_DIGEST;
/**
 * Label used to specify "finished" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_FINISHED;
/**
 * Label used to specify "flags" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_FLAGS;
/**
 * Label used to specify "job" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_JOB;
/**
 * Label used to specify "level" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_LEVEL;
/**
 * Label used to specify "name" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_NAME;
/**
 * Label used to specify "next_verb" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_NEXT_VERB;
/**
 * Label used to specify "rc" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_RC;
/**
 * Label used to specify "rmid" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_RMID;
/**
 * Label used to specify "r_state" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_R_STATE;
/**
 * Label used to specify "s_state" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_S_STATE;
/**
 * Label used to specify "t_state" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_T_STATE;
/**
 * Label used to specify "txstate" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_TXSTATE;
/**
 * Label used to specify "step" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_STEP;
/**
 * Label used to specify "verb" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_VERB;
/**
 * Label used to specify "will_commit" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_WILL_COMMIT;
/**
 * Label used to specify "will_rollback" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_WILL_ROLLBACK;
/**
 * Label used to specify "xa_info" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_XA_INFO;
/**
 * Label used to specify "xa_name" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_XA_NAME;
/**
 * Label used to specify "xid" property
 */
extern const xmlChar *LIXA_XML_MSG_PROP_XID;
/**
 * Label used to specify "answer" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_ANSWER;
/**
 * Label used to specify "client" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_CLIENT;
/**
 * Label used to specify "conthr" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_CONTHR;
/**
 * Label used to specify "last_verb_step" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_LAST_VERB_STEP;
/**
 * Label used to specify "msg" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_MSG;
/**
 * Label used to specify "rsrmgr" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_RSRMGR;
/**
 * Label used to specify "rsrmgrs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_RSRMGRS;
/**
 * Label used to specify "state" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_STATE;
/**
 * Label used to specify "xa_commit_exec" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_COMMIT_EXEC;
/**
 * Label used to specify "xa_commit_execs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_COMMIT_EXECS;
/**
 * Label used to specify "xa_end_exec" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_END_EXEC;
/**
 * Label used to specify "xa_end_execs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_END_EXECS;
/**
 * Label used to specify "xa_open_exec" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXEC;
/**
 * Label used to specify "xa_open_execs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_OPEN_EXECS;
/**
 * Label used to specify "xa_prepare_exec" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_PREPARE_EXEC;
/**
 * Label used to specify "xa_prepare_execs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_PREPARE_EXECS;
/**
 * Label used to specify "xa_rollback_exec" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_ROLLBACK_EXEC;
/**
 * Label used to specify "xa_rollback_execs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_ROLLBACK_EXECS;
/**
 * Label used to specify "xa_start_exec" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_START_EXEC;
/**
 * Label used to specify "xa_start_execs" tag
 */
extern const xmlChar *LIXA_XML_MSG_TAG_XA_START_EXECS;



/**
 * The communication protocol is discrete and the values are in the set
 * (verb x step)
 */
struct lixa_msg_verb_step_s {
    /**
     * Specifies the verb (open, close, begin, commit, ecc...)
     */
    int verb;
    /**
     * Specifies the step inside the verb (1, 2, 3, ...)
     */
    int step; 
};



/**
 * Mandatory header for every message encoded as @ref lixa_msg_s
 */
struct lixa_msg_header_s {
    /**
     * Protocol level must be applied to this message
     */
    int                         level;
    /**
     * Protocol verb and step of the message
     */
    struct lixa_msg_verb_step_s pvs;
};

 

/**
 * Generic answer message struct
 */
struct lixa_msg_body_answer_s {
    /**
     * Return code of the invoked operation
     */
    int rc;
};



/**
 * Convenience struct for @ref lixa_msg_body_open_8_s
 */
struct lixa_msg_body_open_8_client_s {
    xmlChar           *job;
    md5_digest_hex_t   config_digest;
};

    

/**
 * Convenience struct for @ref lixa_msg_body_open_8_s
 */
struct lixa_msg_body_open_8_rsrmgr_s {
    /**
     * rmid parameter as passed to xa_open routine
     */
    int        rmid;
    /**
     * name of the resource manager as configured for Lixa
     */
    xmlChar   *name;
    /**
     * name of the resource manager as stored in XA switch data structure
     */
    xmlChar   *xa_name;
};

    

/**
 * Message body for verb "open", step "8"
 */
struct lixa_msg_body_open_8_s {
    struct lixa_msg_body_open_8_client_s   client;
    GArray                                *rsrmgrs;
};



/**
 * Message body for verb "open", step "16"
 */
struct lixa_msg_body_open_16_s {
    struct lixa_msg_body_answer_s   answer;
};



/**
 * Control thread status
 */
struct lixa_msg_body_open_24_conthr_s {
    /**
     * State of the control thread
     */
    int                       txstate;
};



/**
 * Convenience struct for @ref lixa_msg_body_open_24_s
 */
struct lixa_msg_body_open_24_xa_open_execs_s {
    /**
     * rmid parameter as passed to xa_open routine
     */
    int             rmid;
    /**
     * xa_info parameter as passed to xa_open routine
     */
    xmlChar        *xa_info;
    /**
     * flags parameter as passed to xa_open routine
     */
    long            flags;
    /**
     * return code of xa_open routine
     */
    int             rc;
    /**
     * the new resource manager state after xa_open execution
     */
    int             r_state;
};



/**
 * Message body for verb "open", step "24"
 */
struct lixa_msg_body_open_24_s {
    /**
     * Control thread information
     */
    struct lixa_msg_body_open_24_conthr_s   conthr;
    /**
     * Parameters and return value of xa_open executions
     */
    GArray                                 *xa_open_execs;
};



/**
 * Convenience struct for @ref lixa_msg_body_open_8_s
 */
struct lixa_msg_body_close_8_rsrmgr_s {
    /**
     * rmid parameter as passed to xa_close routine
     */
    int        rmid;
};

    

/**
 * Message body for verb "close", step "8"
 */
struct lixa_msg_body_close_8_s {
    GArray                   *rsrmgrs;
};



/**
 * Convenience struct for @ref lixa_msg_body_start_8_s
 */
struct lixa_msg_body_start_8_conthr_s {
    /**
     * Transaction id
     */
    XID   xid;
};

    

/**
 * Convenience struct for @ref lixa_msg_body_start_8_s
 */
struct lixa_msg_body_start_8_rsrmgr_s {
    /**
     * rmid parameter as passed to xa_start routine
     */
    int        rmid;
};

    

/**
 * Message body for verb "start", step "8"
 */
struct lixa_msg_body_start_8_s {
    struct lixa_msg_body_start_8_conthr_s   conthr;
    GArray                                 *rsrmgrs;
};



/**
 * Message body for verb "start", step "16"
 */
struct lixa_msg_body_start_16_s {
    struct lixa_msg_body_answer_s   answer;
};



/**
 * Control thread status
 */
struct lixa_msg_body_start_24_conthr_s {
    /**
     * State of the control thread
     */
    int                       txstate;
};



/**
 * Convenience struct for @ref lixa_msg_body_start_24_s
 * xid is not stored in this structure because it was already stored by the
 * server after receiving step 8 message, see @ref lixa_msg_body_start_8_s
 */
struct lixa_msg_body_start_24_xa_start_execs_s {
    /**
     * rmid parameter as passed to xa_start routine
     */
    int             rmid;
    /**
     * flags parameter as passed to xa_start routine
     */
    long            flags;
    /**
     * return code of xa_start routine
     */
    int             rc;
    /**
     * the new transaction branch association state associated to the resource
     * manager after xa_end execution
     */
    int             t_state;
};



/**
 * Message body for verb "start", step "24"
 */
struct lixa_msg_body_start_24_s {
    /**
     * Control thread information
     */
    struct lixa_msg_body_start_24_conthr_s   conthr;
    /**
     * Parameters and return value of xa_start executions
     */
    GArray                                  *xa_start_execs;
};



/**
 * Convenience struct for @ref lixa_msg_body_end_8_s
 */
struct lixa_msg_body_end_8_conthr_s {
    /**
     * TRUE = commit
     * FALSE = rollback
     */
    int   commit;
};

    

/**
 * Message body for verb "end", step "8"
 */
struct lixa_msg_body_end_8_s {
    struct lixa_msg_body_end_8_conthr_s   conthr;
};



/**
 * Message body for verb "end", step "16"
 */
struct lixa_msg_body_end_16_s {
    struct lixa_msg_body_answer_s   answer;
};



/**
 * Convenience struct for @ref lixa_msg_body_end_24_s
 * xid is not stored in this structure because it was already stored by the
 * server after receiving step 8 message, see @ref lixa_msg_body_end_8_s
 */
struct lixa_msg_body_end_24_xa_end_execs_s {
    /**
     * rmid parameter as passed to xa_end routine
     */
    int             rmid;
    /**
     * flags parameter as passed to xa_end routine
     */
    long            flags;
    /**
     * return code of xa_end routine
     */
    int             rc;
    /**
     * the new transaction branch association state associated to the resource
     * manager after xa_end execution
     */
    int             t_state;
    /**
     * the new transaction branch state associated to the resource
     * manager after xa_end execution
     */
    int             s_state;
};



/**
 * Message body for verb "end", step "24"
 */
struct lixa_msg_body_end_24_s {
    /**
     * Parameters and return value of xa_end executions
     */
    GArray                                  *xa_end_execs;
};



/**
 * Control thread status
 */
struct lixa_msg_body_prepare_8_conthr_s {
    /**
     * TRUE = commit
     * FALSE = rollback
     */
    int   commit;
};



/**
 * Convenience struct for @ref lixa_msg_body_prepare_8_s
 * xid is not stored in this structure because it was already stored by the
 * server after receiving step 8 message, see @ref lixa_msg_body_prepare_8_s
 */
struct lixa_msg_body_prepare_8_xa_prepare_execs_s {
    /**
     * rmid parameter as passed to xa_prepare routine
     */
    int             rmid;
    /**
     * flags parameter as passed to xa_prepare routine
     */
    long            flags;
    /**
     * return code of xa_prepare routine
     */
    int             rc;
    /**
     * the new transaction branch state associated to the resource
     * manager after xa_prepare execution
     */
    int             s_state;
    /**
     * the new transaction branch association state associated to the resource
     * manager after xa_prepare execution
     */
    int             t_state;
};



/**
 * Message body for verb "prepare", step "8"
 */
struct lixa_msg_body_prepare_8_s {
    /**
     * Control thread information
     */
    struct lixa_msg_body_prepare_8_conthr_s   conthr;
    /**
     * Parameters and return value of xa_prepare executions
     */
    GArray                                   *xa_prepare_execs;
};



/**
 * Message body for verb "prepare", step "16"
 */
struct lixa_msg_body_prepare_16_s {
    struct lixa_msg_body_answer_s   answer;
};



/**
 * Control thread status
 */
struct lixa_msg_body_commit_8_conthr_s {
    /**
     * TRUE = yes
     * FALSE = no
     */
    int   finished;
};



/**
 * Convenience struct for @ref lixa_msg_body_commit_8_s
 * xid is not stored in this structure because it was already stored by the
 * server after receiving step 8 message, see @ref lixa_msg_body_commit_8_s
 */
struct lixa_msg_body_commit_8_xa_commit_execs_s {
    /**
     * rmid parameter as passed to xa_commit routine
     */
    int             rmid;
    /**
     * flags parameter as passed to xa_commit routine
     */
    long            flags;
    /**
     * return code of xa_commit routine
     */
    int             rc;
    /**
     * the new resource manager state after xa_commit execution
     */
    int             r_state;
    /**
     * the new transaction branch state after xa_commit execution
     */
    int             s_state;
};



/**
 * Message body for verb "commit", step "8"
 */
struct lixa_msg_body_commit_8_s {
    /**
     * Control thread information
     */
    struct lixa_msg_body_commit_8_conthr_s    conthr;
    /**
     * Parameters and return value of xa_commit executions
     */
    GArray                                   *xa_commit_execs;
};



/**
 * Control thread status
 */
struct lixa_msg_body_rollback_8_conthr_s {
    /**
     * TRUE = yes
     * FALSE = no
     */
    int   finished;
};



/**
 * Convenience struct for @ref lixa_msg_body_rollback_8_s
 * xid is not stored in this structure because it was already stored by the
 * server after receiving step 8 message, see @ref lixa_msg_body_rollback_8_s
 */
struct lixa_msg_body_rollback_8_xa_rollback_execs_s {
    /**
     * rmid parameter as passed to xa_rollback routine
     */
    int             rmid;
    /**
     * flags parameter as passed to xa_rollback routine
     */
    long            flags;
    /**
     * return code of xa_rollback routine
     */
    int             rc;
    /**
     * the new resource manager state after xa_rollback execution
     */
    int             r_state;
    /**
     * the new transaction branch state after xa_rollback execution
     */
    int             s_state;
};



/**
 * Message body for verb "rollback", step "8"
 */
struct lixa_msg_body_rollback_8_s {
    /**
     * Control thread information
     */
    struct lixa_msg_body_rollback_8_conthr_s    conthr;
    /**
     * Parameters and return value of xa_rollback executions
     */
    GArray                                   *xa_rollback_execs;
};



/**
 * Convenience struct for @ref lixa_msg_body_qrcvr_8_s
 */
struct lixa_msg_body_qrcvr_8_client_s {
    xmlChar           *job;
    md5_digest_hex_t   config_digest;
};

    

/**
 * Message body for verb "qrcvr", step "8"
 */
struct lixa_msg_body_qrcvr_8_s {
    struct lixa_msg_body_qrcvr_8_client_s client;
};



/**
 * Convenience struct for @ref lixa_msg_body_qrcvr_16_s
 */
struct lixa_msg_body_qrcvr_16_client_s {
    xmlChar           *job;
    md5_digest_hex_t   config_digest;
};

    

/**
 * Convenience struct for @ref lixa_msg_body_qrcvr_16_s
 */
struct lixa_msg_body_qrcvr_16_state_s {
    /**
     * Boolean: did the transaction finish?
     */
    int      finished;
    /**
     * Client TX state
     */
    int      txstate;
    /**
     * Boolean: did the transaction asked commit?
     */
    int      will_commit;
    /**
     * Boolean: did the transaction asked rollback?
     */
    int      will_rollback;
    /**
     * Transaction id
     */
    XID      xid;
};

    




/**
 * Convenience struct for @ref lixa_msg_body_qrcvr_16_s
 */
struct lixa_msg_body_qrcvr_16_rsrmgr_s {
    /**
     * rmid parameter as passed to xa_close routine
     */
    int        rmid;
    /**
     * next expected verb at crash time
     */
    int        next_verb;
    /**
     * the resource manager state at crash time
     */
    int        r_state;
    /**
     * the transaction branch state at crash time
     */
    int        s_state;
    /**
     * the transaction branch association state associated to the resource
     * manager at crash time
     */
    int        t_state;
};

    

/**
 * Message body for verb "qrcvr", step "16"
 */
struct lixa_msg_body_qrcvr_16_s {
    struct lixa_msg_body_qrcvr_16_client_s   client;
    struct lixa_msg_verb_step_s              last_verb_step;
    struct lixa_msg_body_qrcvr_16_state_s    state;
    GArray                                  *rsrmgrs;
};



/**
 * This structure maps the messages flowing between LIXA client (lixac) and
 * LIXA server (lixad). The struct is not used for the transmission over the
 * network, but only inside the client and the server.
 * This is a "fake" object; it's defined and used in the hope of simplicity
 */
struct lixa_msg_s {
    /**
     * Message header, common to all messages
     */
    struct lixa_msg_header_s                   header;
    /**
     * Message body, it depends from header
     */
    union {
        struct lixa_msg_body_open_8_s          open_8;
        struct lixa_msg_body_open_16_s         open_16;
        struct lixa_msg_body_open_24_s         open_24;
        struct lixa_msg_body_close_8_s         close_8;
        struct lixa_msg_body_start_8_s         start_8;
        struct lixa_msg_body_start_16_s        start_16;
        struct lixa_msg_body_start_24_s        start_24;
        struct lixa_msg_body_end_8_s           end_8;
        struct lixa_msg_body_end_16_s          end_16;
        struct lixa_msg_body_end_24_s          end_24;
        struct lixa_msg_body_prepare_8_s       prepare_8;
        struct lixa_msg_body_prepare_16_s      prepare_16;
        struct lixa_msg_body_commit_8_s        commit_8;
        struct lixa_msg_body_rollback_8_s      rollback_8;
        struct lixa_msg_body_qrcvr_8_s         qrcvr_8;
        struct lixa_msg_body_qrcvr_16_s        qrcvr_16;
    } body;
};



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Retrieve the first XML message from a TCP/IP socket (file descriptor)
     * @param fd IN file descriptor associated to the TCP/IP socket
     * @param buf OUT buffer will be used to store the XML message
     * @param buf_size IN size of buf
     * @param read_bytes OUT number of bytes read, XML message length
     * @return a reason code
     */
    int lixa_msg_retrieve(int fd,
                          char *buf, size_t buf_size,
                          ssize_t *read_bytes);


    
    /**
     * Free all the dynamically allocated strings previously allocated by
     * @ref lixa_msg_deserialize using xmlGetProp method
     * @param msg IN/OUT the message must be massaged
     * @return a reason code
     */
    int lixa_msg_free(struct lixa_msg_s *msg);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_XML_MSG_H */
