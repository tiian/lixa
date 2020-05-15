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
#ifndef STATUS_RECORD_H
# define STATUS_RECORD_H



#include "config.h"



#include "lixa_common_status.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Maximum number of blocks in a chain used by a session
 */
#define CHAIN_MAX_SIZE 20
/**
 * The data record is an header
 */
#define DATA_PAYLOAD_TYPE_HEADER  1
/**
 * The data record is a resource manager block
 */
#define DATA_PAYLOAD_TYPE_RSRMGR  2
/**
 * Maximum lenght (null char terminator included) of resource manager name
 */
#define PAYLOAD_RSRMGR_NAME_MAX   30
/**
 * Number of (verb,step) values are stored by the server
 */
#define PAYLOAD_HEADER_VERB_STEP  5



/**
 * This is the control record type and is used for first record only
 */
struct status_record_ctrl_s
{
    /**
     * It's the signature of the first 4 bytes and can be used
     * by "file" utility to discover a file is a lixa status file
     */
    uint32_t       magic_number;
    /**
     * Used to distinguish status files produced and used by different
     * versions of the software
     */
    uint32_t       level;
    /**
     * TRADITIONAL engine: timestamp (seconds and microseconds) of the last
     * sync operation <br>
     * JOURNAL engine: timestamp (seconds and microseconds) of the last update
     */
    struct timeval last_sync;
    /**
     * Last record ID persisted in the state log
     */
    lixa_word_t    last_record_id;
    /**
     * Overall checksum: checksum of all checksums; used only with
     * JOURNAL mode (superfast engine)
     */
    uint32_t       checksum;
    /**
     * The total number of blocks kept by the status file. The control block
     * itself (block_id = 0) is computed as well.
     */
    uint32_t       number_of_blocks;
    /**
     * First record of the used blocks chain (0 means the chain is empty)
     */
    uint32_t       first_used_block;
    /**
     * First record of the free blocks chain (0 means the chain is empty)
     */
    uint32_t       first_free_block;
};



/**
 * This struct is used as header to concatenate other records; the header
 * must be defined for every client session. Different type of records are
 * not mandatory
 */
struct payload_header_s
{
    /**
     * Number of elements in the block array: it should ever be equal to the
     * number of the resource managers
     */
    int                           n;
    /**
     * Array of "pointers" to the blocks of this logical concatenation
     */
    uint32_t                      block_array[CHAIN_MAX_SIZE];
    /**
     * Block of the next branch in the same global transaction; this field is
     * meaningful only for global transactions with multiple branches
     */
    uint32_t                      next_branch_block;
    /**
     * Block of the previous branch in the same global transaction; this field
     * is meaningful only for global transactions with multiple branches
     */
    uint32_t                      prev_branch_block;
    /**
     * Timestamp of the client's arrival time
     */
    struct timeval                arrival_time;
    /**
     * TCP/IP coordinates of the local socket
     */
    struct sockaddr_in            local_sock_addr;
    /**
     * TCP/IP coordinates of the peer socket
     */
    struct sockaddr_in            peer_sock_addr;
    /**
     * Hex format of the MD5 digest of lixac_conf file & profile
     */
    md5_digest_hex_t              config_digest;
    /**
     * Logical JOB associated to the transaction
     */
    lixa_job_t                    job;
    /**
     * Sequence of last (verb,step) stored for the client.
     * This array is used as a circular buffer and position 0 contains ever
     * the last (verb,step)
     */
    struct lixa_msg_verb_step_s   last_verb_step[PAYLOAD_HEADER_VERB_STEP];
    /**
     * Status of the control thread is managing the transaction
     */
    struct common_status_conthr_s state;
    /**
     * Id of the block is in recovery phase (if any)
     */
    uint32_t                      recovering_block_id;
    /**
     * An attempted recovery failed
     */
    int                           recovery_failed;
    /**
     * Date and time of the recovery failed event
     */
    struct timeval                recovery_failed_time;
    /**
     * recovery phase attempted xa_commit (TRUE) or xa_rollback (FALSE);
     * meaningless if recovery phase did not happen
     */
    int                           recovery_commit;
};



/**
 * This struct is the used to store the status of a resource manager
 */
struct payload_rsrmgr_s
{
    /**
     * Resource Manager ID
     */
    int rmid;
    /**
     * Resource manager status
     */
    struct common_status_rsrmgr_s state;
    /**
     * Name of the resource manager as defined in lixac_conf.xml
     */
    char name[PAYLOAD_RSRMGR_NAME_MAX];
    /**
     * Name of the resource manager as returned by XA switch data structure
     */
    char xa_name[RMNAMESZ];
    /**
     * xa_info string configured for xa_open
     */
    char xa_open_info[MAXINFOSIZE];
    /**
     * flags value as passed to xa_open
     */
    long xa_open_flags;
    /**
     * rc value as returned from xa_open
     */
    int xa_open_rc;
    /**
     * flags value as passed to xa_start
     */
    long xa_start_flags;
    /**
     * rc value as returned from xa_start
     */
    int xa_start_rc;
    /**
     * flags value as passed to xa_end
     */
    long xa_end_flags;
    /**
     * rc value as returned from xa_end
     */
    int xa_end_rc;
    /**
     * flags value as passed to xa_prepare
     */
    long xa_prepare_flags;
    /**
     * rc value as returned from xa_prepare
     */
    int xa_prepare_rc;
    /**
     * flags value as passed to xa_commit
     */
    long xa_commit_flags;
    /**
     * rc value as returned from xa_commit
     */
    int xa_commit_rc;
    /**
     * flags value as passed to xa_rollback
     */
    long xa_rollback_flags;
    /**
     * rc value as returned from xa_rollback
     */
    int xa_rollback_rc;
    /**
     * flags value as passed to ax_reg
     */
    long ax_reg_flags;
    /**
     * rc value as returned from ax_reg
     */
    int ax_reg_rc;
    /**
     * flags value as passed to ax_unreg
     */
    long ax_unreg_flags;
    /**
     * rc value as returned from ax_unreg
     */
    int ax_unreg_rc;
    /**
     * flags value as passed to xa_forget
     */
    long xa_forget_flags;
    /**
     * rc value as returned from xa_forget
     */
    int xa_forget_rc;
    /**
     * return code of the recovery phase
     * meaningless if recovery phase did not happen
     */
    int recovery_rc;
};



/**
 * This struct is used to separate scaffolding from payload in data records;
 * all the payload data are kept by this structure
 */
struct status_record_data_payload_s
{
    /**
     * Type of payload of this record
     */
    int type;
    /**
     * The data stored by the records depend from the value of the field type
     */
    union
    {
        /**
         * Payload used for header
         */
        struct payload_header_s ph;
        /**
         * Payload used for resource manager
         */
        struct payload_rsrmgr_s rm;
    };
};



/**
 * This is the data record type and is used for all but first records
 */
struct status_record_data_s
{
    /**
     * Index of the first record in the chain (0 means the record is the last
     * in the chain)
     */
    uint32_t next_block;
    /**
     * It contains the interesting data, the persistent status of the client
     */
    struct status_record_data_payload_s pld;
};



/**
 * This union is used to store and retrieve a record from the status file
 */
union status_record_u {
    /**
     * This type of record applies to first record only: it's the control
     * block
     */
    struct status_record_ctrl_s ctrl;
    /**
     * This type of record applies to all but first record: it's the data block
     */
    struct status_record_data_s data;
};



/**
 * This struct is the basic type of a status record: it contains two control
 * fields and one payload field.
 * THE POSITION OF THE FIELDS MUST NOT BE CHANGED INSIDE THE RECORD 
 */
struct status_record_s
{
    /**
     * This field is incremented twice between a couple of sync:
     * if the value is even, the record is synched
     * if the value is odd, the record is modified
     * This is the lifetime:
     * - increment once for first update after synch
     * - increment before compute the record digest and successive synch
     */
    uint32_t counter;
    /**
     * This field contains useful control or data information
     */
    union status_record_u sr;
    /**
     * This field contains the digest of the previous fields
     */
    md5_digest_t digest;
};



/**
 * Number of bytes must be signed to check integrity
 */
#define STATUS_RECORD_CHECKSUM_SIZE (sizeof(status_record_t) -  \
                                     sizeof(md5_digest_t))



/**
 * It's defined as a type because it's used in an object oriented fashion
 */
typedef struct status_record_s status_record_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* STATUS_RECORD_H */
