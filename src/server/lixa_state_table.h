/*
 * Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_STATE_TABLE_H
# define LIXA_STATE_TABLE_H



#include "config.h"



#ifdef HAVE_GLIB_H
#include <glib.h>
#endif



#include "lixa_trace.h"
#include "lixa_state_common.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



#define LIXA_STATE_TABLE_INIT_SIZE   2 /* STATUS_FILE_INIT_SIZE */
#define LIXA_STATE_TABLE_DELTA_SIZE  STATUS_FILE_DELTA_SIZE



/**
 * Suffix appended when a new state table is created
 */
extern const char *LIXA_STATE_TABLE_SUFFIX;



/**
 * Possible statuses of a state table
 */
enum lixa_state_table_status_e {
    STATE_TABLE_UNDEFINED = 0,
    STATE_TABLE_FORMATTED,
    STATE_TABLE_OPENED,
    STATE_TABLE_USED,
    STATE_TABLE_FULL,
    STATE_TABLE_EXTENDING,
    STATE_TABLE_COPY_SOURCE,
    STATE_TABLE_COPY_TARGET,
    STATE_TABLE_SYNCH,
    STATE_TABLE_CLOSED,
    STATE_TABLE_DISPOSED,
    STATE_TABLE_CORRUPTED
};



/**
 * LIXA State Table data type ("class")
 */
typedef struct lixa_state_table_s {
    /**
     * Current status of the state table;
     * use only @ref lixa_state_table_set_status to change it;
     * reading is allowed without restriction
     */
    enum lixa_state_table_status_e   status;
    /**
     * The file must be opened in read only mode
     */
    int                              read_only;
    /**
     * File descriptor associated to the underlying file
     */
    int                              fd;
    /**
     * Open flags associated to the underlying file
     */
    int                              flags;
    /**
     * Name of the associated underlying file
     */
    char                            *pathname;
    /**
     * Memory map associated to the underlying file
     */
    lixa_state_slot_t               *map;
    /**
     * Array with the checksum of every slot in the map; position 0 contains
     * the overall checksum (checksum of checksums)
     */
    uint32_t                        *checksums;
} lixa_state_table_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Return the human readable string associated to a status
     */
    const char *lixa_state_table_status_string(
        enum lixa_state_table_status_e status);

    
    
    /**
     * Initialize a StateTable object to manage a state table
     * @param[in,out] this object to be initialized
     * @param[in] pathname that must be used to open or create the underlying
     *            file
     * @param[in] read_only must be TRUE if the object is created only for
     *            reading state and not changing it
     * @return a reason code
     */
    int lixa_state_table_init(lixa_state_table_t *this,
                              const char *pathname, int read_only);

    

    /**
     * Create a new underlying file for the state table object
     * @param[in,out] this state table object
     * @param[in] number_of_blocks is the number of (initial) blocks in the
     *            file
     * @return a reason code
     */
    int lixa_state_table_create_new_file(lixa_state_table_t *this,
                                         uint32_t number_of_blocks);



    /**
     * Open an existing state table file
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_open_file(lixa_state_table_t *this);

    

    /**
     * Check the integrity of a state table
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_check_integrity(lixa_state_table_t *this);


    
    /**
     * Put the state table object in "closed" status
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_close(lixa_state_table_t *this);


    
    /**
     * Shutdown the state table object and make durable it
     * @param[in,out] this state table object
     * @return a reason code
     */
    int lixa_state_table_shutdown(lixa_state_table_t *this);


    
    /**
     * Use an existent and opened state table file
     * @param[in,out] this state table object
     * @param[in] read_only boolean condition that allows to map the file only
     *                      for reading
     * @return a reason code
     */
    int lixa_state_table_map(lixa_state_table_t *this, int read_only);


    
    /**
     * Check if the underlying file exists and can be opened
     * @param[in] this state table object
     * @return a reason code, LIXA_RC_OK if the file exists and can be opened
     */
    int lixa_state_table_file_exist(lixa_state_table_t *this);

    
    
    /**
     * Cleanup a StateTable object
     */
    int lixa_state_table_clean(lixa_state_table_t *this);


    
    /**
     * Return the pathname of the file associated to the state table object
     */
    static inline const char* lixa_state_table_get_pathname(
        const lixa_state_table_t *this) {
        return this->pathname;
    }

    

    /**
     * Return a boolean value: is the state file full?
     */
    static inline int lixa_state_table_is_full(const lixa_state_table_t *this)
    {
        return STATE_TABLE_FULL == this->status ||
            0 == this->map[0].sr.ctrl.first_free_block;
    }



    /**
     * Return a boolean value: is the state file in the progress of a
     * synchronization?
     */
    static inline int lixa_state_table_is_syncing(
        const lixa_state_table_t *this) {
        return STATE_TABLE_SYNCH == this->status;
    }

    
    
    /**
     * Extend the state table; this method is typically called if the state
     * table is full
     * @param[in] this state table object
     * @param[out] changed_block_ids an array with all the blocks that must
     *             be marked for log
     * @return a reason code
     */
    int lixa_state_table_extend(lixa_state_table_t *this,
                                GArray *changed_block_ids);
    
    

    /**
     * Set a new status for the state table: before setting, it checks if the
     * transition is valid or not
     * @param[in,out] this state table object
     * @param[in] new_status to be set
     * @param[in] dry_run boolean value, TRUE if function must only test the
     *            status change, FALSE if real switch must be performed
     * @return a reason code
     */
    int lixa_state_table_set_status(lixa_state_table_t *this,
                                    enum lixa_state_table_status_e new_status,
                                    int dry_run);


    
    /**
     * Return the current status of the state table
     */
    static inline enum lixa_state_table_status_e
    lixa_state_table_get_status(const lixa_state_table_t *this) {
        return this->status;
    }


    
    /**
     * Insert a new used block in the state table
     * @param[in,out] this state table object
     * @param[out] block_id of the inserted block
     * @param[out] changed_block_ids an array with all the blocks that must
     *             be marked for log
     * @return a reason code
     */
    int lixa_state_table_insert_block(lixa_state_table_t *this,
                                      uint32_t *block_id,
                                      GArray *changed_block_ids);



    /**
     * Delete a used block from the state table
     * @param[in,out] this state table object
     * @param[in] block_id of the deleted block
     * @param[out] changed_block_ids an array with all the blocks that must
     *             be marked for log
     * @return a reason code
     */
    int lixa_state_table_delete_block(lixa_state_table_t *this,
                                      uint32_t block_id,
                                      GArray *changed_block_ids);



    /**
     * Copy the content of another state table to the current one
     * @param[in,out] this state table object
     * @param[in] source state table that's used to copy from
     * @return a reason code
     */
    int lixa_state_table_copy_from(lixa_state_table_t *this,
                                   const lixa_state_table_t *source);

    
    
    /**
     * Return the reference to a read only slot of the state table
     * @param[in] this state table object
     * @param[in] block_id of the desired block
     * @return a reference to the desired slot
     */     
    static inline const lixa_state_slot_t *lixa_state_table_get_roslot(
        const lixa_state_table_t *this, uint32_t block_id) {
        return &this->map[block_id];
    }



    /**
     * Return the reference to a read write slot of the state table
     * @param[in,out] this state table object
     * @param[in] block_id of the desired block
     * @return a reference to the desired slot
     */     
    static inline lixa_state_slot_t *lixa_state_table_get_rwslot(
        lixa_state_table_t *this, uint32_t block_id) {
        return &this->map[block_id];
    }



    /**
     * Synchronize the content of the memory map to the underlying file
     * @param[in,out] this state table object
     * @param[in] last_sync is TRUE for the last synchronization operation that
     *            must be performed in any case
     * @return a reason code
     */
    int lixa_state_table_sync_map(lixa_state_table_t *this, int last_sync);



    /**
     * Save in state table header the id of a state log record that for sure
     * has been persisted till now
     * @param[in,out] this state table object
     * @param[in] last_record_id persisted in the state log
     * @return a reason code
     */
    int lixa_state_table_set_last_record_id(lixa_state_table_t *this,
                                            lixa_word_t last_record_id);



    /**
     * Getter method for last_record_id
     * @param[in] this state table object
     * @return the record id of the last synchronization
     */
    static inline lixa_word_t lixa_state_table_get_last_record_id(
        const lixa_state_table_t *this) {
        return NULL == this ? 0 : this->map[0].sr.ctrl.last_record_id;
    }


    
    /**
     * Getter method for last_sync
     * @param[in] this state table object
     * @return the timestamp of the last synchronization
     */
    static inline const struct timeval *lixa_state_table_get_last_sync(
        const lixa_state_table_t *this) {
        return NULL == this ? NULL : &this->map[0].sr.ctrl.last_sync;
    }



    /**
     * Replace the content of a slot, patching the inner record
     * @param[in,out] this state table object
     * @param[in] block_id of the desired block
     * @param[in] sr source state record
     * @return a reason code
     */
    int lixa_state_table_patch_slot(lixa_state_table_t *this,
                                    uint32_t block_id,
                                    const lixa_state_record_t *sr);

    
    /**
     * Synchronize (and sign) a slot in the state table
     * @param[in,out] this state table object
     * @param[in] block_id of the slot to be signed
     * @return a reason code
     */
    int lixa_state_table_sync_slot(lixa_state_table_t *this,
                                   uint32_t block_id);

    

    /**
     * Refresh all the checksums of the state table and compute the overall
     * CRC; it automatically reallocate the array if the number of slots
     * changed
     * @param[in,out] this state table object
     * @param[in] number_of_slots in the state table
     * @return a reason code     
     */
    int lixa_state_table_refresh_checksums(lixa_state_table_t *this,
                                            uint32_t number_of_slots);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_STATE_TABLE_H */
