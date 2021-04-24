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
#ifndef LIXA_STATE_COMMON_H
# define LIXA_STATE_COMMON_H



#include "config.h"



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include "lixa_utils.h"
#include "status_record.h"



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_SERVER_STATUS



/**
 * Number of state tables and log files; can NOT be less than 3
 */
#define LIXA_STATE_TABLES   3



/**
 * Operations that can be sent from the main thread to the flusher thread
 */
enum lixa_state_flusher_ops_e {
    /** nothing to do, just wait on condition */
    STATE_FLUSHER_WAIT = 0,
    /** flush the buffer / mapped pages to the current used file */
    STATE_FLUSHER_FLUSH,
    /** flush for shutdown: it's the last flush on the object before exiting */
    STATE_FLUSHER_LAST_FLUSH,
    /** thread termination */
    STATE_FLUSHER_EXIT
};



/**
 * This union is used to store and retrieve a record from the status file
 */
typedef union status_record_u lixa_state_record_t;



/**
 * This struct is the basic type of a status record: it contains one control
 * field and one payload field.
 * THE POSITION OF THE FIELDS MUST NOT BE CHANGED INSIDE THE RECORD.
 * This type will replace the legacy struct status_record_s when only
 * "superfast" engine will remain
 */
typedef struct lixa_state_slot_s {
    /**
     * This field contains a control record or a data record, briefly "sr"
     * (Status Record)
     */
    lixa_state_record_t            sr;
    /**
     * This field contains the CRC32 signature of the previous fields and is
     * used to guarantee the record integrity
     */
    uint32_t                       crc32;
} lixa_state_slot_t;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Prepare a slot for synchronization: signature is computed
     * @param[in,out] slot that must be marked for update
     * @return a reason code
     */
    int lixa_state_slot_sync(lixa_state_slot_t *slot);
    
    

    /**
     * Return the record inside the slot
     */
    static inline const lixa_state_record_t *lixa_state_slot_get_record(
        const lixa_state_slot_t *slot) {
        return &slot->sr;
    }



    /**
     * Return the CRC of the slot
     */
    static inline uint32_t lixa_state_slot_get_crc32(
        const lixa_state_slot_t *slot) {
        return slot->crc32;
    }


    
    /**
     * Conversion from buffer size to number of pages
     * @param[in] buffer_size in bytes
     * @return number of pages
     */
    static inline size_t lixa_state_common_buffer2pages(size_t buffer_size)
    {
        return (buffer_size / LIXA_SYSTEM_PAGE_SIZE) +
            (buffer_size % LIXA_SYSTEM_PAGE_SIZE ? 1 : 0);
    }



    /**
     * Trace the free list and used list
     */
    void lixa_state_slot_trace_lists(const lixa_state_slot_t *lss);

    
    
    /**
     * Conversion from number of pages to buffer size in bytes
     * @param[in] number_of_pages
     * @return the buffer size in bytes
     */
    static inline size_t lixa_state_common_pages2buffer(size_t number_of_pages)
    {
        return number_of_pages * LIXA_SYSTEM_PAGE_SIZE;
    }

    

    /**
     * Compares two checkpoints composed by a record id and a timestamp
     * @param[in] first_record_id
     * @param[in] first_timeval
     * @param[in] second_record_id
     * @param[in] second_timeval
     * @return an integer less than, equal to, or greater than zero if first
     *         checkpoint is found, respectively, to be older than, to
     *         be as old as, or be younger than second checkpoint
     */
    int lixa_state_common_chkp_order(lixa_word_t first_record_id,
                                     const struct timeval *first_timeval,
                                     lixa_word_t second_record_id,
                                     const struct timeval *second_timeval);



    /**
     * Compute the successive state (table or log)
     */
    static inline int lixa_state_common_succ_state(int current) {
        return ++current >= LIXA_STATE_TABLES ? 0 : current;
    }


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_STATE_COMMON_H */
