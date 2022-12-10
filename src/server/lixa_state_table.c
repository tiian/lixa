/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
#include "config.h"



#ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
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
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif



#include "lixa_errors.h"
#include "lixa_trace.h"
#include "lixa_state_table.h"
#include "lixa_syslog.h"
#include "server_status.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



extern gboolean run_as_daemon;
const char *LIXA_STATE_TABLE_SUFFIX = ".table";



const char *lixa_state_table_status_string(
    enum lixa_state_table_status_e status)
{
    switch (status) {
        case STATE_TABLE_UNDEFINED:   return "UNDEFINED";        break;
        case STATE_TABLE_FORMATTED:   return "FORMATTED";        break;
        case STATE_TABLE_OPENED:      return "OPENED";           break;
        case STATE_TABLE_USED:        return "USED";             break;
        case STATE_TABLE_FULL:        return "FULL";             break;
        case STATE_TABLE_EXTENDING:   return "EXTENDING";        break;
        case STATE_TABLE_COPY_SOURCE: return "COPY from SOURCE"; break;
        case STATE_TABLE_COPY_TARGET: return "COPY to TARGET";   break;
        case STATE_TABLE_SYNCH:       return "SYNCH";            break;
        case STATE_TABLE_CLOSED:      return "CLOSED";           break;
        case STATE_TABLE_DISPOSED:    return "DISPOSED";         break;
        case STATE_TABLE_CORRUPTED:   return "CORRUPTED";        break;
        default:                      return "???";
    }
    return "!!!";
}



int lixa_state_table_init(lixa_state_table_t *this,
                          const char *pathname, int read_only)
{
    enum Exception {
        NULL_OBJECT1,
        NULL_OBJECT2,
        INVALID_STATUS,
        STRDUP_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_init: pathname='%s'\n", pathname));
    TRY {
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT1);
        if (NULL == pathname)
            THROW(NULL_OBJECT2);
        /* check the state table has not been already used */
        if (STATE_TABLE_UNDEFINED != this->status) {
            LIXA_TRACE(("lixa_state_table_init: status=%d\n", this->status));
            THROW(INVALID_STATUS);
        }
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_table_t));
        /* keep a local copy of the pathname */
        if (NULL == (this->pathname = strdup(pathname)))
            THROW(STRDUP_ERROR);
        this->read_only = read_only;
        if (read_only)
            this->flags = O_RDONLY;
        else
            this->flags = O_RDWR;
        this->fd = LIXA_NULL_FD;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT1:
            case NULL_OBJECT2:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case STRDUP_ERROR:
                ret_cod = LIXA_RC_STRDUP_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_init/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_create_new_file(lixa_state_table_t *this,
                                     uint32_t number_of_blocks)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        OPEN_ERROR,
        GETTIMEOFDAY_ERROR,
        STATE_TABLE_SLOT_SYNC_ERROR,
        WRITE_ERROR,
        SET_STATUS_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_create_new_file(number_of_blocks="
                UINT32_T_FORMAT ")\n", number_of_blocks));
    TRY {
        int i;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check the status */
        if (STATE_TABLE_UNDEFINED != this->status) {
            LIXA_TRACE(("lixa_state_table_create_new_file: status should be "
                        "%d, but it's %d instead\n", STATE_TABLE_UNDEFINED,
                        this->status));
            THROW(INVALID_STATUS);
        }
        if (-1 == (this->fd = open(this->pathname, O_RDWR | O_CREAT | O_EXCL,
                              S_IRUSR | S_IWUSR | S_IRGRP)))
                THROW(OPEN_ERROR);
        LIXA_TRACE(("lixa_state_table_create_new_file: created new status "
                    "file '%s' with file descriptor %d\n",
                    this->pathname, this->fd));
        for (i = 0; i < number_of_blocks; ++i) {
            lixa_state_slot_t tmp_slot;

            memset(&tmp_slot, 0, sizeof(tmp_slot));
            if (!i) {
                /* write control record */
                tmp_slot.sr.ctrl.magic_number = STATUS_FILE_MAGIC_NUMBER;
                tmp_slot.sr.ctrl.level = STATUS_FILE_LEVEL;
                if (LIXA_RC_OK != (ret_cod = gettimeofday(
                                       &tmp_slot.sr.ctrl.last_sync, NULL)))
                    THROW(GETTIMEOFDAY_ERROR);
                tmp_slot.sr.ctrl.number_of_blocks = LIXA_STATE_TABLE_INIT_SIZE;
                tmp_slot.sr.ctrl.first_used_block = 0;
                tmp_slot.sr.ctrl.first_free_block = 1;
            } else {
                if (i == LIXA_STATE_TABLE_INIT_SIZE-1)
                    tmp_slot.sr.data.next_block = 0;
                else
                    tmp_slot.sr.data.next_block = i + 1;
            }
            if (LIXA_RC_OK != (ret_cod = lixa_state_slot_sync(&tmp_slot)))
                THROW(STATE_TABLE_SLOT_SYNC_ERROR);
            if (sizeof(tmp_slot) != write(
                    this->fd, &tmp_slot, sizeof(tmp_slot)))
                THROW(WRITE_ERROR);
        } /* for (i = 0; i < number_of_blocks; ++i) */

        /* set new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_FORMATTED, FALSE)))
            THROW(SET_STATUS_ERROR);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case OPEN_ERROR:
                if (!run_as_daemon)
                    fprintf(stderr, "Error while opening file '%s' "
                            "(errno=%d '%s')\n",
                            this->pathname, errno, strerror(errno));
                else
                    LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD025E,
                                 this->pathname, errno, strerror(errno)));
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case STATE_TABLE_SLOT_SYNC_ERROR:
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
                break;
            case SET_STATUS_ERROR:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_create_new_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_open_file(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        OPEN_ERROR,
        FSTAT_ERROR,
        MMAP_ERROR,
        CHECK_INTEGRITY,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_open_file\n"));
    TRY {
        struct stat fd_stat;
        enum lixa_state_table_status_e new_status;
        uint32_t number_of_slots;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_OPENED, TRUE))) {
            LIXA_TRACE(("lixa_state_table_open_file: status %d does not "
                        "allow open operation\n", this->status));
            THROW(INVALID_STATUS);
        }
        
        LIXA_SYSLOG((LOG_DEBUG, LIXA_SYSLOG_LXD063D, this->pathname));
        /* open the file descriptor */
        if (-1 == (this->fd = open(this->pathname, this->flags))) {
            LIXA_TRACE(("lixa_state_table_open_file: open('%s')=%d "
                        "(%s)\n", this->pathname, errno, strerror(errno)));
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD064W,
                         this->pathname, errno, strerror(errno)));
            THROW(OPEN_ERROR);
        }
        /* retrieve file size */
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR);
        number_of_slots = fd_stat.st_size/sizeof(lixa_state_slot_t);
        /* map the file */
        if (MAP_FAILED == (
                this->map = mmap(
                    NULL, fd_stat.st_size, PROT_READ | PROT_WRITE,
                    this->read_only ? MAP_PRIVATE : MAP_SHARED,
                    this->fd, 0)))
            THROW(MMAP_ERROR);
        LIXA_TRACE(("lixa_state_table_open_file: state table file '%s' mapped "
                    "at address %p, number_of_slots=" UINT32_T_FORMAT "\n",
                    this->pathname, this->map, number_of_slots));
            
        /* analyze the content */
        ret_cod = lixa_state_table_check_integrity(this);
        if (LIXA_RC_OK == ret_cod)
            new_status = STATE_TABLE_OPENED;
        else if (LIXA_RC_OBJ_CORRUPTED == ret_cod ||
                 LIXA_RC_INVALID_MAGIC_NUMBER == ret_cod) {
            new_status = STATE_TABLE_CORRUPTED;
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD079W,
                         lixa_state_table_get_pathname(this)));
        } else
            THROW(CHECK_INTEGRITY);
        
        /* move status to OPENED */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, new_status, FALSE)))
            THROW(SET_STATUS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case CHECK_INTEGRITY:
            case SET_STATUS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_open_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_check_integrity(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_MAGIC_NUMBER,
        CRC_DOES_NOT_MATCH1,
        REFRESH_CHECKSUMS,
        OVERALL_CRC_DOES_NOT_MATCH,
        CRC_DOES_NOT_MATCH2,
        DAMAGED_CHAINS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_check_integrity\n"));
    TRY {
        lixa_state_slot_t *first_slot = NULL;
        uint32_t tmp_crc32;
        uint32_t i, free_blocks, used_blocks;
        char iso_timestamp[ISO_TIMESTAMP_BUFFER_SIZE];
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* check the integrity of first slot */
        first_slot = &this->map[0];
        tmp_crc32 = lixa_crc32((const uint8_t *)&first_slot->sr,
                               sizeof(lixa_state_record_t));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    " first block content:\n"));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [magic_number] = "
                    UINT32_T_FORMAT "\n", first_slot->sr.ctrl.magic_number));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [level] = "
                    UINT32_T_FORMAT "\n", first_slot->sr.ctrl.level));
        lixa_utils_iso_timestamp(&first_slot->sr.ctrl.last_sync,
                                 iso_timestamp, sizeof(iso_timestamp));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [last_sync] = %s\n", iso_timestamp));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [last_record_id] = " LIXA_WORD_T_FORMAT "\n",
                    first_slot->sr.ctrl.last_record_id));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [checksum] = " UINT32_T_XFORMAT "\n",
                    first_slot->sr.ctrl.checksum));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [number_of_blocks] = " UINT32_T_FORMAT "\n",
                    first_slot->sr.ctrl.number_of_blocks));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [first_used_block] = " UINT32_T_FORMAT "\n",
                    first_slot->sr.ctrl.first_used_block));
        LIXA_TRACE(("lixa_state_table_check_integrity:"
                    "   [first_free_block] = " UINT32_T_FORMAT "\n",
                    first_slot->sr.ctrl.first_free_block));
        if (STATUS_FILE_MAGIC_NUMBER != first_slot->sr.ctrl.magic_number) {
            LIXA_TRACE(("lixa_state_table_check_integrity: invalid magic "
                        "number, it should be " UINT32_T_FORMAT "\n",
                        STATUS_FILE_MAGIC_NUMBER));
            THROW(INVALID_MAGIC_NUMBER);
        }
        if (tmp_crc32 != first_slot->crc32) {
            LIXA_TRACE(("lixa_state_table_check_integrity: the CRC of the "
                        "first block does not match: computed="
                        UINT32_T_XFORMAT ", in file=" UINT32_T_XFORMAT "\n",
                        tmp_crc32, first_slot->crc32));
            THROW(CRC_DOES_NOT_MATCH1);
        }
        /* refresh the overall checksum */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_refresh_checksums(
                               this, first_slot->sr.ctrl.number_of_blocks)))
            THROW(REFRESH_CHECKSUMS);
        if (first_slot->sr.ctrl.checksum != this->checksums[0]) {
            LIXA_TRACE(("lixa_state_table_check_integrity: the overall CRC "
                        "does not match: current=" UINT32_T_XFORMAT
                        ", expected=" UINT32_T_XFORMAT "\n",
                        this->checksums[0],
                        first_slot->sr.ctrl.checksum));
            THROW(OVERALL_CRC_DOES_NOT_MATCH);
        }   
        /* check data blocks */
        for (i=1; i<first_slot->sr.ctrl.number_of_blocks; ++i) {
            LIXA_TRACE(("lixa_state_table_check_integrity: checking block # "
                        UINT32_T_FORMAT "\n", i));
            /* compute status record CRC */
            tmp_crc32 = lixa_crc32((const uint8_t *)&this->map[i].sr,
                                   sizeof(lixa_state_record_t));
            if (this->map[i].crc32 != tmp_crc32) {
                LIXA_TRACE(("lixa_state_table_check_integrity: the CRC of "
                            "block " UINT32_T_FORMAT " does not match: "
                            "computed=" UINT32_T_XFORMAT ", in file="
                            UINT32_T_XFORMAT "\n", i, tmp_crc32,
                            first_slot->crc32));
                THROW(CRC_DOES_NOT_MATCH2);
            }
        } /* for (i=1; i<first_slot->sr.ctrl.number_of_blocks; ++i) */
        /* check used blocks chain */
        LIXA_TRACE(("lixa_state_table_check_integrity: checking used blocks "
                    "chain...\n"));
        used_blocks = 0;
        if (0 != (i = first_slot->sr.ctrl.first_used_block)) {
            used_blocks++;
            /* traverse the chain */
            while (0 != (i = this->map[i].sr.data.next_block))
                used_blocks++;
        }
        LIXA_TRACE(("lixa_state_table_check_integrity: there are "
                    UINT32_T_FORMAT " data used blocks\n", used_blocks));
        
        /* check free blocks chain */
        LIXA_TRACE(("lixa_state_table_check_integrity: checking free blocks "
                    "chain...\n"));
        free_blocks = 0;
        if (0 != (i = first_slot->sr.ctrl.first_free_block)) {
            free_blocks++;
            /* traverse the chain */
            while (0 != (i = this->map[i].sr.data.next_block))
                free_blocks++;
        }
        LIXA_TRACE(("lixa_state_table_check_integrity: there are "
                    UINT32_T_FORMAT " data free blocks\n", free_blocks));
        if (used_blocks + free_blocks + 1 !=
            first_slot->sr.ctrl.number_of_blocks) {
            LIXA_TRACE(("lixa_state_table_check_integrity: used blocks ("
                        UINT32_T_FORMAT ") + free blocks (" UINT32_T_FORMAT
                        ") + 1 != number of blocks (" UINT32_T_FORMAT ")\n",
                        used_blocks, free_blocks,
                        first_slot->sr.ctrl.number_of_blocks));
            THROW(DAMAGED_CHAINS);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_MAGIC_NUMBER:
                ret_cod = LIXA_RC_INVALID_MAGIC_NUMBER;
                break;
            case CRC_DOES_NOT_MATCH1:
            case OVERALL_CRC_DOES_NOT_MATCH:
            case CRC_DOES_NOT_MATCH2:
            case DAMAGED_CHAINS:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case REFRESH_CHECKSUMS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_check_integrity/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_close(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        NOTHING_TO_DO,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_close\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);

        /* check if the operation must be skipped because useless */
        if (STATE_TABLE_UNDEFINED == this->status ||
            STATE_TABLE_DISPOSED == this->status ||
            STATE_TABLE_CLOSED == this->status) {
            LIXA_TRACE(("lixa_state_table_close: nothing to do, status is "
                        "%d:%s\n", this->status,
                        lixa_state_table_status_string(this->status)));
            THROW(NOTHING_TO_DO);
        }

        /* set new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_CLOSED, FALSE)))
            THROW(SET_STATUS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NOTHING_TO_DO:
                ret_cod = LIXA_RC_OK;
                break;
            case SET_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_shutdown(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        STATE_SLOT_SYNC_ERROR1,
        REFRESH_CHECKSUMS,
        STATE_SLOT_SYNC_ERROR2,
        MUNMAP_ERROR,
        CLOSE_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_shutdown\n"));
    TRY {
        uint32_t i;
        lixa_state_slot_t *slot;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
                
        /* unmap the table */
        if (NULL != this->map) {
            /*
             * This synchronization allows a restart with RPO=0 in the event
             * that proper shutdown is asked
             */
            /* synchronize all slots but the first one */
            for (i=1; i<this->map[0].sr.ctrl.number_of_blocks; ++i) {
                if (LIXA_RC_OK != (
                        ret_cod = lixa_state_slot_sync(&this->map[i])))
                    THROW(STATE_SLOT_SYNC_ERROR1);
            }
            /* refresh the array with the checksums */
            if (LIXA_RC_OK != (
                    ret_cod = lixa_state_table_refresh_checksums(
                        this, this->map[0].sr.ctrl.number_of_blocks)))
                THROW(REFRESH_CHECKSUMS);
            /* update the overall checksum */
            if (this->map[0].sr.ctrl.checksum != this->checksums[0]) {
                this->map[0].sr.ctrl.checksum = this->checksums[0];
                /* synchronize the first slot */
                slot = &this->map[0];
                if (LIXA_RC_OK != (ret_cod = lixa_state_slot_sync(slot)))
                    THROW(STATE_SLOT_SYNC_ERROR2);
            }
            /* unmap the file */
            LIXA_TRACE(("lixa_state_table_shutdown: unmapping fd=%d, "
                        "from %p\n", this->fd, this->map));
            if (0 != munmap(this->map, this->map[0].sr.ctrl.number_of_blocks
                            * sizeof(lixa_state_slot_t)))
                THROW(MUNMAP_ERROR);
            this->map = NULL;
        }
        /* close the file descriptor */
        if (LIXA_NULL_FD != this->fd) {
            LIXA_TRACE(("lixa_state_table_shutdown: closing fd=%d, '%s'\n",
                        this->fd, this->pathname));
            if (-1 == close(this->fd)) {
                LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXD046E,
                             lixa_state_table_get_pathname(this)));
                THROW(CLOSE_ERROR);
            }
            this->fd = LIXA_NULL_FD;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_SLOT_SYNC_ERROR1:
            case REFRESH_CHECKSUMS:
            case STATE_SLOT_SYNC_ERROR2:
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* anyway, set the file descriptor to null */
        if (excp > CLOSE_ERROR)
            this->fd = LIXA_NULL_FD;
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_shutdown/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_map(lixa_state_table_t *this, int read_only)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        FSTAT_ERROR,
        MMAP_ERROR,
        REFRESH_CHECKSUMS,
        SYNC_SLOT,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_map\n"));
    TRY {
        struct stat fd_stat;
        uint32_t number_of_slots;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_USED, TRUE))) {
            LIXA_TRACE(("lixa_state_table_map: status %d does not "
                        "allow map operation\n", this->status));
            THROW(INVALID_STATUS);
        }
        /* retrieve file size */
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR);
        number_of_slots = fd_stat.st_size/sizeof(lixa_state_slot_t);
        /* map the file */
        if (NULL == (this->map =
                     mmap(NULL, fd_stat.st_size,
                          PROT_READ | PROT_WRITE,
                          this->read_only ? MAP_PRIVATE : MAP_SHARED,
                          this->fd, 0)))
            THROW(MMAP_ERROR);
        LIXA_TRACE(("lixa_state_table_map: state table file '%s' mapped at "
                    "address %p, number_of_slots=" UINT32_T_FORMAT "\n",
                    this->pathname, this->map, number_of_slots));
        /* refresh the array with the checksums */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_refresh_checksums(
                               this, number_of_slots)))
            THROW(REFRESH_CHECKSUMS);
        /* refresh the CRC of the first block */
        if (this->map[0].sr.ctrl.checksum != this->checksums[0]) {
            this->map[0].sr.ctrl.checksum = this->checksums[0];
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_sync_slot(this, 0)))
                THROW(SYNC_SLOT);
        }
        /* move status to USED */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_USED, FALSE)))
            THROW(SET_STATUS);

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case REFRESH_CHECKSUMS:
            case SYNC_SLOT:
            case SET_STATUS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_map/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_file_exist(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_file_exist\n"));
    TRY {
        int fd;
        
        /* check the object is not null */
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* try to open the file with the same flags of a real usage */
        if (-1 == (fd = open(this->pathname, this->flags)))
            THROW(OPEN_ERROR);
        close(fd);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_file_exist/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_clean(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_clean\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        if (STATE_TABLE_UNDEFINED == this->status) {
            LIXA_TRACE(("lixa_state_table_clean: WARNING, status is "
                        "UNDEFINED!\n"));
        }
        /* release allocated memory */
        if (NULL != this->pathname)
            free(this->pathname);
        if (NULL != this->checksums)
            free(this->checksums);
        /* reset everything, bye bye... */
        memset(this, 0, sizeof(lixa_state_table_t));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_clean/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_extend(lixa_state_table_t *this,
                            GArray *changed_block_ids)
{
    enum Exception {
        NULL_OBJECT,
        SET_STATUS1,
        OPEN_ERROR,
        FSTAT_ERROR1,
        MUNMAP_ERROR,
        CONTAINER_FULL,
        TRUNCATE_ERROR,
        LSEEK_ERROR,
        WRITE_ERROR,
        FSTAT_ERROR2,
        MMAP_ERROR,
        REFRESH_CHECKSUMS,
        SET_STATUS2,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_extend\n"));
    TRY {
        struct stat fd_stat;
        uint32_t curr_size, new_size, delta_size, i;
        off_t curr_size_bytes, new_size_bytes;
        uint32_t tmp_block_id;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_EXTENDING, FALSE)))
            THROW(SET_STATUS1);
        /* re-open the file for append: new records must be added */
        if (-1 == (this->fd = open(this->pathname, this->flags | O_APPEND))) {
            LIXA_TRACE(("lixa_state_table_extend: error while opening "
                        "state table file '%s' (errno=%d)\n",
                        this->pathname, errno));
            THROW(OPEN_ERROR);
        }
        /* retrieve current size */
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR1);
        /* unmap the current mapping */
        if (0 != munmap(this->map, fd_stat.st_size))
            THROW(MUNMAP_ERROR);
        this->map = NULL;
        /* compute the new size for the state table */
        curr_size = fd_stat.st_size / sizeof(lixa_state_slot_t);
        delta_size = fd_stat.st_size / sizeof(lixa_state_slot_t) *
            LIXA_STATE_TABLE_DELTA_SIZE / 100;
        if (0 == delta_size)
            delta_size++;
        new_size = curr_size + delta_size;
        /* check reached size */
        if (new_size > UINT32_MAX) {
            LIXA_TRACE(("lixa_state_table_extend: new size after "
                        "resizing would exceed " UINT32_T_FORMAT
                        " max value; "
                        "resized to max value only\n", UINT32_MAX));
            new_size = UINT32_MAX;
            delta_size = new_size - curr_size;
            if (delta_size == 0)
                THROW(CONTAINER_FULL);
        }
        new_size_bytes = new_size * sizeof(lixa_state_slot_t);
        curr_size_bytes = curr_size * sizeof(lixa_state_slot_t);
        LIXA_TRACE(("lixa_state_table_extend: curr_size = " UINT32_T_FORMAT
                    " (" OFF_T_FORMAT "), new_size = " UINT32_T_FORMAT " ("
                    OFF_T_FORMAT ")\n", curr_size, curr_size_bytes,
                    new_size, new_size_bytes));

        /* extend the file to new size */
        /* move file pointer to old size */
        if (curr_size_bytes != lseek(this->fd, curr_size_bytes, SEEK_SET))
            THROW(LSEEK_ERROR);
        /* append the new records */
        for (i = 0; i < delta_size; ++i) {
            lixa_state_slot_t tmp_slot;

            memset(&tmp_slot, 0, sizeof(tmp_slot));
            if (i == delta_size - 1)
                tmp_slot.sr.data.next_block = 0;
            else
                tmp_slot.sr.data.next_block = curr_size + i + 1;
            LIXA_TRACE(("lixa_state_table_extend: new block "
                        UINT32_T_FORMAT " (" UINT32_T_FORMAT ") "
                        "next_block = " UINT32_T_FORMAT "\n",
                        i, i + curr_size, tmp_slot.sr.data.next_block));
            if (sizeof(tmp_slot) != write(this->fd, &tmp_slot,
                                          sizeof(tmp_slot)))
                THROW(WRITE_ERROR);
        } /* for (i = 0; i < delta_size; ++i) */
        
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR2);
        /* map the state table file again */
        if (MAP_FAILED == (this->map = mmap(NULL, fd_stat.st_size,
                                            PROT_READ | PROT_WRITE, MAP_SHARED,
                                            this->fd, 0)))
            THROW(MMAP_ERROR);
        /* update free block list */
        this->map[0].sr.ctrl.first_free_block = curr_size;
        this->map[0].sr.ctrl.number_of_blocks = new_size;
        LIXA_TRACE(("lixa_state_table_extend: state table file '%s' of "
                    OFF_T_FORMAT " bytes mapped at address %p, "
                    "number_of_slots=" UINT32_T_FORMAT "\n",
                    this->pathname, fd_stat.st_size, this->map,
                    this->map[0].sr.ctrl.number_of_blocks));
        /* refresh overall checksum */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_refresh_checksums(
                               this, this->map[0].sr.ctrl.number_of_blocks)))
            THROW(REFRESH_CHECKSUMS);       
        /* first block must be marked for change */
        tmp_block_id = 0;
        g_array_append_val(changed_block_ids, tmp_block_id);
        /* all the added blocks must be marked for change */
        for (i=0; i<delta_size; ++i) {
            tmp_block_id = curr_size + i;
            g_array_append_val(changed_block_ids, tmp_block_id);
        }
        /* set the new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_USED, FALSE)))
            THROW(SET_STATUS2);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SET_STATUS1:
                break;
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case OPEN_ERROR:
                ret_cod = LIXA_RC_OPEN_ERROR;
                break;
            case FSTAT_ERROR1:
            case FSTAT_ERROR2:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case CONTAINER_FULL:
                ret_cod = LIXA_RC_CONTAINER_FULL;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case LSEEK_ERROR:
                ret_cod = LIXA_RC_LSEEK_ERROR;
                break;
            case WRITE_ERROR:
                ret_cod = LIXA_RC_WRITE_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case REFRESH_CHECKSUMS:
            case SET_STATUS2:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_extend/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_set_status(lixa_state_table_t *this,
                                enum lixa_state_table_status_e new_status,
                                int dry_run)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_set_status(new_status=%d:%s, "
                "dry_run=%d)\n", new_status,
                lixa_state_table_status_string(new_status), dry_run));
    TRY {
        int valid = TRUE;
        
        if (NULL == this)
            THROW(NULL_OBJECT);

        LIXA_TRACE(("lixa_state_table_set_status: current status is %d:%s\n",
                    this->status,
                    lixa_state_table_status_string(this->status)));
        switch (new_status) {
            case STATE_TABLE_UNDEFINED:
                LIXA_TRACE(("lixa_state_table_set_status: transition to "
                            "UNDEFINED status is never acceptable\n"));
                valid = FALSE;
                break;
            case STATE_TABLE_FORMATTED:
                if (STATE_TABLE_UNDEFINED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "FORMATTED is acceptable only from "
                                "UNDEFINED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_OPENED:
                if (STATE_TABLE_UNDEFINED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "OPENED is acceptable only from UNDEFINED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_USED:
                if (STATE_TABLE_COPY_TARGET != this->status &&
                    STATE_TABLE_FORMATTED != this->status &&
                    STATE_TABLE_OPENED != this->status &&
                    STATE_TABLE_EXTENDING != this->status &&
                    STATE_TABLE_FULL != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "USED is acceptable only from COPY_TARGET, "
                                "FORMATTED, OPENED, EXTENDED and FULL\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_FULL:
                if (STATE_TABLE_USED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "FULL is acceptable only from USED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_EXTENDING:
                if (STATE_TABLE_FULL != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "EXTENDING is acceptable only from FULL\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_COPY_SOURCE:
                if (STATE_TABLE_USED != this->status &&
                    STATE_TABLE_FULL != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "COPY_SOURCE is acceptable only from USED"
                                " and FULL\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_COPY_TARGET:
                if (STATE_TABLE_FORMATTED != this->status &&
                    STATE_TABLE_DISPOSED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "COPY_TARGET is acceptable only from "
                                "FORMATTED and DISPOSED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_SYNCH:
                if (STATE_TABLE_COPY_SOURCE != this->status &&
                    STATE_TABLE_USED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "SYNCH is acceptable only from "
                                "USED and COPY_SOURCE\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_CLOSED:
                if (STATE_TABLE_FORMATTED != this->status &&
                    STATE_TABLE_SYNCH != this->status &&
                    STATE_TABLE_OPENED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "CLOSED is acceptable only from "
                                "FORMATTED, SYNCH, and OPENED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_DISPOSED:
                if (STATE_TABLE_CLOSED != this->status &&
                    STATE_TABLE_CORRUPTED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "DISPOSED is acceptable only from "
                                "CLOSED and CORRUPTED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_CORRUPTED:
                if (STATE_TABLE_UNDEFINED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "CORRUPTED is acceptable only from "
                                "UNDEFINED\n"));
                    valid = FALSE;
                }
                break;
            default:
                LIXA_TRACE(("lixa_state_table_set_status: %d:%s is not a "
                            "valid value for a the new_status\n", new_status,
                            lixa_state_table_status_string(new_status)));
                valid = FALSE;
                break;
        } /* switch (new_status) */
        if (!valid) {
            THROW(INVALID_STATUS);
        } else if (dry_run) {
            LIXA_TRACE(("lixa_state_table_set_status: dry run, status can be "
                        "switched from %d:%s to %d:%s\n",
                        this->status,
                        lixa_state_table_status_string(this->status),
                        new_status,
                        lixa_state_table_status_string(new_status)));
        } else {
            LIXA_TRACE(("lixa_state_table_set_status: %d:%s -> %d:%s\n",
                        this->status,
                        lixa_state_table_status_string(this->status),
                        new_status,
                        lixa_state_table_status_string(new_status)));
            this->status = new_status;
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_set_status/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_insert_block(lixa_state_table_t *this,
                                  uint32_t *block_id,
                                  GArray *changed_block_ids)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_insert_block\n"));
    TRY {
        uint32_t tmp;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (STATE_TABLE_USED != this->status ||
            LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_FULL, TRUE))) {
            LIXA_TRACE(("lixa_state_table_insert_block: status %d:%s does not "
                        "allow insert_block operation\n", this->status,
                        lixa_state_table_status_string(this->status)));
            THROW(INVALID_STATUS);
        }

#ifdef LIXA_DEBUG
        lixa_state_slot_trace_lists(this->map);
#endif /* LIXA_DEBUG */
        
        LIXA_TRACE(("lixa_state_table_insert_block: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT "\n",
                    this->map[0].sr.ctrl.first_free_block,
                    this->map[0].sr.ctrl.first_used_block));
        *block_id = this->map[0].sr.ctrl.first_free_block;
        this->map[0].sr.ctrl.first_free_block =
            this->map[*block_id].sr.data.next_block;
        this->map[*block_id].sr.data.next_block =
            this->map[0].sr.ctrl.first_used_block;
        tmp = *block_id;
        g_array_append_val(changed_block_ids, tmp);
        this->map[0].sr.ctrl.first_used_block = *block_id;
        tmp = 0;
        g_array_append_val(changed_block_ids, tmp);
        LIXA_TRACE(("lixa_state_table_insert_block: first_free_block = "
                    UINT32_T_FORMAT ", first_used_block = "
                    UINT32_T_FORMAT ", last inserted next block = "
                    UINT32_T_FORMAT "\n",
                    this->map[0].sr.ctrl.first_free_block,
                    this->map[0].sr.ctrl.first_used_block,
                    this->map[*block_id].sr.data.next_block));
#ifdef LIXA_DEBUG
        lixa_state_slot_trace_lists(this->map);
#endif /* LIXA_DEBUG */
        
        /* is the state table now full? */
        if (0 == this->map[0].sr.ctrl.first_free_block)
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                                   this, STATE_TABLE_FULL, FALSE)))
                THROW(SET_STATUS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case SET_STATUS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_insert_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_delete_block(lixa_state_table_t *this,
                                  uint32_t block_id,
                                  GArray *changed_block_ids)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        USED_BLOCK_NOT_FOUND,
        OBJ_CORRUPTED,
        SET_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_delete_block(block_id=" UINT32_T_FORMAT
                ")\n", block_id));
    TRY {
        uint32_t tmp;
        uint32_t ul; /* used list left block */
        uint32_t ur; /* used list right block */
        uint32_t fl; /* free list left block */
        uint32_t fr; /* free list right block */
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* check status */
        if (STATE_TABLE_USED != this->status &&
            STATE_TABLE_FULL != this->status) {
            LIXA_TRACE(("lixa_state_table_delete_block: status %d:%s does not "
                        "allow delete_block operation\n", this->status,
                        lixa_state_table_status_string(this->status)));
            THROW(INVALID_STATUS);
        }

#ifdef LIXA_DEBUG
        lixa_state_slot_trace_lists(this->map);
#endif /* LIXA_DEBUG */
        
        /* find ul and ur positions */
        ul = 0;
        ur = this->map[0].sr.ctrl.first_used_block;
        LIXA_TRACE(("lixa_state_table_delete_block: ul=" UINT32_T_FORMAT
                    ", ur=" UINT32_T_FORMAT "\n", ul, ur));
        while (ur > 0) {
            if (ur == block_id)
                break;
            ul = ur;
            ur = this->map[ur].sr.data.next_block;
#ifdef LIXA_DEBUG
            LIXA_TRACE(("lixa_state_table_delete_block: ul=" UINT32_T_FORMAT
                        ", ur=" UINT32_T_FORMAT "\n", ul, ur));
#endif /* LIXA_DEBUG */
        }
        if (ur == 0)
            THROW(USED_BLOCK_NOT_FOUND);
        
        /* find fl and fr positions */
        fl = 0;
        fr = this->map[0].sr.ctrl.first_free_block;
        while (fr > 0) {
            if (fr > block_id)
                break;
            else if (fr == block_id)
                THROW(OBJ_CORRUPTED);
            fl = fr;
            fr = this->map[fr].sr.data.next_block;
        }

        LIXA_TRACE(("lixa_state_table_delete_block: ul=" UINT32_T_FORMAT
                    ", ur=" UINT32_T_FORMAT ", fl=" UINT32_T_FORMAT
                    ", fr=" UINT32_T_FORMAT "\n", ul, ur, fl, fr));
        
        /* remove block from used block list */
        if (ul == 0) {
            /* first block in used block list */
            this->map[0].sr.ctrl.first_used_block =
                this->map[ur].sr.data.next_block;
            tmp = 0;
            g_array_append_val(changed_block_ids, tmp);
        } else {
            /* central or last block in used block list */
            this->map[ul].sr.data.next_block =
                this->map[ur].sr.data.next_block;
            g_array_append_val(changed_block_ids, ul);
        }
        this->map[ur].sr.data.next_block = 0;
        g_array_append_val(changed_block_ids, ur);

        /* insert block in free block list */
        if (fl == 0) {
            /* insertion happens at list head or list is empty */
            if (fr != 0) {
                /* list is not empty */
                this->map[ur].sr.data.next_block = fr;
                g_array_append_val(changed_block_ids, ur);
            }
            this->map[0].sr.ctrl.first_free_block = ur;
            tmp = 0;
            g_array_append_val(changed_block_ids, tmp);
        } else {
            /* insertion happens in the middle or at list tail */
            this->map[ur].sr.data.next_block = fr;
            g_array_append_val(changed_block_ids, ur);
            this->map[fl].sr.data.next_block = ur;
            g_array_append_val(changed_block_ids, fl);
        }
        
#ifdef LIXA_DEBUG
        lixa_state_slot_trace_lists(this->map);
#endif /* LIXA_DEBUG */
        
        /* is the state table now not full? */
        if (STATE_TABLE_FULL == this->status &&
            0 != this->map[0].sr.ctrl.first_free_block)
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                                   this, STATE_TABLE_USED, FALSE)))
                THROW(SET_STATUS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case INVALID_STATUS:
                ret_cod = LIXA_RC_INVALID_STATUS;
                break;
            case USED_BLOCK_NOT_FOUND:
                ret_cod = LIXA_RC_OBJ_NOT_FOUND;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case SET_STATUS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_delete_block/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_copy_from(lixa_state_table_t *this,
                               const lixa_state_table_t *source)
{
    enum Exception {
        NULL_OBJECT,
        FSTAT_ERROR1,
        FSTAT_ERROR2,
        MUNMAP_ERROR,
        TRUNCATE_ERROR,
        FSTAT_ERROR3,
        INTERNAL_ERROR,
        MMAP_ERROR,
        REFRESH_CHECKSUMS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_copy_from\n"));
    TRY {
        struct stat fd_stat_this;
        struct stat fd_stat_source;
        
        if (NULL == this || NULL == source)
            THROW(NULL_OBJECT);
        LIXA_TRACE(("lixa_state_table_copy_from: this={fd=%d,'%s'}, "
                    "source={fd=%d,'%s'}\n",
                    this->fd, this->pathname,
                    source->fd, source->pathname));
        /* retrieve the size of the two state table */
        if (0 != fstat(this->fd, &fd_stat_this))
            THROW(FSTAT_ERROR1);
        if (0 != fstat(source->fd, &fd_stat_source))
            THROW(FSTAT_ERROR2);
        /* check if the state files differ */
        if (fd_stat_this.st_size != fd_stat_source.st_size) {
            LIXA_TRACE(("lixa_state_table_copy_from: this state table file is "
                        OFF_T_FORMAT " bytes long, source state table file is "
                        OFF_T_FORMAT " bytes long\n", fd_stat_this.st_size,
                        fd_stat_source.st_size));
            /* check if this state file is mapped */
            if (NULL != this->map) {
                if (0 != munmap(this->map, fd_stat_this.st_size))
                    THROW(MUNMAP_ERROR);
                this->map = NULL;
            }
            /* truncate the state file to the desired length, if necessary */
            if (fd_stat_this.st_size != fd_stat_source.st_size) {
                LIXA_TRACE(("lixa_state_table_copy_from: truncating this "
                            "state table file to " OFF_T_FORMAT " bytes\n",
                            fd_stat_source.st_size));
                if (0 != ftruncate(this->fd, fd_stat_source.st_size))
                    THROW(TRUNCATE_ERROR);
            }
            /* retrieve the size again */
            if (0 != fstat(this->fd, &fd_stat_this))
                THROW(FSTAT_ERROR3);
            if (fd_stat_this.st_size != fd_stat_source.st_size)
                THROW(INTERNAL_ERROR);
        }
        /* map the new state file if necessary */
        if (NULL == this->map &&
            MAP_FAILED == (this->map = mmap(NULL, fd_stat_source.st_size,
                                            PROT_READ | PROT_WRITE,
                                            MAP_SHARED, this->fd, 0)))
            THROW(MMAP_ERROR);
        /* copy the content */
        memcpy(this->map, source->map, (size_t)fd_stat_source.st_size);
        /* refresh the array with the checksums */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_refresh_checksums(
                               this, fd_stat_source.st_size/
                               sizeof(lixa_state_slot_t))))
            THROW(REFRESH_CHECKSUMS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case FSTAT_ERROR1:
            case FSTAT_ERROR2:
            case FSTAT_ERROR3:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case INTERNAL_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case REFRESH_CHECKSUMS:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_copy_from/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_sync_map(lixa_state_table_t *this, int last_sync)
{
    enum Exception {
        NULL_OBJECT,
        TABLE_SET_STATUS1,
        GETTIMEOFDAY_ERROR,
        FSTAT_ERROR,
        OBJ_CORRUPTED,
        STATE_SLOT_SYNC_ERROR1,
        REFRESH_CHECKSUMS,
        STATE_SLOT_SYNC_ERROR2,
        MSYNC_ERROR,
        TABLE_SET_STATUS2,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_sync_map\n"));
    TRY {
        struct stat fd_stat;
        lixa_timer_t timer;
        long duration;
        lixa_state_slot_t *slot;
        uint32_t i;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* set the new status of the object */
        if (!last_sync &&
            LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_SYNCH, FALSE)))
            THROW(TABLE_SET_STATUS1);

        /* update last_sync timestamp */
        slot = &this->map[0];
        if (LIXA_RC_OK != (ret_cod = gettimeofday(
                               &slot->sr.ctrl.last_sync, NULL)))
            THROW(GETTIMEOFDAY_ERROR); 
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR);

        if (fd_stat.st_size/sizeof(lixa_state_slot_t) !=
            this->map[0].sr.ctrl.number_of_blocks) {
            LIXA_TRACE(("lixa_state_table_sync_map: file '%s' is " OFF_T_FORMAT
                        " bytes long, but map is of " UINT32_T_FORMAT
                        " slots (" OFF_T_FORMAT " bytes)\n",
                        this->pathname, fd_stat.st_size,
                        this->map[0].sr.ctrl.number_of_blocks,
                        (off_t)this->map[0].sr.ctrl.number_of_blocks*
                        sizeof(lixa_state_slot_t)));
            THROW(OBJ_CORRUPTED);
        }
        
        LIXA_TRACE(("lixa_state_table_sync_map: SYNCHING "
                    OFF_T_FORMAT " bytes to file '%s'\n", fd_stat.st_size,
                    this->pathname));
        
        /* synchronize all slots but the first one */
        for (i=1; i<this->map[0].sr.ctrl.number_of_blocks; ++i) {
            slot = &this->map[i];
            if (LIXA_RC_OK != (ret_cod = lixa_state_slot_sync(slot)))
                THROW(STATE_SLOT_SYNC_ERROR1);
        }
        /* refresh the array with the checksums */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_refresh_checksums(
                               this, this->map[0].sr.ctrl.number_of_blocks)))
            THROW(REFRESH_CHECKSUMS);
        /* update the overall checksum */
        this->map[0].sr.ctrl.checksum = this->checksums[0];
        /* synchronize the first slot */
        slot = &this->map[0];
        if (LIXA_RC_OK != (ret_cod = lixa_state_slot_sync(slot)))
            THROW(STATE_SLOT_SYNC_ERROR2);
        
        lixa_timer_start(&timer);
        if (0 != msync(this->map, fd_stat.st_size, MS_SYNC))
            THROW(MSYNC_ERROR);
        lixa_timer_stop(&timer);
        duration = lixa_timer_get_diff(&timer);
        LIXA_TRACE(("lixa_state_table_sync_map: synchronization of mapped "
                    "memory to the underlying file '%s' required %ld us\n",
                    this->pathname, duration));
        /* transform micro seconds to milli seconds */
        duration /= 1000;
        /* log a message if performance is not good */
        if (duration > 500) { /* half second :( */
            LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXD059W, duration,
                         fd_stat.st_size, this->pathname));
        } else if (duration > 50) { /* 50 ms */
            LIXA_SYSLOG((LOG_NOTICE, LIXA_SYSLOG_LXD060N, duration,
                         fd_stat.st_size, this->pathname));
        } else if (duration > 5) { /* 5 ms */
            LIXA_SYSLOG((LOG_INFO, LIXA_SYSLOG_LXD061I, duration,
                         fd_stat.st_size, this->pathname));
        }
        /* set to CLOSED the status of this table */
        if (!last_sync &&
            LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_CLOSED, FALSE)))
            THROW(TABLE_SET_STATUS2);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case TABLE_SET_STATUS1:
            case TABLE_SET_STATUS2:
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case STATE_SLOT_SYNC_ERROR1:
            case REFRESH_CHECKSUMS:
            case STATE_SLOT_SYNC_ERROR2:
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case OBJ_CORRUPTED:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case MSYNC_ERROR:
                ret_cod = LIXA_RC_MSYNC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_sync_map/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_set_last_record_id(lixa_state_table_t *this,
                                        lixa_word_t last_record_id)
{
    enum Exception {
        NULL_OBJECT,
        GETTIMEOFDAY_ERROR,
        STATE_SLOT_SYNC,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_set_last_record_id(last_record_id="
                LIXA_WORD_T_FORMAT ")\n", last_record_id));
    TRY {
        struct timeval timestamp;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* retrieve the current timestamp */
        if (0 != gettimeofday(&timestamp, NULL))
            THROW(GETTIMEOFDAY_ERROR);
        this->map[0].sr.ctrl.last_record_id = last_record_id;
        this->map[0].sr.ctrl.last_sync = timestamp;
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_sync_slot(this, 0)))
            THROW(STATE_SLOT_SYNC);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case STATE_SLOT_SYNC:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_set_last_record_id/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_patch_slot(lixa_state_table_t *this,
                                uint32_t block_id,
                                const lixa_state_record_t *sr)
{
    enum Exception {
        NULL_OBJECT,
        FSTAT_ERROR,
        MUNMAP_ERROR,
        TRUNCATE_ERROR,
        MMAP_ERROR,
        REFRESH_CHECKSUMS,
        STATE_SLOT_SYNC,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_patch_slot(block_id=" UINT32_T_FORMAT ")\n",
                block_id));
    TRY {
        struct stat fd_stat;
        uint32_t curr_size, new_size;
        
        if (NULL == this || NULL == sr)
            THROW(NULL_OBJECT);
        
        /* retrieve current size */
        if (0 != fstat(this->fd, &fd_stat))
            THROW(FSTAT_ERROR);
        /* check the file is big enough... */
        curr_size = fd_stat.st_size / sizeof(lixa_state_slot_t);
        if (block_id >= curr_size)
            new_size = block_id+1;
        else
            new_size = curr_size;
        /* if necessary, extend the file */
        if (new_size != curr_size) {
            if (0 != munmap(this->map, fd_stat.st_size))
                THROW(MUNMAP_ERROR);
            this->map = NULL;
            /* extend the file */
            if (0 != ftruncate(this->fd, new_size*sizeof(lixa_state_slot_t)))
                THROW(TRUNCATE_ERROR);
            /* map the state table file again */
            if (MAP_FAILED == (
                    this->map = mmap(NULL, fd_stat.st_size,
                                     PROT_READ | PROT_WRITE, MAP_SHARED,
                                     this->fd, 0)))
                THROW(MMAP_ERROR);
            /* refresh the array with the checksums */
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_refresh_checksums(
                                   this, new_size)))
                THROW(REFRESH_CHECKSUMS);
        }
        /* replacing the content */
        memcpy(&this->map[block_id].sr, sr, sizeof(lixa_state_record_t));
        /* resyncing the CRC of the record */
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_table_sync_slot(this, block_id)))
            THROW(STATE_SLOT_SYNC);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case FSTAT_ERROR:
                ret_cod = LIXA_RC_FSTAT_ERROR;
                break;
            case TRUNCATE_ERROR:
                ret_cod = LIXA_RC_TRUNCATE_ERROR;
                break;
            case MUNMAP_ERROR:
                ret_cod = LIXA_RC_MUNMAP_ERROR;
                break;
            case MMAP_ERROR:
                ret_cod = LIXA_RC_MMAP_ERROR;
                break;
            case REFRESH_CHECKSUMS:
            case STATE_SLOT_SYNC:
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_patch_slot/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_sync_slot(lixa_state_table_t *this,
                               uint32_t block_id)
{
    enum Exception {
        NULL_OBJECT,
        STATE_SLOT_SYNC1,
        REFRESH_CHECKSUMS,
        GETTIMEOFDAY_ERROR,
        STATE_SLOT_SYNC2,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    LIXA_TRACE(("lixa_state_table_sync_slot(block_id=" UINT32_T_FORMAT ")\n",
                block_id));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        if (LIXA_RC_OK != (
                ret_cod = lixa_state_slot_sync(&this->map[block_id])))
            THROW(STATE_SLOT_SYNC1);
        /* re-compute the overall checksum if block_id is not 0 */
        if (block_id) {
            /* allocate the checksums array if null */
            if (NULL == this->checksums &&
                LIXA_RC_OK != (
                    ret_cod = lixa_state_table_refresh_checksums(
                        this, this->map[0].sr.ctrl.number_of_blocks)))
                THROW(REFRESH_CHECKSUMS);       
            this->checksums[block_id] =
                lixa_state_slot_get_crc32(&this->map[block_id]);
            this->checksums[0] = lixa_crc32(
                (const uint8_t *)(this->checksums+1),
                (this->map[0].sr.ctrl.number_of_blocks - 1) *
                sizeof(uint32_t));
            if (this->map[0].sr.ctrl.checksum != this->checksums[0]) {
                this->map[0].sr.ctrl.checksum = this->checksums[0];
                /* update last_sync */
                if (LIXA_RC_OK != (ret_cod = gettimeofday(
                                       &this->map[0].sr.ctrl.last_sync, NULL)))
                    THROW(GETTIMEOFDAY_ERROR);
                /* re-compute first block checksum */
                if (LIXA_RC_OK != (
                        ret_cod = lixa_state_slot_sync(&this->map[0])))
                    THROW(STATE_SLOT_SYNC2);
            }
        } /* if (block_id) */
        LIXA_TRACE(("lixa_state_table_sync_slot: block_id=" UINT32_T_FORMAT
                    ", slot CRC32=" UINT32_T_XFORMAT ", overall CRC32="
                    UINT32_T_XFORMAT ", first block CRC32=" UINT32_T_XFORMAT
                    "\n", block_id,
                    lixa_state_slot_get_crc32(&this->map[block_id]),
                    this->map[0].sr.ctrl.checksum,
                    this->map[0].crc32));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case STATE_SLOT_SYNC1:
            case REFRESH_CHECKSUMS:
            case STATE_SLOT_SYNC2:
                break;
            case GETTIMEOFDAY_ERROR:
                ret_cod = LIXA_RC_GETTIMEOFDAY_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_sync_slot/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}



int lixa_state_table_refresh_checksums(lixa_state_table_t *this,
                                       uint32_t number_of_slots)
{
    enum Exception {
        NULL_OBJECT,
        REALLOC_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_refresh_checksums(number_of_slots="
                UINT32_T_FORMAT ")\n", number_of_slots));
    TRY {
        uint32_t i;
        
        if (NULL == this)
            THROW(NULL_OBJECT);
        /* realloc works only if necessary :) */
        if (NULL == (this->checksums = (uint32_t *)realloc(
                         this->checksums, number_of_slots * sizeof(uint32_t))))
            THROW(REALLOC_ERROR);
        /* copy all the CRCs but slot 0 */
        for (i=1; i<number_of_slots; ++i)
            this->checksums[i] = lixa_state_slot_get_crc32(&this->map[i]);
        /* put in the first position the overall CRC */
        this->checksums[0] = lixa_crc32(
            (const uint8_t *)&this->checksums[1],
            (number_of_slots - 1) * sizeof(uint32_t));
        LIXA_TRACE(("lixa_state_table_refresh_checksums: checksum of "
                    "checksums is " UINT32_T_XFORMAT "\n",
                    this->checksums[0]));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NULL_OBJECT:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case REALLOC_ERROR:
                ret_cod = LIXA_RC_REALLOC_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_refresh_checksums/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    LIXA_TRACE_STACK();
    return ret_cod;
}

