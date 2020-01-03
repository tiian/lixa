/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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



int lixa_state_table_slot_sync(lixa_state_table_slot_t *slot)
{
    enum Exception {
        DIGEST_SIZE_ERROR,
        NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_slot_sync\n"));
    TRY {
        if (slot->counter%2) {
            slot->counter++;
        } else {
            LIXA_TRACE(("lixa_state_table_slot_sync: WARNING! record %p is "
                        "already even (it was NOT updated before!)\n", slot));
        }
        /* compute the CRC32 signature */
        slot->crc32 = lixa_crc32(
            (const uint8_t *)slot,
            sizeof(lixa_state_table_slot_t) - sizeof(uint32_t));
        
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
    LIXA_TRACE(("lixa_state_table_slot_sync/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


int lixa_state_table_init(lixa_state_table_t *this,
                         const char *pathname)
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
        /* check the state log has not been already used */
        if (STATE_TABLE_UNDEFINED != this->status)
            THROW(INVALID_STATUS);
        /* clean-up the object memory, maybe not necessary, but safer */
        memset(this, 0, sizeof(lixa_state_table_t));
        /* keep a local copy of the pathname */
        if (NULL == (this->pathname = strdup(pathname)))
            THROW(STRDUP_ERROR);
        this->flags = O_RDWR;
        
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
    return ret_cod;
}



int lixa_state_table_create_new_file(lixa_state_table_t *this)
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
    
    LIXA_TRACE(("lixa_state_table_create_new_file\n"));
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
        for (i = 0; i < LIXA_STATE_TABLE_INIT_SIZE; ++i) {
            lixa_state_table_slot_t tmp_slot;

            memset(&tmp_slot, 0, sizeof(tmp_slot));
            tmp_slot.counter = 1;
            if (!i) {
                /* write control record */
                tmp_slot.sr.ctrl.magic_number = STATUS_FILE_MAGIC_NUMBER;
                tmp_slot.sr.ctrl.level = STATUS_FILE_LEVEL;
                if (LIXA_RC_OK != (ret_cod = gettimeofday(
                                       &tmp_slot.sr.ctrl.last_sync, NULL)))
                    THROW(GETTIMEOFDAY_ERROR);
                tmp_slot.sr.ctrl.number_of_blocks = STATUS_FILE_INIT_SIZE;
                tmp_slot.sr.ctrl.first_used_block = 0;
                tmp_slot.sr.ctrl.first_free_block = 1;
            } else {
                if (i == STATUS_FILE_INIT_SIZE - 1)
                    tmp_slot.sr.data.next_block = 0;
                else
                    tmp_slot.sr.data.next_block = i + 1;
            }
            if (LIXA_RC_OK != (ret_cod = lixa_state_table_slot_sync(
                                   &tmp_slot)))
                THROW(STATE_TABLE_SLOT_SYNC_ERROR);
            if (sizeof(tmp_slot) != write(
                    this->fd, &tmp_slot, sizeof(tmp_slot)))
                THROW(WRITE_ERROR);
        }
        /* set new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_FORMATTED)))
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
    return ret_cod;
}



int lixa_state_table_synchronize(lixa_state_table_t *this)
{
    enum Exception {
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_synchronize\n"));
    TRY {
        /* @@@ Implement this function, probably some other parameter is
           necessary */
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
    LIXA_TRACE(("lixa_state_table_synchronize/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_table_close(lixa_state_table_t *this)
{
    enum Exception {
        CLOSE_ERROR,
        INVALID_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_close\n"));
    TRY {
        if (-1 == close(this->fd)) {
            THROW(CLOSE_ERROR);
        }
        /* set new status */
        if (LIXA_RC_OK != (ret_cod = lixa_state_table_set_status(
                               this, STATE_TABLE_CLOSED)))
            THROW(INVALID_STATUS);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case CLOSE_ERROR:
                ret_cod = LIXA_RC_CLOSE_ERROR;
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
        /* anyway, set the file descriptor to null */
        this->fd = LIXA_NULL_FD;
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_state_table_close/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_table_exist_file(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        OPEN_ERROR,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_exist_file\n"));
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
    LIXA_TRACE(("lixa_state_table_exist_file/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
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
    return ret_cod;
}



int lixa_state_table_extend(lixa_state_table_t *this)
{
    enum Exception {
        NULL_OBJECT,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_extend\n"));
    TRY {
        if (NULL == this)
            THROW(NULL_OBJECT);
        
        /* @@@ implement me */
        
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
    LIXA_TRACE(("lixa_state_table_extend/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_state_table_set_status(lixa_state_table_t *this,
                                enum lixa_state_table_status_e new_status)
{
    enum Exception {
        NULL_OBJECT,
        INVALID_STATUS,
        NONE
    } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_table_set_status\n"));
    TRY {
        int valid = TRUE;
        
        if (NULL == this)
            THROW(NULL_OBJECT);

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
            case STATE_TABLE_USED:
                if (STATE_TABLE_COPY_TARGET != this->status &&
                    STATE_TABLE_FORMATTED != this->status &&
                    STATE_TABLE_EXTENDED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "USED is acceptable only from COPY_TARGET, "
                                "FORMATTED and EXTENDED\n"));
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
            case STATE_TABLE_EXTENDED:
                if (STATE_TABLE_FULL != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "EXTENDED is acceptable only from FULL\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_COPY_SOURCE:
                if (STATE_TABLE_USED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "COPY_SOURCE is acceptable only from USED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_COPY_TARGET:
                if (STATE_TABLE_FORMATTED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "COPY_TARGET is acceptable only from "
                                "FORMATTED\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_SYNCH:
                if (STATE_TABLE_COPY_SOURCE != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "SYNCH is acceptable only from "
                                "COPY_SOURCE\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_CLOSED:
                if (STATE_TABLE_FORMATTED != this->status &&
                    STATE_TABLE_SYNCH != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "CLOSED is acceptable only from "
                                "FORMATTED and SYNCH\n"));
                    valid = FALSE;
                }
                break;
            case STATE_TABLE_DISPOSED:
                if (STATE_TABLE_CLOSED != this->status) {
                    LIXA_TRACE(("lixa_state_table_set_status: transition to "
                                "DISPOSED is acceptable only from "
                                "CLOSED\n"));
                    valid = FALSE;
                }
                break;
            default:
                LIXA_TRACE(("lixa_state_table_set_status: %d is not a valid "
                            "value for a the new_status\n", new_status));
                valid = FALSE;
                break;
        } /* switch (new_status) */
        if (!valid) {
            THROW(INVALID_STATUS);
        } else {
            LIXA_TRACE(("lixa_state_table_set_status: %d -> %d\n",
                        this->status, new_status));
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
    return ret_cod;
}

