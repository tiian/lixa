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



#include "lixa_errors.h"
#include "lixa_state_common.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_SERVER_STATUS



int lixa_state_slot_sync(lixa_state_slot_t *slot)
{
    enum Exception {
        NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_state_slot_sync\n"));
    TRY {
        /* compute the CRC32 signature */
        uint32_t crc32 = lixa_crc32((const uint8_t *)&slot->sr,
                                  sizeof(lixa_state_record_t));
        /*
        LIXA_TRACE(("lixa_state_slot_sync: old CRC32=" UINT32_T_XFORMAT "\n",
                    lixa_state_slot_get_crc32(slot)));
        */
        /* update only if different (don't touch the page if it's not
           necessary */
        if (slot->crc32 != crc32)
            slot->crc32 = crc32;
        /*
        LIXA_TRACE(("lixa_state_slot_sync: new CRC32=" UINT32_T_XFORMAT "\n",
                    lixa_state_slot_get_crc32(slot)));
        */
        
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
    LIXA_TRACE(("lixa_state_slot_sync/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}
    


void lixa_state_slot_trace_lists(const lixa_state_slot_t *lss)
{
    uint32_t ur; /* used list block */
    uint32_t fr; /* free list block */
    gchar *tmp_string = NULL, *string = NULL;
    gchar buffer[20];
    
    ur = lss[0].sr.ctrl.first_used_block;
    string = g_strdup("-");
    while (ur > 0) {
        snprintf(buffer, sizeof(buffer), "[" UINT32_T_FORMAT "]", ur);
        tmp_string = g_strconcat(string, buffer, NULL);
        g_free(string);
        string = tmp_string;
        ur = lss[ur].sr.data.next_block;
    }
    LIXA_TRACE(("lixa_state_slot_trace_lists: used blocks list = %s\n",
                string));
    g_free(string);

    fr = lss[0].sr.ctrl.first_free_block;
    string = g_strdup("-");
    while (fr > 0) {
        snprintf(buffer, sizeof(buffer), "[" UINT32_T_FORMAT "]", fr);
        tmp_string = g_strconcat(string, buffer, NULL);
        g_free(string);
        string = tmp_string;
        fr = lss[fr].sr.data.next_block;
    }
    LIXA_TRACE(("lixa_state_slot_trace_lists: free blocks list = %s\n",
                string));
    g_free(string);
}



int lixa_state_common_chkp_order(lixa_word_t first_record_id,
                                 const struct timeval *first_timeval,
                                 lixa_word_t second_record_id,
                                 const struct timeval *second_timeval)
{
    int result = 0;
    
    if (first_record_id == second_record_id)
        if (timercmp(first_timeval, second_timeval, <))
            result = -1; /* timeval precedence */
        else if (timercmp(first_timeval, second_timeval, >))
            result = +1; /* it's really older! */
        else
            result = 0;
    else if (first_record_id < second_record_id)
        result = -1; /* record id precedence */
    else { /* first record seems to be older than second record */
        if (first_record_id > 0xC000000 &&
            second_record_id < 0x4000000) {
            if (timercmp(first_timeval, second_timeval, <))
                result = -1; /* timeval precedence */
            else if (timercmp(first_timeval, second_timeval, >))
                result = +1; /* it's really older! */
            else
                result = +1; /* ??? tricky situation !!! */
        } else
            result = +1;
    }
    LIXA_TRACE(("lixa_state_common_chkp_order(first_record_id="
                LIXA_WORD_T_FORMAT ", first_timeval->tv_sec=%d, "
                "first_timeval->tv_usec=%d, second_record_id="
                LIXA_WORD_T_FORMAT ", second_timeval->tv_sec=%d, "
                "second_timeval->tv_usec=%d)=%d\n", first_record_id,
                (int)first_timeval->tv_sec, (int)first_timeval->tv_usec,
                second_record_id, (int)second_timeval->tv_sec,
                (int)second_timeval->tv_usec, result));
    return result;
}

