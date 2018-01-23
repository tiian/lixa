/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <config.h>



#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif



#include "lixa_errors.h"
#include "lixa_common_status.h"
#include "lixa_trace.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_STATUS



void common_status_conthr_init(
    struct common_status_conthr_s *csc) {
    csc->txstate = TX_STATE_S0;
    memset(&csc->xid, 0, sizeof(XID));
    csc->xid.formatID = NULLXID;
    csc->will_commit = FALSE;
    csc->will_rollback = FALSE;
    csc->global_recovery = XTA_GLOBAL_RECOV_NULL;
    csc->finished = FALSE;
    return;
}



void common_status_conthr_display(const struct common_status_conthr_s *csc)
{
    LIXA_TRACE(("common_status_conthr_display: finished=%d, txstate=%d, "
                "will_commit=%d, will_rollback=%d, global_recovery=%d\n",
                csc->finished, csc->txstate, csc->will_commit,
                csc->will_rollback, csc->global_recovery));
    LIXA_TRACE_HEX_DATA("common_status_conthr_display: xid = ",
                        (const byte_t *)&csc->xid, sizeof(XID));
}



