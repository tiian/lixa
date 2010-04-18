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
#include <config.h>



#include <client_status.h>
#include <lixa_errors.h>
#include <lixa_trace.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



int ax_reg(int rmid, XID *xid, long flags)
/*
{
    xid->formatID = -1;
    return TM_OK;
}
*/
{
    enum Exception { STATUS_NOT_FOUND
                     , COLL_GET_CS_ERROR
                     , NONE } excp;
    int ret_cod = TMER_TMERR;
    
    LIXA_TRACE(("ax_reg: rmid=%d, xid=%p, flags=0x%lx\n", rmid, xid, flags));
    TRY {
        client_status_t *cs;
        
        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                THROW(STATUS_NOT_FOUND);
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case STATUS_NOT_FOUND:
                ret_cod = TMER_PROTO;
                break;
            case NONE:
                ret_cod = TM_OK;
                break;
            default:
                ret_cod = TMER_TMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int ax_unreg(int rmid, long flags)
{
    /* @@@ dummy implementation to be fixed: bug 2913849 */
    LIXA_TRACE(("ax_unreg: rmid=%d, flags=0x%lx\n", rmid, flags));
    return TM_OK;
}
