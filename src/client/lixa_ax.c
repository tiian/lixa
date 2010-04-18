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
    enum Exception { COLL_GET_CS_ERROR
                     , MSG_SERIALIZE_ERROR
                     , SEND_ERROR
                     , NONE } excp;
    int ret_cod = TM_OK;
    
    LIXA_TRACE(("ax_reg: rmid=%d, xid=%p, flags=0x%lx\n", rmid, xid, flags));
    TRY {
        client_status_t *cs;
        struct common_status_rsrmgr_s *csr;
        struct lixa_msg_s msg;
        size_t buffer_size = 0;
        int fd;
        char buffer[LIXA_MSG_XML_BUFFER_SIZE];
        
        /* retrieve a reference to the thread status */
        ret_cod = client_status_coll_get_cs(&global_csc, &cs);
        switch (ret_cod) {
            case LIXA_RC_OK: /* nothing to do */
                break;
            case LIXA_RC_OBJ_NOT_FOUND:
                LIXA_TRACE(("ax_reg: status not found\n"));
                ret_cod = TMER_PROTO;
                break;
            default:
                THROW(COLL_GET_CS_ERROR);
        }

        /* check the rmid value is not out of range */
        if ((TM_OK == ret_cod) &&
            (rmid < 0 || rmid >= global_ccc.actconf.rsrmgrs->len)) {
            LIXA_TRACE(("ax_reg: rmid out of range\n"));
            ret_cod = TMER_INVAL;
        }
        
        /* check the status of the resource manager */
        csr = &g_array_index(cs->rmstates, struct common_status_rsrmgr_s,
                             rmid);
        if ((TM_OK == ret_cod) && !csr->dynamic) {
            LIXA_TRACE(("ax_reg: resource manager # %d is not using "
                        "dynamic registration\n", rmid));
            ret_cod = TMER_INVAL;
        }

        if ((TM_OK == ret_cod) && (XA_STATE_R1 != csr->xa_r_state)) {
            LIXA_TRACE(("ax_reg: invalid XA r_state (%d)\n", csr->xa_r_state));
            ret_cod = TMER_PROTO;
        }

        if ((TM_OK == ret_cod) && (XA_STATE_D0 != csr->xa_td_state)) {
            LIXA_TRACE(("ax_reg: invalid XA td_state (%d)\n",
                        csr->xa_td_state));
            ret_cod = TMER_PROTO;
        }

        /* build the message */
        msg.header.level = LIXA_MSG_LEVEL;
        msg.header.pvs.verb = LIXA_MSG_VERB_REG;
        msg.header.pvs.step = LIXA_MSG_STEP_INCR;

        msg.body.reg_8.rmid = rmid;
        msg.body.reg_8.flags = flags;
        msg.body.reg_8.rc = ret_cod;
        msg.body.reg_8.td_state = XA_STATE_D1;
        
        if (LIXA_RC_OK != (ret_cod = lixa_msg_serialize(
                               &msg, buffer, sizeof(buffer), &buffer_size)))
            THROW(MSG_SERIALIZE_ERROR);

        /* retrieve the socket */
        fd = client_status_get_sockfd(cs);
        LIXA_TRACE(("ax_reg: sending " SIZE_T_FORMAT
                    " bytes to the server for step 8\n", buffer_size));
        if (buffer_size != send(fd, buffer, buffer_size, 0))
            THROW(SEND_ERROR);

        /* @@@ return XID or NULLXID */
        
        csr->xa_td_state = XA_STATE_D1;
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case COLL_GET_CS_ERROR:
            case MSG_SERIALIZE_ERROR:
            case SEND_ERROR:
                ret_cod = TMER_TMERR;
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
