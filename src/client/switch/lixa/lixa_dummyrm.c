/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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



/*
 * This is a dummy implementation of a XA compliant resource manager
 * It's primarily used for development and debug purposes
 */



#include <lixa_trace.h>
#include <lixa_common_status.h>
#include <lixa_xid.h>
#include <xa.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA_SWITCH



/*
 * Dummy implementation of xa_open function
 */ 
int lixa_dummyrm_open(char *xa_info, int rmid, long flags) {
    LIXA_TRACE(("lixa_dummyrm_open: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    LIXA_TRACE(("lixa_dummyrm_open\n"));
    return XA_OK;
}



/*
 * Dummy implementation of xa_close function
 */ 
int lixa_dummyrm_close(char *xa_info, int rmid, long flags) {
    LIXA_TRACE(("lixa_dummyrm_close: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    return XA_OK;
}



/*
 * Dummy implementation of xa_start function
 */ 
int lixa_dummyrm_start(const XID *xid, int rmid, long flags) {
    lixa_ser_xid_t xid_str;
    if (lixa_xid_serialize(xid, xid_str)) {
        LIXA_TRACE(("lixa_dummyrm_start: xid='%s', rmid=%d, flags=0x%lx\n",
                    xid_str, rmid, flags));
    }
    return XA_OK;
}



/*
 * Dummy implementation of xa_end function
 */ 
int lixa_dummyrm_end(const XID *xid, int rmid, long flags) {
    lixa_ser_xid_t xid_str;
    if (lixa_xid_serialize(xid, xid_str)) {
        LIXA_TRACE(("lixa_dummyrm_end: xid='%s', rmid=%d, flags=0x%lx\n",
                    xid_str, rmid, flags));
    }
    return XA_OK;
}



/*
 * Dummy implementation of xa_rollback function
 */ 
int lixa_dummyrm_rollback(const XID *xid, int rmid, long flags) {
    lixa_ser_xid_t xid_str;
    if (lixa_xid_serialize(xid, xid_str)) {
        LIXA_TRACE(("lixa_dummyrm_rollback: xid='%s', rmid=%d, flags=0x%lx\n",
                    xid_str, rmid, flags));
    }
    return XA_OK;
}



/*
 * Dummy implementation of xa_prepare function
 */ 
int lixa_dummyrm_prepare(const XID *xid, int rmid, long flags) {
    lixa_ser_xid_t xid_str;
    if (lixa_xid_serialize(xid, xid_str)) {
        LIXA_TRACE(("lixa_dummyrm_prepare: xid='%s', rmid=%d, flags=0x%lx\n",
                    xid_str, rmid, flags));
    }
    return XA_OK;
}



/*
 * Dummy implementation of xa_commit function
 */ 
int lixa_dummyrm_commit(const XID *xid, int rmid, long flags) {
    lixa_ser_xid_t xid_str;
    if (lixa_xid_serialize(xid, xid_str)) {
        LIXA_TRACE(("lixa_dummyrm_commit: xid='%s', rmid=%d, flags=0x%lx\n",
                    xid_str, rmid, flags));
    }
    return XA_OK;
}



/*
 * Dummy implementation of xa_recover function
 */ 
int lixa_dummyrm_recover(XID *xids, long count, int rmid, long flags) {
    LIXA_TRACE(("lixa_dummyrm_recover: *xids=%p, count=%ld, rmid=%d, "
                "flags=0x%lx\n", xids, count, rmid, flags));
    return XA_OK;
}



/*
 * Dummy implementation of xa_forget function
 */ 
int lixa_dummyrm_forget(const XID *xid, int rmid, long flags) {
    lixa_ser_xid_t xid_str;
    if (lixa_xid_serialize(xid, xid_str)) {
        LIXA_TRACE(("lixa_dummyrm_forget: xid='%s', rmid=%d, flags=0x%lx\n",
                    xid_str, rmid, flags));
    }
    return XA_OK;
}



/*
 * Dummy implementation of xa_complete function
 */ 
int lixa_dummyrm_complete(int *handle, int *retval, int rmid, long flags) {
    LIXA_TRACE(("lixa_dummyrm_complete: rmid=%d, flags=0x%lx\n",
                rmid, flags));
    return XA_OK;
}



/*
 * This is the struct pointing to dummy functions
 */
struct xa_switch_t lixa_dummyrm_sw = {
    "lixa_dummyrm",
    TMNOMIGRATE,
    0,
    lixa_dummyrm_open,
    lixa_dummyrm_close,
    lixa_dummyrm_start,
    lixa_dummyrm_end,
    lixa_dummyrm_rollback,
    lixa_dummyrm_prepare,
    lixa_dummyrm_commit,
    lixa_dummyrm_recover,
    lixa_dummyrm_forget,
    lixa_dummyrm_complete
};



/*
 * The function is exported and dynamically retrieved afted the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &lixa_dummyrm_sw;
}
