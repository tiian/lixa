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



#include <liblixamonkey.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



/*
 * Monkey implementation of xa_open function
 */ 
int lixa_monkeyrm_open(char *xa_info, int rmid, long flags) {
    LIXA_TRACE(("lixa_monkeyrm_open: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    LIXA_TRACE(("lixa_monkeyrm_open\n"));
    return XA_OK;
}



/*
 * Monkey implementation of xa_close function
 */ 
int lixa_monkeyrm_close(char *xa_info, int rmid, long flags) {
    LIXA_TRACE(("lixa_monkeyrm_close: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    return XA_OK;
}



/*
 * Monkey implementation of xa_start function
 */ 
int lixa_monkeyrm_start(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_start: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    return XA_OK;
}



/*
 * Monkey implementation of xa_end function
 */ 
int lixa_monkeyrm_end(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_end: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    return XA_OK;
}



/*
 * Monkey implementation of xa_rollback function
 */ 
int lixa_monkeyrm_rollback(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_rollback: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    return XA_OK;
}



/*
 * Monkey implementation of xa_prepare function
 */ 
int lixa_monkeyrm_prepare(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_prepare: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    return XA_OK;
}



/*
 * Monkey implementation of xa_commit function
 */ 
int lixa_monkeyrm_commit(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_commit: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    return XA_OK;
}



/*
 * Monkey implementation of xa_recover function
 */ 
int lixa_monkeyrm_recover(XID *xid, long count, int rmid, long flags) {
    LIXA_TRACE(("lixa_monkeyrm_recover: *xid=%p, count=%ld, rmid=%d, "
                "flags=0x%lx\n", xid, count, rmid, flags));
    return XA_OK;
}



/*
 * Monkey implementation of xa_forget function
 */ 
int lixa_monkeyrm_forget(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_monkeyrm_forget: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    return XA_OK;
}



/*
 * Monkey implementation of xa_complete function
 */ 
int lixa_monkeyrm_complete(int *handle, int *retval, int rmid, long flags) {
    LIXA_TRACE(("lixa_monkeyrm_complete: rmid=%d, flags=0x%lx\n",
                rmid, flags));
    return XA_OK;
}



/*
 * This is the struct pointing to monkey functions
 */
struct xa_switch_t lixa_monkeyrm_sta_sw = {
    "LIXA Monkey RM",
    TMNOFLAGS,
    0,
    lixa_monkeyrm_open,
    lixa_monkeyrm_close,
    lixa_monkeyrm_start,
    lixa_monkeyrm_end,
    lixa_monkeyrm_rollback,
    lixa_monkeyrm_prepare,
    lixa_monkeyrm_commit,
    lixa_monkeyrm_recover,
    lixa_monkeyrm_forget,
    lixa_monkeyrm_complete
};



/*
 * This is the struct pointing to monkey functions
 */
struct xa_switch_t lixa_monkeyrm_dyn_sw = {
    "LIXA Monkey RM",
    TMREGISTER,
    0,
    lixa_monkeyrm_open,
    lixa_monkeyrm_close,
    lixa_monkeyrm_start,
    lixa_monkeyrm_end,
    lixa_monkeyrm_rollback,
    lixa_monkeyrm_prepare,
    lixa_monkeyrm_commit,
    lixa_monkeyrm_recover,
    lixa_monkeyrm_forget,
    lixa_monkeyrm_complete
};
