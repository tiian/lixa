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



/*
 * This is a random implementation of a XA compliant resource manager
 * It's primarily used for development and debug purposes
 */
#include <config.h>



#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif



#include <lixa_trace.h>
#include <lixa_common_status.h>
#include <xa.h>



#define ERROR_PERC 5



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



/*
 * Random implementation of xa_open function
 */ 
int lixa_randomrm_open(char *xa_info, int rmid, long flags) {
    LIXA_TRACE(("lixa_randomrm_open: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_close function
 */ 
int lixa_randomrm_close(char *xa_info, int rmid, long flags) {
    LIXA_TRACE(("lixa_randomrm_close: xa_info='%s', rmid=%d, flags=0x%lx\n",
                xa_info, rmid, flags));
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_start function
 */ 
int lixa_randomrm_start(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_start: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_end function
 */ 
int lixa_randomrm_end(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_end: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_rollback function
 */ 
int lixa_randomrm_rollback(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_rollback: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_prepare function
 */ 
int lixa_randomrm_prepare(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_prepare: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}

/*
 * Random implementation of xa_commit function
 */ 
int lixa_randomrm_commit(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_commit: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_recover function
 */ 
int lixa_randomrm_recover(XID *xid, long count, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_recover: xid='%s', count=%ld, rmid=%d, "
                "flags=0x%lx\n", xid_str, count, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_forget function
 */ 
int lixa_randomrm_forget(XID *xid, int rmid, long flags) {
    char *xid_str = xid_serialize(xid);
    LIXA_TRACE(("lixa_randomrm_forget: xid='%s', rmid=%d, flags=0x%lx\n",
                xid_str, rmid, flags));
    free(xid_str);
    if (random() % 100 < ERROR_PERC)
        return XAER_RMERR;
    return XA_OK;
}



/*
 * Random implementation of xa_complete function
 */ 
int lixa_randomrm_complete(int *handle, int *retval, int rmid, long flags) {
    LIXA_TRACE(("lixa_randomrm_complete: rmid=%d, flags=0x%lx\n",
                rmid, flags));
    if (random() % 100 < ERROR_PERC)
        return XAER_INVAL;
    return XA_OK;
}



/*
 * This is the struct pointing to random functions
 */
struct xa_switch_t lixa_randomrm_sw = {
    "lixa_randomrm",
    TMNOFLAGS,
    0,
    lixa_randomrm_open,
    lixa_randomrm_close,
    lixa_randomrm_start,
    lixa_randomrm_end,
    lixa_randomrm_rollback,
    lixa_randomrm_prepare,
    lixa_randomrm_commit,
    lixa_randomrm_recover,
    lixa_randomrm_forget,
    lixa_randomrm_complete
};



/*
 * The function is exported and dynamically retrieved afted the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &lixa_randomrm_sw;
}
