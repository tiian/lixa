/*
 * Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_GMODULE_H
# include <gmodule.h>
#endif



/* XA standard header */
#include <xa.h>
/* PostgreSQL front-end */
#include <libpq-fe.h>
/* PostgreSQL XA wrapper provided by LIXA */
#include <liblixapq.h>
/* LIXA standard trace library: it could be removed if "TRACE" calls would
 * be removed from this source */
#include <lixa_trace.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA



struct xa_switch_t xapqls = {
    "PostgreSQL[LIXA]",
    TMNOFLAGS,
    0,
    lixa_pq_open,
    lixa_pq_close,
    lixa_pq_start,
    lixa_pq_end,
    lixa_pq_rollback,
    lixa_pq_prepare,
    lixa_pq_commit,
    lixa_pq_recover,
    lixa_pq_forget,
    lixa_pq_complete
};



const gchar *g_module_check_init(GModule *module)
{
    LIXA_TRACE(("lixa_pq/g_module_check_init: initializing module\n"));
    return NULL;
}



void g_module_unload(GModule *module)
{
    LIXA_TRACE(("lixa_pq/g_module_unload: releasing local storage\n"));
}



int lixa_pq_open(char *xa_info, int rmid, long flags)
{
    PGconn     *conn;
    
    LIXA_TRACE(("lixa_pq_open: xa_info='%s', rmid=%d, flags=%ld\n",
                xa_info, rmid, flags));

    /* asynchronous operations are not supported */
    if (TMASYNC & flags) {
        LIXA_TRACE(("lixa_pq_open: TMASYNC flag is not supported\n"));
        return XAER_ASYNC;
    }

    /* check if the connection was already established */
    /* @@@ */
    
    conn = PQconnectdb(xa_info);
    if (CONNECTION_OK != PQstatus(conn)) {
        LIXA_TRACE(("lixa_pq_open: error while connecting to the database: "
                    "%s", PQerrorMessage(conn)));
        PQfinish(conn);
        return XAER_RMERR;
    }
    
    return XA_OK;
}



int lixa_pq_close(char *xa_info, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_start(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_end(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_rollback(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_prepare(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_commit(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_recover(XID *xids, long count, int rmid, long flags)
{
    return 0;
}



int lixa_pq_forget(XID *xid, int rmid, long flags)
{
    return XA_OK;
}



int lixa_pq_complete(int *handle, int *retval, int rmid, long flags)
{
    /* asynchronous mode is not supported by this wrapper */
    return XAER_INVAL;
}
