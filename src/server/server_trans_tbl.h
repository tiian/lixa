/*
 * Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_SERVER_TRANS_TBL_H
#define LIXA_SERVER_TRANS_TBL_H

#include <config.h>

#ifdef HAVE_GLIB_H

#include <glib.h>
#include <glibconfig.h>

#endif

#include <lixa_trace.h>
#include <lixa_xid.h>
#include <xa.h>

/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#else
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_SERVER_TPM

#define SERVER_TRANS_TBL_INIT {NULL, NULL, 0}

struct server_trans_tbl_rec_s
{
    char *gtrid; /** key */
    guint tsid; /** thread status identifier */
    lixa_ser_xid_t xid; /** the transaction XID */
    uint32_t block_id; /** relevant block inside the status file */
};

struct server_trans_tbl_s
{
    GMutex *mutex;
    GTree *records; /** multidimentional tree with gtrid as key */
    guint tsid_array_size; /** size of thread status identifier array */
};

typedef struct server_trans_tbl_s server_trans_tbl_t;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

int server_trans_tbl_new(server_trans_tbl_t *stt, guint tsid_array_size);

int server_trans_tbl_comp(gconstpointer a, gconstpointer b,
                          gpointer user_data);

void server_trans_tbl_value_destroy(gpointer data);

int server_trans_tbl_insert(server_trans_tbl_t *stt,
                            const struct server_trans_tbl_rec_s *sttr);

int server_trans_tbl_delete(server_trans_tbl_t *stt);

int server_trans_tbl_query_xid(server_trans_tbl_t *stt,
                               const struct server_trans_tbl_rec_s *sttr,
                               struct server_trans_tbl_rec_s *out,
                               guint *out_array_size,
                               int main);

gboolean server_trans_tbl_traverse(gpointer key, gpointer value, gpointer data);

#ifdef __cplusplus
}
#endif /* __cplusplus */

/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif //LIXA_SERVER_TRANS_TBL_H
