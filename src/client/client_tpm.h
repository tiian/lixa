/*
 * Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_CLIENT_TPM_H
#define LIXA_CLIENT_TPM_H

#include <config.h>

#ifdef HAVE_GLIB_H

#include <glib.h>

#endif

#include <client_status.h>
#include <tx.h>

/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#else
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_CLIENT_TPM

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    int client_tpm_trans(client_status_t *cs, GTree *xidt, int maint);

    int client_tpm_report(const client_status_t *cs, GTree *xidt);

    gboolean client_tpm_report_foreach(gpointer key, gpointer value, gpointer data);

/**
 * @brief Count the number of transactions
 * @param key
 * @param value
 * @param data
 * @return boolean indicating if traversal should continue
 */
    gboolean client_tpm_value_foreach(gpointer key, gpointer value, gpointer data);

/**
 * @brief Count the number of unique transactions
 * @param key
 * @param value
 * @param data
 * @return boolean indicating if traversal should continue
 */
    gboolean client_tpm_unique_value_foreach(gpointer key, gpointer value, gpointer data);

    int tpm_gtrid_compare(gconstpointer a, gconstpointer b, gpointer user_data);

    void tpm_gtrid_value_destroy(gpointer data);

#ifdef __cplusplus
}
#endif /* __cplusplus */

/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif //LIXA_CLIENT_TPM_H
