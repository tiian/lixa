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
#ifndef LIXA_SERVER_TPM_H
#define LIXA_SERVER_TPM_H

#include <config.h>

#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixa_xml_msg.h>
#include <server_status.h>
#include <server_trans_tbl.h>

/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
#undef LIXA_TRACE_MODULE
#else
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE LIXA_TRACE_MOD_SERVER_TPM

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

    int server_trans(struct thread_status_s *ts, size_t slot_id,
                     const struct lixa_msg_s *lmi, struct lixa_msg_s *lmo,
                     uint32_t block_id,
                     struct lixa_msg_verb_step_s *last_verb_step);

    int server_trans_8(struct thread_status_s *ts, size_t slot_id,
                       const struct lixa_msg_s *lmi, struct lixa_msg_s *lmo,
                       uint32_t block_id,
                       struct lixa_msg_verb_step_s *last_verb_step);

    int server_trans_result(struct thread_status_s *ts,
                            const struct server_trans_tbl_rec_s *record,
                            const guint record_array_size,
                            const struct lixa_msg_s *lmi,
                            struct lixa_msg_s *lmo,
                            uint32_t block_id);

    int server_trans_empty_result(struct thread_status_s *ts,
                                  const struct lixa_msg_s *lmi,
                                  struct lixa_msg_s *lmo);

#ifdef __cplusplus
}
#endif /* __cplusplus */

/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE
#define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
#undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */

#endif //LIXA_SERVER_TPM_H
