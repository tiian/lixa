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
#include <config.h>



#ifdef HAVE_POSTGRESQL
# include "lixapq.h"
#endif



#include "tx.h"
#include "lixa_trace.h"
#include "lixa_xid.h"



/* this file contains COBOL wrappers of the TX interface */



/**
 * This data struct is necessary to map the equivalent COBOL record
 * Note: the values changes between C and COBOL (32/64 bit)
 */
struct XID_REC_s {
    int32_t     FORMAT_ID;
    int32_t     GTRID_LENGTH;
    int32_t     BRANCH_LENGTH;
    char        XID_DATA[XIDDATASIZE];
} XID_REC;



/**
 * This data struct is necessary to map TXINFDEF copybook to a C struct
 */
struct TX_INFO_AREA_s {
    struct XID_REC_s XID_REC;
    int32_t          TRANSACTION_MODE;
    int32_t          COMMIT_RETURN;
    int32_t          TRANSACTION_CONTROL;
    int32_t          TRANSACTION_TIMEOUT;
    int32_t          TRANSACTION_STATE;
};



/**
 * COBOL wrapper for @ref tx_begin
 * @param TX_STATUS OUT return code
 */
void TXBEGIN(int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_commit
 * @param TX_STATUS OUT return code
 */
void TXCOMMIT(int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_close
 * @param TX_STATUS OUT return code
 */
void TXCLOSE(int32_t *TX_STATUS);



/**
 * Transform a C XID struct in the equivalent COBOL friendly XID_REC_s
 * @param xid IN source C xid
 * @param XID_REC OUT target COBOL friendly XID_REC_s
 */
void lixa_tx_xid_c_cobol(const XID *xid, struct XID_REC_s *XID_REC);



/**
 * Transform a COBOL friendly XID_REC_s in the equivalent C XID struct  
 * @param XID_REC IN source COBOL friendly XID_REC_s
 * @param xid OUT target C xid
 */
void lixa_tx_xid_cobol_c(const struct XID_REC_s *XID_REC, XID *xid);



/**
 * COBOL wrapper for @ref tx_info
 * @param TX_INFO_AREA OUT retrieved information
 * @param TX_STATUS OUT return code
 */
void TXINFORM(struct TX_INFO_AREA_s *TX_INFO_AREA, int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_open
 * @param TX_STATUS OUT return code
 */
void TXOPEN(int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_rollback
 * @param TX_STATUS OUT return code
 */
void TXROLLBACK(int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_set_commit_return
 * @param TX_INFO_AREA IN passed information
 * @param TX_STATUS OUT return code
 */
void TXSETCOMMITRET(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                    int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_set_transaction_timeout
 * @param TX_INFO_AREA IN passed information
 * @param TX_STATUS OUT return code
 */
void TXSETTIMEOUT(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                  int32_t *TX_STATUS);



/**
 * COBOL wrapper for @ref tx_set_transaction_control
 * @param TX_INFO_AREA IN passed information
 * @param TX_STATUS OUT return code
 */
void TXSETTRANCTL(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                  int32_t *TX_STATUS);




/**
 * This utility helps to print XID DATA binary part (GTRID)
 * @param TX_INFO_AREA IN passed information
 * @param LIXA_XID OUT returned string (it must be at least XIDDATASIZE + 1
 *                     bytes long)
 * @param TX_STATUS OUT return code
 */
void LIXAXIDSERIALIZE(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                      lixa_ser_xid_t LIXA_SER_XID, int32_t *TX_STATUS);



