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


#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif



#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include "tx.h"
#include "lixa_trace.h"
#include "lixa_xid.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



/* this file contains COBOL wrappers of the TX interface */



/**
 * COBOL wrapper for @ref tx_begin
 * @param TX_STATUS OUT return code
 */
void TXBEGIN(int32_t *TX_STATUS) {
    *TX_STATUS = tx_begin();
}



/**
 * COBOL wrapper for @ref tx_commit
 * @param TX_STATUS OUT return code
 */
void TXCOMMIT(int32_t *TX_STATUS) {
    *TX_STATUS = tx_commit();
}



/**
 * COBOL wrapper for @ref tx_close
 * @param TX_STATUS OUT return code
 */
void TXCLOSE(int32_t *TX_STATUS) {
    *TX_STATUS = tx_close();
}



/**
 * This data struct is necessary to map TXINFDEF copybook to a C struct
 */
struct TX_INFO_AREA_s {
    struct XID_REC_s {
        int32_t     FORMAT_ID;
        int32_t     GTRID_LENGTH;
        int32_t     BRANCH_LENGTH;
        char        XID_DATA[XIDDATASIZE];
    } XID_REC;
    int32_t         TRANSACTION_MODE;
    int32_t         COMMIT_RETURN;
    int32_t         TRANSACTION_CONTROL;
    int32_t         TRANSACTION_TIMEOUT;
    int32_t         TRANSACTION_STATE;
};



/**
 * COBOL wrapper for @ref tx_info
 * @param TX_INFO_AREA OUT retrieved information
 * @param TX_STATUS OUT return code
 */
void TXINFORM(struct TX_INFO_AREA_s *TX_INFO_AREA, int32_t *TX_STATUS) {
    TXINFO info;
    int ret_cod;
    /* call to C function */
    ret_cod = tx_info(&info);
    /* data conversion from C to COBOL */
    TX_INFO_AREA->XID_REC.FORMAT_ID = (int32_t)info.xid.formatID;
    TX_INFO_AREA->XID_REC.GTRID_LENGTH = (int32_t)info.xid.gtrid_length;
    TX_INFO_AREA->XID_REC.BRANCH_LENGTH = (int32_t)info.xid.bqual_length;
    memcpy(TX_INFO_AREA->XID_REC.XID_DATA, info.xid.data,
           XIDDATASIZE);
    if (0 == ret_cod || 1 == ret_cod) {
        TX_INFO_AREA->TRANSACTION_MODE = (int32_t)ret_cod;
        ret_cod = TX_OK; /* it will be returned to caller program */
    }
    TX_INFO_AREA->COMMIT_RETURN = (int32_t)info.when_return;
    TX_INFO_AREA->TRANSACTION_CONTROL = (int32_t)info.transaction_control;
    TX_INFO_AREA->TRANSACTION_TIMEOUT = (int32_t)info.transaction_timeout;
    TX_INFO_AREA->TRANSACTION_STATE = (int32_t)info.transaction_state;
    *TX_STATUS = ret_cod;
}



/**
 * COBOL wrapper for @ref tx_open
 * @param TX_STATUS OUT return code
 */
void TXOPEN(int32_t *TX_STATUS) {
    *TX_STATUS = tx_open();
}



/**
 * COBOL wrapper for @ref tx_rollback
 * @param TX_STATUS OUT return code
 */
void TXROLLBACK(int32_t *TX_STATUS) {
    *TX_STATUS = tx_rollback();
}



/**
 * COBOL wrapper for @ref tx_set_commit_return
 * @param TX_INFO_AREA IN passed information
 * @param TX_STATUS OUT return code
 */
void TXSETCOMMITRET(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                    int32_t *TX_STATUS) {
    *TX_STATUS = tx_set_commit_return(
        (COMMIT_RETURN)TX_INFO_AREA->COMMIT_RETURN);
}



/**
 * COBOL wrapper for @ref tx_set_transaction_timeout
 * @param TX_INFO_AREA IN passed information
 * @param TX_STATUS OUT return code
 */
void TXSETTIMEOUT(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                  int32_t *TX_STATUS) {
    *TX_STATUS = tx_set_transaction_timeout(
        (TRANSACTION_TIMEOUT)TX_INFO_AREA->TRANSACTION_TIMEOUT);
}



/**
 * COBOL wrapper for @ref tx_set_transaction_control
 * @param TX_INFO_AREA IN passed information
 * @param TX_STATUS OUT return code
 */
void TXSETTRANCTL(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                  int32_t *TX_STATUS) {
    *TX_STATUS = tx_set_transaction_control(
        (TRANSACTION_CONTROL)TX_INFO_AREA->TRANSACTION_CONTROL);
}




/**
 * This utility helps to print XID DATA binary part (GTRID)
 * @param TX_INFO_AREA IN passed information
 * @param LIXA_XID OUT returned string (it must be at least XIDDATASIZE + 1
 *                     bytes long)
 * @param TX_STATUS OUT return code
 */
void LIXAXIDSERIALIZE(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                      lixa_ser_xid_t LIXA_SER_XID, int32_t *TX_STATUS) {
    int ret_cod = lixa_xid_serialize(TX_INFO_AREA->XID_REC.XID_DATA,
                                     LIXA_SER_XID);
    if (!ret_cod) {
        LIXA_TRACE(("LIXAXIDSERIALIZE: lixa_xid_serialize returned %d\n",
                    ret_cod));
        *TX_STATUS = TX_ERROR;
    } else
        *TX_STATUS = TX_OK;
}
