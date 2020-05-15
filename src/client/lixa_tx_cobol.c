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
#include "lixa_tx_cobol.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_TX



/* this file contains COBOL wrappers of the TX interface */



void TXBEGIN(int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXBEGIN\n"));
    *TX_STATUS = tx_begin();
    LIXA_TRACE(("TXBEGIN/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXCOMMIT(int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXCOMMIT\n"));
    *TX_STATUS = tx_commit();
    LIXA_TRACE(("TXCOMMIT/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXCLOSE(int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXCLOSE\n"));
    *TX_STATUS = tx_close();
    LIXA_TRACE(("TXCLOSE/TX_STATUS=%d\n", *TX_STATUS));    
}



void lixa_tx_xid_c_cobol(const XID *xid, struct XID_REC_s *XID_REC) {
    XID_REC->FORMAT_ID = xid->formatID;
    XID_REC->GTRID_LENGTH = xid->gtrid_length;
    XID_REC->BRANCH_LENGTH = xid->bqual_length;
    memcpy(XID_REC->XID_DATA, xid->data, XIDDATASIZE);
}



void lixa_tx_xid_cobol_c(const struct XID_REC_s *XID_REC, XID *xid) {
    xid->formatID = XID_REC->FORMAT_ID;
    xid->gtrid_length = XID_REC->GTRID_LENGTH;
    xid->bqual_length = XID_REC->BRANCH_LENGTH;
    memcpy(xid->data, XID_REC->XID_DATA, XIDDATASIZE);
}



void TXINFORM(struct TX_INFO_AREA_s *TX_INFO_AREA, int32_t *TX_STATUS) {
    TXINFO info;
    int ret_cod;
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXINFORM\n"));
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
    LIXA_TRACE(("TXINFORM/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXOPEN(int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXOPEN\n"));
    *TX_STATUS = tx_open();
    LIXA_TRACE(("TXOPEN/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXROLLBACK(int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXROLLBACK\n"));
    *TX_STATUS = tx_rollback();
    LIXA_TRACE(("TXROLLBACK/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXSETCOMMITRET(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                    int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXSETCOMMITRET\n"));
    *TX_STATUS = tx_set_commit_return(
        (COMMIT_RETURN)TX_INFO_AREA->COMMIT_RETURN);
    LIXA_TRACE(("TXSETCOMMITRET/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXSETTIMEOUT(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                  int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXSETTIMEOUT\n"));
    *TX_STATUS = tx_set_transaction_timeout(
        (TRANSACTION_TIMEOUT)TX_INFO_AREA->TRANSACTION_TIMEOUT);
    LIXA_TRACE(("TXSETTIMEOUT/TX_STATUS=%d\n", *TX_STATUS));    
}



void TXSETTRANCTL(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                  int32_t *TX_STATUS) {
    LIXA_TRACE_INIT;
    LIXA_TRACE(("TXSETTRANCTL\n"));
    *TX_STATUS = tx_set_transaction_control(
        (TRANSACTION_CONTROL)TX_INFO_AREA->TRANSACTION_CONTROL);
    LIXA_TRACE(("TXSETTRANCTL/TX_STATUS=%d\n", *TX_STATUS));    
}



void LIXAXIDSERIALIZE(const struct TX_INFO_AREA_s *TX_INFO_AREA,
                      lixa_ser_xid_t LIXA_SER_XID, int32_t *TX_STATUS) {
    int ret_cod;
    XID xid;
    LIXA_TRACE_INIT;
    LIXA_TRACE(("LIXAXIDSERIALIZE\n"));
    lixa_tx_xid_cobol_c(&TX_INFO_AREA->XID_REC, &xid);
    memset(LIXA_SER_XID, 0, sizeof(lixa_ser_xid_t));
    ret_cod = lixa_xid_serialize(&xid, LIXA_SER_XID);
     if (!ret_cod) {
        LIXA_TRACE(("LIXAXIDSERIALIZE: lixa_xid_serialize returned %d\n",
                    ret_cod));
        *TX_STATUS = TX_ERROR;
    } else
        *TX_STATUS = TX_OK;
    LIXA_TRACE(("LIXAXIDSERIALIZE/TX_STATUS=%d\n", *TX_STATUS));    
}



