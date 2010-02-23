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
#ifndef LIXA_TX_H
# define LIXA_TX_H



#include <config.h>



#include <tx.h>
#include <lixa_trace.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_TX



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * This function implements the real logic underlaying @ref tx_close
     * X/Open function
     * @param txrc OUT tx_* return code
     * @return a return code 
     */
    int lixa_tx_close(int *txrc);


    
    /**
     * This function implements the real logic underlaying @ref tx_commit
     * X/Open function
     * @param txrc OUT tx_* return code
     * @param begin_new OUT is a TRUE (boolean) value if a new transaction
     *        must be started with @ref lixa_tx_begin
     * @return a return code 
     */
    int lixa_tx_commit(int *txrc, int *begin_new);


    
    /**
     * This function implements the real logic underlaying @ref tx_begin
     * X/Open function
     * @param txrc OUT tx_* return code
     * @return a return code 
     */
    int lixa_tx_begin(int *txrc);


    
    /**
     * This function implements the real logic underlaying @ref tx_info
     * X/Open function
     * @param txrc OUT tx_* return code
     * @param info OUT TXINFO structure
     * @return a return code 
     */
    int lixa_tx_info(int *txrc, TXINFO *info);


    
    /**
     * This function implements the real logic underlaying @ref tx_open
     * X/Open function
     * @param txrc OUT tx_* return code
     * @return a return code 
     */
    int lixa_tx_open(int *txrc);


    
    /**
     * This function implements the real logic underlaying @ref tx_rollback
     * X/Open function
     * @param txrc OUT tx_* return code
     * @param begin_new OUT is a TRUE (boolean) value if a new transaction
     *        must be started with @ref lixa_tx_begin
     * @return a return code 
     */
    int lixa_tx_rollback(int *txrc, int *begin_new);


    
    /**
     * This function implements the real logic underlaying
     * @ref tx_set_commit_return X/Open function
     * @param txrc OUT tx_* return code
     * @param when_return IN commit_return characteristic
     * @return a return code 
     */
    int lixa_tx_set_commit_return(int *txrc, COMMIT_RETURN when_return);



    /**
     * This function implements the real logic underlaying
     * @ref tx_set_transaction_control X/Open function
     * @param txrc OUT tx_* return code
     * @param control IN transaction control characteristic
     * @return a return code 
     */
    int lixa_tx_set_transaction_control(int *txrc,
                                        TRANSACTION_CONTROL control);


    
    /**
     * This function implements the real logic underlaying
     * @ref tx_set_transaction_timeout X/Open function
     * @param txrc OUT tx_* return code
     * @param timeout IN transaction timeout (seconds)
     * @return a return code 
     */
    int lixa_tx_set_transaction_timeout(int *txrc,
                                        TRANSACTION_TIMEOUT timeout);



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_TX_H */
