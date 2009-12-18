/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free 
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
     * This function implements the real logic underlaying @ref tx_begin
     * X/Open function
     * @param txrc OUT tx_* return code
     * @return a return code 
     */
    int lixa_tx_begin(int *txrc);


    
    /**
     * This function implements the real logic underlaying @ref tx_open
     * X/Open function
     * @param txrc OUT tx_* return code
     * @return a return code 
     */
    int lixa_tx_open(int *txrc);


    
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
