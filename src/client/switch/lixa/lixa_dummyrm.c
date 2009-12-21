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



/*
 * This is a dummy implementation of a XA compliant resource manager
 * It's primarily used for development and debug purposes
 */



#include "xa.h"



/*
 * Dummy implementation of xa_open function
 */ 
int lixa_dummyrm_open(char *xa_info, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_close function
 */ 
int lixa_dummyrm_close(char *xa_info, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_start function
 */ 
int lixa_dummyrm_start(XID *xid, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_end function
 */ 
int lixa_dummyrm_end(XID *xid, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_rollback function
 */ 
int lixa_dummyrm_rollback(XID *xid, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_prepare function
 */ 
int lixa_dummyrm_prepare(XID *xid, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_commit function
 */ 
int lixa_dummyrm_commit(XID *xid, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_recover function
 */ 
int lixa_dummyrm_recover(XID *xid, long count, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_forget function
 */ 
int lixa_dummyrm_forget(XID *xid, int rmid, long flags) {
    return XA_OK;
}

/*
 * Dummy implementation of xa_complete function
 */ 
int lixa_dummyrm_complete(int *handle, int *retval, int rmid, long flags) {
    return XA_OK;
}



/*
 * This is the struct pointing to dummy functions
 */
struct xa_switch_t lixa_dummyrm_sw = {
    "lixa_dummyrm",
    TMREGISTER,
    0,
    lixa_dummyrm_open,
    lixa_dummyrm_close,
    lixa_dummyrm_start,
    lixa_dummyrm_end,
    lixa_dummyrm_rollback,
    lixa_dummyrm_prepare,
    lixa_dummyrm_commit,
    lixa_dummyrm_recover,
    lixa_dummyrm_forget,
    lixa_dummyrm_complete
};



/*
 * The function is exported and dynamically retrieved afted the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &lixa_dummyrm_sw;
}
