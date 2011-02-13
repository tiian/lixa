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
#ifndef LIXA_TX_RC_H
# define LIXA_TX_RC_H



#include <config.h>



#ifdef HAVE_ASSERT_H
# include <assert.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif



#include <tx.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_TX




/**
 * This is a special XAER return code used to manage the special situation
 * of xa_prepare with errors and xa_rollback with errors. This is not an XA
 * extension, but an internal LIXA implementation dependent value
 */
#define LIXA_XAER_HAZARD   -10



/**
 * This struct is the body of the "class" used to compute TX_* return codes
 * from XA_* return codes
 */
struct lixa_tx_rc_s {
    /**
     * The original operation issued from Application Program was tx_commit
     * (TRUE) or tx_rollback (FALSE). <br>
     * <b>Note:</b> the original operation may be different from the operation
     * was really performed because the prepare phase of a tx_commit can
     * fail and the transaction manager switch from tx_commit/commit to
     * tx_commit/rollback operation
     */
    int         tx_commit;
    /**
     * The object will be used in a commit operation (TRUE), or in a rollback
     * (FALSE) operation
     */
    int         commit;
    /**
     * (Temporary) value after some computation; (definitive) TX_* return code
     * after all the computation
     */
    int         tx_rc;
    /**
     * Array of XA_* return codes already acquired
     */
    GArray     *xa_rc;
};



/**
 * type version of @ref lixa_tx_rc_s struct
 */
typedef struct lixa_tx_rc_s lixa_tx_rc_t;



/**
 * Static initializer for an object of type @ref lixa_tx_rc_t
 */
#define LIXA_TX_RC_T_INIT { TRUE, TRUE, TX_FAIL, NULL }



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Create an object of type @ref lixa_tx_rc_t
     * @param ltr IN/OUT reference to the object must be initialized
     * @param tx_commit IN the original operation issued from Application
     *                  Program was tx_commit (TRUE) or tx_rollback (FALSE)
     * @param commit IN boolean: TRUE if the object will be used in a commit
     *               operation; <br>
     *               FALSE if the object will be used in a rollback operation
     * @param size IN initial/estimated size of the array (if 0, it will be
     *             dynamically increased when necessary)
     */
    static inline void lixa_tx_rc_create(lixa_tx_rc_t *ltr,
                                         int tx_commit, int commit,
                                         guint size) {
        ltr->tx_commit = tx_commit;
        ltr->commit = commit;
        ltr->tx_rc = TX_OK;
        ltr->xa_rc = g_array_sized_new(FALSE, FALSE, sizeof(int), size);
        assert(ltr->xa_rc != NULL);
    }
    


    /**
     * Delete an object of type @ref lixa_tx_rc_t
     * @param ltr IN/OUT reference to the object
     */
    static inline void lixa_tx_rc_delete(lixa_tx_rc_t *ltr) {
        if (NULL != ltr->xa_rc) {
            g_array_free(ltr->xa_rc, TRUE);
            ltr->xa_rc = NULL;
        }
    }



    
    /**
     * This function is used to sort the severity of TX_* return codes
     * (see The TX (Transaction Demarcation) Specifications,
     * Appendix B.5 pag. 67
     * I am not using a static array because I would include a positional
     * dependency on labels could (very unlikely) change in the future
     * @param tx_rc IN a TX_* return code
     * @return the severity of the TX_* passed return code: <br>
     *         - 0 means "no severity associated to the return code" <br>
     *         - 1 means higher severity <br>
     *         - 6 means lower severity <br>
     */
    static inline int lixa_tx_rc_hierarchy(int tx_rc) {
        switch (tx_rc) {
            case TX_FAIL:
                return 1;
            case TX_MIXED:
                return 2;
            case TX_HAZARD:
                return 3;
            case TX_ERROR:
                return 4;
            case TX_OUTSIDE:
            case TX_ROLLBACK:
            case TX_COMMITTED:
                return 5;
            case TX_OK:
                return 6;
            default:
                break;
        }
        return 0;
    }


    
    /**
     * Add a new xa_rc value; compute the new tx_rc (partial) result
     * @param ltr IN/OUT reference to the object
     * @param xa_rc IN new xa_rc value to add
     * @return a standardized return code
     */
    int lixa_tx_rc_add(lixa_tx_rc_t *ltr, int xa_rc);



    /**
     * Retrieve the currently computed value for tx_rc
     * @param ltr IN/OUT reference to the object
     * @return a TX_* return code
     */
    int lixa_tx_rc_get(lixa_tx_rc_t *ltr);


    
#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_TX_RC_H */
