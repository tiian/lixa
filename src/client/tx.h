/*
 * This header is NOT original but it's derived from this official
 * documentation:
 *
 * X/Open CAE Specification
 * Distributed Transaction Processing:
 * The TX (Transaction Demarcation) Specification
 * ISBN: 1-85912-094-6
 * X/Open Document Number: C504
 */



/*
 * Start of tx.h header
 *
 * Define a symbol to prevent multiple inclusions of this header file
 */
#ifndef TX_H
#define TX_H
#define TX_H_VERSION 0 /* current version of this header file */



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_CLIENT_TX



/*
 * Transaction identifier
 */
#define XIDDATASIZE 128 /* size in bytes */
#ifndef XID_T_TYPE
# define XID_T_TYPE
struct xid_t {
    long formatID; /* format identifier */
    long gtrid_length; /* value from 1 through 64 */
    long bqual_length; /* value from 1 through 64 */
    char data[XIDDATASIZE];
};
typedef struct xid_t XID;
#endif /* XID_T_TYPE */



/*
 * A value of -1 in formatID means that the XID is null.
 */
#ifndef NULLXID
# define NULLXID -1
#endif
/*
 * Definitions for tx_*() routines
 */
/* commit return values */
typedef long COMMIT_RETURN;
#define TX_COMMIT_COMPLETED 0
#define TX_COMMIT_DECISION_LOGGED 1
/* transaction control values */
typedef long TRANSACTION_CONTROL;
#define TX_UNCHAINED 0
#define TX_CHAINED 1
/* type of transaction timeouts */
typedef long TRANSACTION_TIMEOUT;
/* transaction state values */
typedef long TRANSACTION_STATE;

/** As described in TX (Transaction Demarcation) Specification */
#define TX_ACTIVE 0
/** As described in TX (Transaction Demarcation) Specification */
#define TX_TIMEOUT_ROLLBACK_ONLY 1
/** As described in TX (Transaction Demarcation) Specification */
#define TX_ROLLBACK_ONLY 2

/* structure populated by tx_info() */
struct tx_info_t {
    XID xid;
    COMMIT_RETURN when_return;
    TRANSACTION_CONTROL transaction_control;
    TRANSACTION_TIMEOUT transaction_timeout;
    TRANSACTION_STATE transaction_state;
};
typedef struct tx_info_t TXINFO;



/*
 * tx_*() return codes (transaction manager reports to application)
 */
#define TX_NOT_SUPPORTED 1 /* option not supported */
#define TX_OK 0 /* normal execution */
#define TX_OUTSIDE -1 /* application is in an RM local
                         transaction */
#define TX_ROLLBACK -2 /* transaction was rolled back */
#define TX_MIXED -3 /* transaction was partially committed
                       and partially rolled back */
#define TX_HAZARD -4 /* transaction may have been partially
                        committed and partially rolled back */
#define TX_PROTOCOL_ERROR -5 /* routine invoked in an improper
                                context */
#define TX_ERROR -6 /* transient error */
#define TX_FAIL -7 /* fatal error */
#define TX_EINVAL -8 /* invalid arguments were given */
#define TX_COMMITTED -9 /* transaction has heuristically
                           committed */
#define TX_NO_BEGIN -100 /* transaction committed plus new
                            transaction could not be started */
#define TX_ROLLBACK_NO_BEGIN (TX_ROLLBACK+TX_NO_BEGIN)
/* transaction rollback plus new
   transaction could not be started */
#define TX_MIXED_NO_BEGIN (TX_MIXED+TX_NO_BEGIN)
/* mixed plus new transaction could not
   be started */
#define TX_HAZARD_NO_BEGIN (TX_HAZARD+TX_NO_BEGIN)
/* hazard plus new transaction could
   not be started */
#define TX_COMMITTED_NO_BEGIN (TX_COMMITTED+TX_NO_BEGIN)
/* heuristically committed plus new
   transaction could not be started */



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



/*
 * Declarations of routines by which Applications call TMs:
 */



/**
 * Begin a global transaction.
 * @return a standardized TX return code (TX_*)
 */
extern int tx_begin(void);


/**
 * Close a set of resource managers.
 * Calling this function you free the resource allocated and locked by
 * @ref tx_open
 * @return a standardized TX return code (TX_*)
 */
extern int tx_close(void);


/**
 * Commit a global transaction.
 * @return a standardized TX return code (TX_*)
 */
extern int tx_commit(void);


/**
 * Return global transaction information.
 * @param info OUT global transaction information returned
 * @return a standardized TX return code (TX_*)
 */
extern int tx_info(TXINFO *info);


/**
 * Open a set of resource managers; this function is the X/Open compliant
 * wrapper for @ref lixa_tx_open function
 * You MUST call @ref tx_close from the same thread issued tx_open
 * (memory leaks and other unpleasant effects may happen otherwise!)
 * @return a standardized TX return code (TX_*)
 */
extern int tx_open(void);


/**
 * Roll back a global transaction.
 * @return a standardized TX return code (TX_*)
 */
extern int tx_rollback(void);


/**
 * Set commit_return characteristic
 * @return a standardized TX return code (TX_*)
 */
extern int tx_set_commit_return(COMMIT_RETURN when_return);


/**
 * Set transaction_control chracteristic
 * @return a standardized TX return code (TX_*)
 */
extern int tx_set_transaction_control(TRANSACTION_CONTROL control);


/**
 * Set transaction_timeout characteristic
 * @return a standardized TX return code (TX_*)
 */
extern int tx_set_transaction_timeout(TRANSACTION_TIMEOUT timeout);

/**
 * Serialize an XID
 * @param info IN global transaction information retrieved by @ref tx_info
 * @param sxid OUT the XID in readible form (memory will be allocated)
 * @return a standardized TX return code (TX_*)
 */
extern int tx_xid_serialize(TXINFO info, char **sxid);

/**
 * De-serialize an XID
 * @param info OUT global transaction information
 * @param sxid IN the readible XID generated by @ref tx_xid_serialize
 * @return a standardized TX return code (TX_*)
 */
extern int tx_xid_deserialize(TXINFO *info, char *sxid);

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */


#endif /* ifndef TX_H */
/*
 * End of tx.h header
 */

