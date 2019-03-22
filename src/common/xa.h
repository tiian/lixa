/*
 * This header is NOT original but it's a derivative work of this public
 * documentation:
 *
 * X/Open CAE Specification
 * Distributed Transaction Processing:
 * The XA Specification
 * ISBN: 1 872630 24 3
 * X/Open Document Number: XO/CAE/91/300
 */

/*
 * Start of xa.h header
 *
 * Define a symbol to prevent multiple inclusions of this header file
 */
#ifndef XA_H
# define XA_H

/*
 * Transaction branch identification: XID and NULLXID:
 */
#define XIDDATASIZE 128 /* size in bytes */
#define MAXGTRIDSIZE 64 /* maximum size in bytes of gtrid */
#define MAXBQUALSIZE 64 /* maximum size in bytes of bqual */
#ifndef XID_T_TYPE
# define XID_T_TYPE
struct xid_t
{
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
 * Declarations of routines by which RMs call TMs:
 */
/**
 * Dynamically register a resource manager with a transaction manager
 */
extern int ax_reg(int rmid, XID *xid, long flags);

/**
 * Dynamically unregister a resource manager with a transaction manager
 */
extern int ax_unreg(int rmid, long flags);



/**
 * length of resource manager name, including the null terminator
 */
#define RMNAMESZ 32
/**
 * maximum size in bytes of xa_info strings, including the null terminator
 */
#define MAXINFOSIZE 256 


/**
 * XA switch data structure
 */
struct xa_switch_t
{
    /**
     * name of resource manager
     */
    char name[RMNAMESZ];
    /**
     * options specific to the resource manager
     */
    long flags;
    /**
     * must be 0
     */
    long version;
    /**
     * xa_open function pointer
     */
    int (*xa_open_entry)(char *, int, long);
    /**
     * xa_close function pointer
     */
    int (*xa_close_entry)(char *, int, long);
    /**
     * xa_start function pointer
     */
    int (*xa_start_entry)(const XID *, int, long);
    /**
     * xa_end function pointer
     */
    int (*xa_end_entry)(const XID *, int, long); 
    /**
     * xa_rollback function pointer
     */
    int (*xa_rollback_entry)(const XID *, int, long);
    /**
     * xa_prepare function pointer
     */
    int (*xa_prepare_entry)(const XID *, int, long);
    /**
     * xa_commit function pointer
     */
    int (*xa_commit_entry)(const XID *, int, long); 
    /**
     * xa_recover function pointer
     */
    int (*xa_recover_entry)(XID *, long, int, long);
    /**
     * xa_forget function pointer
     */
    int (*xa_forget_entry)(const XID *, int, long); 
    /**
     * xa_complete function pointer
     */
    int (*xa_complete_entry)(int *, int *, int, long);
};



typedef struct xa_switch_t *(*lixa_get_xa_switch_f)(void);



/*
 * Flag definitions for the RM switch
 */
/**
 * no resource manager features selected
 */
#define TMNOFLAGS 0x00000000L
/**
 * resource manager dynamically registers
 */
#define TMREGISTER 0x00000001L
/**
 * resource manager does not support association migration
 */
#define TMNOMIGRATE 0x00000002L
/**
 * resource manager supports asynchronous operations
 */
#define TMUSEASYNC 0x00000004L
/*
 * Flag definitions for xa_ and ax_ routines
 */
/* use TMNOFLAGS, defined above, when not specifying other flags */
/**
 * perform routine asynchronously
 */
#define TMASYNC 0x80000000L
/**
 * caller is using one-phase commit optimisation
 */
#define TMONEPHASE 0x40000000L
/**
 * dissociates caller and marks transaction branch rollback-only
 */
#define TMFAIL 0x20000000L
/**
 * return if blocking condition exists
 */
#define TMNOWAIT 0x10000000L
/**
 * caller is resuming association with suspended transaction branch
 */
#define TMRESUME 0x08000000L
/**
 * dissociate caller from transaction branch
 */
#define TMSUCCESS 0x04000000L
/**
 * caller is suspending, not ending, association
 */
#define TMSUSPEND 0x02000000L
/**
 * start a recovery scan
 */
#define TMSTARTRSCAN 0x01000000L
/**
 * end a recovery scan
 */
#define TMENDRSCAN 0x00800000L
/**
 * wait for any asynchronous operation
 */
#define TMMULTIPLE 0x00400000L
/**
 * caller is joining existing transaction branch
 */
#define TMJOIN 0x00200000L
/**
 * caller intends to perform migration
 */
#define TMMIGRATE 0x00100000L
/*
 * NOTE: this is an extension implemented by LIXA but it's outside the XA
 *       standard
 * Flag definitions for xta_
 */
/**
 * caller is creating a new branch under the scope of an already existent
 * global transaction
 */
#define TMXTABRANCH   0x00008000L



/*
 * ax_() return codes (transaction manager reports to resource manager)
 */
/**
 * caller is joining existing transaction branch
 */
#define TM_JOIN 2
/**
 * caller is resuming association with suspended transaction branch
 */
#define TM_RESUME 1
/**
 * normal execution
 */
#define TM_OK 0
/**
 * an error occurred in the transaction manager
 */
#define TMER_TMERR -1
/**
 * invalid arguments were given
 */
#define TMER_INVAL -2
/**
 * routine invoked in an improper context
 */
#define TMER_PROTO -3



/*
 * xa_() return codes (resource manager reports to transaction manager)
 */
/**
 * the inclusive lower bound of the rollback codes
 */
#define XA_RBBASE 100
/**
 * the rollback was caused by an unspecified reason
 */
#define XA_RBROLLBACK XA_RBBASE
/**
 * the rollback was caused by a communication failure
 */
#define XA_RBCOMMFAIL XA_RBBASE+1
/**
 * a deadlock was detected
 */
#define XA_RBDEADLOCK XA_RBBASE+2
/**
 * a condition that violates the integrity of the resources was detected
 */
#define XA_RBINTEGRITY XA_RBBASE+3
/**
 * the resource manager rolled back the transaction branch for a reason not
 * on this list
 */
#define XA_RBOTHER XA_RBBASE+4
/**
 * a protocol error occurred in the resource manager
 */
#define XA_RBPROTO XA_RBBASE+5
/**
 * a transaction branch took too long
 */
#define XA_RBTIMEOUT XA_RBBASE+6
/**
 * may retry the transaction branch
 */
#define XA_RBTRANSIENT XA_RBBASE+7
/**
 * the inclusive upper bound of the rollback codes
 */
#define XA_RBEND XA_RBTRANSIENT
/**
 * resumption must occur where suspension occurred
 */
#define XA_NOMIGRATE 9
/**
 * the transaction branch may have been heuristically completed
 */
#define XA_HEURHAZ 8
/**
 * the transaction branch has been heuristically committed
 */
#define XA_HEURCOM 7
/**
 * the transaction branch has been heuristically rolled back
 */
#define XA_HEURRB 6
/**
 * the transaction branch has been heuristically committed and rolled back
 */
#define XA_HEURMIX 5
/**
 * routine returned with no effect and may be reissued
 */
#define XA_RETRY 4
/**
 * the transaction branch was read-only and has been committed
 */
#define XA_RDONLY 3
/**
 * normal execution
 */
#define XA_OK 0
/**
 * asynchronous operation already outstanding
 */
#define XAER_ASYNC -2
/**
 * a resource manager error occurred in the transaction branch
 */
#define XAER_RMERR -3
/**
 * the XID is not valid
 */
#define XAER_NOTA -4
/**
 * invalid arguments were given
 */
#define XAER_INVAL -5
/**
 * routine invoked in an improper context
 */
#define XAER_PROTO -6
/**
 * resource manager unavailable
 */
#define XAER_RMFAIL -7
/**
 * the XID already exists
 */
#define XAER_DUPID -8
/**
 * resource manager doing work outside global transaction
 */
#define XAER_OUTSIDE -9
/**
 * Oracle proprietary extension to support a specific RAC feature. This is a
 * nonstandard value. Please refer to original Oracle documentation
 */
#define XAER_AFFINITY -10
/**
 * This is a special XAER return code used to manage the special situation
 * of xa_prepare with errors and xa_rollback with errors. This is not an XA
 * extension, but an internal LIXA implementation dependent value
 */
#define LIXA_XAER_HAZARD   -20



#endif /* ifndef XA_H */
/*
 * End of xa.h header
 */
