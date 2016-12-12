      * This copybook is NOT original but it's derived from this
      * official documentation:
      *
      * X/Open CAE Specification
      * Distributed Transaction Processing:
      * The TX (Transaction Demarcation) Specification
      * ISBN: 1-85912-094-6
      * X/Open Document Number: C504
      *
      * XID record
        05 XID-REC.
           10 FORMAT-ID PIC S9(9) COMP-5.
      * A value of -1 in FORMAT-ID means that the XID is null
           10 GTRID-LENGTH PIC S9(9) COMP-5.
           10 BRANCH-LENGTH PIC S9(9) COMP-5.
           10 XID-DATA PIC X(128).
      * Transaction mode settings
        05 TRANSACTION-MODE PIC S9(9) COMP-5.
           88 TX-NOT-IN-TRAN VALUE 0.
           88 TX-IN-TRAN VALUE 1.
      * Commit_return settings
        05 COMMIT-RETURN PIC S9(9) COMP-5.
           88 TX-COMMIT-COMPLETED VALUE 0.
           88 TX-COMMIT-DECISION-LOGGED VALUE 1.
      * Transaction_control settings
        05 TRANSACTION-CONTROL PIC S9(9) COMP-5.
           88 TX-UNCHAINED VALUE 0.
           88 TX-CHAINED VALUE 1.
      * Transaction_timeout value
        05 TRANSACTION-TIMEOUT PIC S9(9) COMP-5.
           88 NO-TIMEOUT VALUE 0.
      * Transaction_state information
        05 TRANSACTION-STATE PIC S9(9) COMP-5.
           88 TX-ACTIVE VALUE 0.
           88 TX-TIMEOUT-ROLLBACK-ONLY VALUE 1.
           88 TX-ROLLBACK-ONLY VALUE 2.
