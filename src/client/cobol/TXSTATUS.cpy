      * 01 TX-RETURN-STATUS.
            05 TX-STATUS PIC S9(9) COMP-5.
      * Option not supported
                88 TX-NOT-SUPPORTED         VALUE 1.
      * Normal execution
                88 TX-OK                    VALUE 0.
      * Application is in an RM local transaction
                88 TX-OUTSIDE               VALUE -1.
      * Transaction was rolled back
                88 TX-ROLLBACK              VALUE -2.
      * Transaction was partially committed and partially rolled back
                88 TX-MIXED                 VALUE -3.
      * Transaction may have been partially committed and partially rolled back
                88 TX-HAZARD                VALUE -4.
      * Routine invoked in an improper context
                88 TX-PROTOCOL-ERROR        VALUE -5.
      * Transient error
                88 TX-ERROR                 VALUE -6.
      * Fatal error
                88 TX-FAIL                  VALUE -7.
      * Invalid arguments were given
                88 TX-EINVAL                VALUE -8.
      * The transaction was heuristically committed
                88 TX-COMMITTED             VALUE -9.
      * Transaction committed plus new transaction could not be started
                88 TX-NO-BEGIN              VALUE -100.
      * Transaction rollback plus new transaction could not be started
                88 TX-ROLLBACK-NO-BEGIN     VALUE -102.
      * Mixed plus new transaction could not be started
                88 TX-MIXED-NO-BEGIN        VALUE -103.
      * Hazard plus new transaction could not be started
                88 TX-HAZARD-NO-BEGIN       VALUE -104.
      * Heuristically committed plus transaction could not be started
                88 TX-COMMITTED-NO-BEGIN    VALUE -109.
           
