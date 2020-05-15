      * Copyright (c) 2009-2020, Christian Ferrari 
      * <tiian@users.sourceforge.net>
      * All rights reserved.
      *
      * This file is part of LIXA.
      *
      * LIXA is free software: you can redistribute it and/or modify
      * it under the terms of the GNU General Public License version 2i
      * as published by the Free Software Foundation.
      *
      * LIXA is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the 
      * GNU General Public License along with LIXA.
      * If not, see <http://www.gnu.org/licenses/>.
      *
        IDENTIFICATION DIVISION.
        PROGRAM-ID. EXAMPLE1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      * Include TX definitions
        01 TX-RETURN-STATUS.
           COPY TXSTATUS.
        01 TX-INFO-AREA.
           COPY TXINFDEF.
      * Include LIXA definitions
           COPY LIXAXID.
        PROCEDURE DIVISION.
        000-MAIN.
            DISPLAY 'Executing EXAMPLE1'.
            MOVE ZERO TO TX-RETURN-STATUS.
      * Calling TXOPEN (tx_open)
            CALL "TXOPEN" USING TX-RETURN-STATUS.
            DISPLAY 'TXOPEN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      * Calling TXBEGIN (tx_begin)
            CALL "TXBEGIN" USING TX-RETURN-STATUS.
            DISPLAY 'TXBEGIN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      * Inspect transaction info
            PERFORM INFO-PARA THRU INFO-PARA.
      * Calling TXCOMMIT (tx_commit)
            CALL "TXCOMMIT" USING TX-RETURN-STATUS.
            DISPLAY 'TXCOMMIT returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
            PERFORM INFO-PARA THRU INFO-PARA.
      * Set non default parameters
      * LIXA does not support this option
            MOVE 1 TO COMMIT-RETURN.
            CALL "TXSETCOMMITRET" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETCOMMITRET returned value ' TX-STATUS.
            IF NOT TX-NOT-SUPPORTED THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      * LIXA supports timeout
            MOVE 5 TO TRANSACTION-TIMEOUT.
            CALL "TXSETTIMEOUT" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETTIMEOUT returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      * LIXA supports transaction control
            MOVE 1 TO TRANSACTION-CONTROL.
            CALL "TXSETTRANCTL" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETTRANCTL returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
            PERFORM INFO-PARA THRU INFO-PARA.
      * Transaction control must be resetted to avoid error during close
            MOVE 0 TO TRANSACTION-CONTROL.
            CALL "TXSETTRANCTL" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETTRANCTL returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
            PERFORM INFO-PARA THRU INFO-PARA.
      * Calling TXBEGIN (tx_begin)
            CALL "TXBEGIN" USING TX-RETURN-STATUS.
            DISPLAY 'TXBEGIN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      * Calling TXROLLBACK (tx_rollback)
            CALL "TXROLLBACK" USING TX-RETURN-STATUS.
            DISPLAY 'TXROLLBACK returned value ' TX-STATUS.
            IF NOT TX-OK
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      * Calling TXCLOSE (tx_close)
            CALL "TXCLOSE" USING TX-RETURN-STATUS.
            DISPLAY 'TXCLOSE returned value ' TX-STATUS.
            IF NOT TX-OK
               STOP RUN RETURNING 1
            DISPLAY 'Execution terminated!'.
            STOP RUN.
      * Calling TXINFORM (tx_info)
            INFO-PARA.
            CALL "TXINFORM" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXINFORM returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
            CALL "LIXAXIDSERIALIZE" USING TX-INFO-AREA LIXA-SER-XID
                 TX-STATUS.
            DISPLAY '  XID-REC/FORMAT-ID:     ' FORMAT-ID.
            DISPLAY '  XID-REC/GTRID-LENGTH:  ' GTRID-LENGTH.
            DISPLAY '  XID-REC/BRANCH-LENGTH: ' BRANCH-LENGTH.
            DISPLAY '  XID-REC/XID (SERIAL.): ' LIXA-SER-XID.
            DISPLAY '  TRANSACTION-MODE :     ' TRANSACTION-MODE.
            IF TX-NOT-IN-TRAN THEN
               DISPLAY '    [TX-NOT-IN-TRAN]'.
            IF TX-IN-TRAN THEN
               DISPLAY '    [TX-IN-TRAN]'.
            DISPLAY '  COMMIT-RETURN :        ' COMMIT-RETURN.
            IF TX-COMMIT-COMPLETED THEN
               DISPLAY '    [TX-COMMIT-COMPLETED]'.
            IF TX-COMMIT-DECISION-LOGGED THEN
               DISPLAY '    [TX-COMMIT-DECISION-LOGGED]'.
            DISPLAY '  TRANSACTION-CONTROL :  ' TRANSACTION-CONTROL.
            IF TX-UNCHAINED THEN
               DISPLAY '    [TX-UNCHAINED]'.
            IF TX-CHAINED THEN
               DISPLAY '    [TX-CHAINED]'.
            DISPLAY '  TRANSACTION-TIMEOUT :  ' TRANSACTION-TIMEOUT.
            IF NO-TIMEOUT THEN
               DISPLAY '    [NO-TIMEOUT]'.
            DISPLAY '  TRANSACTION-STATE :    ' TRANSACTION-STATE.
            IF TX-ACTIVE THEN
               DISPLAY '    [TX-ACTIVE]'.
            IF TX-TIMEOUT-ROLLBACK-ONLY THEN
               DISPLAY '    [TX-TIMEOUT-ROLLBACK-ONLY]'.
            IF TX-ROLLBACK-ONLY THEN
               DISPLAY '    [TX-ROLLBACK-ONLY]'.
            

