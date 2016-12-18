      * Copyright (c) 2009-2016, Christian Ferrari 
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
        PROGRAM-ID. EXAMPLE5-PQL.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      * Include TX definitions
        01 TX-RETURN-STATUS.
           COPY TXSTATUS.
        01 TX-INFO-AREA.
           COPY TXINFDEF.
      * Include LIXA definitions
           COPY LIXAXID.
      * PostgreSQL connection handle
        01 PGCONN USAGE POINTER.
      * PostgreSQL result
        01 PGRES USAGE POINTER.
      * Command line argument
        01 ARGV PIC X(100) VALUE SPACES.
        01 ARGV-BOOL PIC 9(1) VALUE 0.
        88      IS-DELETE VALUES 1.
        88      IS-INSERT VALUES 0.
      *
        PROCEDURE DIVISION.
        000-MAIN.
            DISPLAY 'Executing EXAMPLE5_PQL'.
            MOVE ZERO TO TX-RETURN-STATUS.
      *
      * Delete or insert?
      *
            ACCEPT ARGV FROM ARGUMENT-VALUE.
      *     DISPLAY 'Passed argument is: "' ARGV '"'.
            IF ARGV IS EQUAL TO 'DELETE' OR
               ARGV IS EQUAL TO 'delete' THEN
               DISPLAY 'Deleting a row from the table...'
               MOVE 1 TO ARGV-BOOL
            ELSE
               DISPLAY 'Inserting a row from the table...'
            END-IF.
      *
      * Open the resource manager
      *
            CALL "TXOPEN" USING TX-RETURN-STATUS.
      *     DISPLAY 'TXOPEN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      *
      * Retrieve PostgreSQL connection
      * do NOT use standard PostgreSQL functions because they don't
      * return an XA connection, but a transaction manager independent
      * connection
      *
            CALL "LIXAPQGETCONN" USING BY REFERENCE PGCONN.
            IF PGCONN EQUAL NULL THEN
               DISPLAY 'Error: unable to retrieve a valid PostgreSQL con
       -nection'
               STOP RUN RETURNING 1
            END-IF.
      *         
      * Start a new transaction
      * 
            CALL "TXBEGIN" USING TX-RETURN-STATUS.
      *     DISPLAY 'TXBEGIN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN RETURNING 1
            END-IF.
      *
      * Execute DELETE stament
            CALL "PQexec" USING
                BY VALUE PGCONN
                BY REFERENCE "DELETE FROM authors WHERE id=1;" & x"00"
                RETURNING PGRES
            END-CALL
            STOP RUN RETURNING 0 
      * Inspect transaction info
            PERFORM INFO-PARA THRU INFO-PARA.
      * Calling TXCOMMIT (tx_commit)
            CALL "TXCOMMIT" USING TX-RETURN-STATUS.
            DISPLAY 'TXCOMMIT returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
            PERFORM INFO-PARA THRU INFO-PARA.
      * Set non default parameters
      * LIXA does not support this option
            MOVE 1 TO COMMIT-RETURN.
            CALL "TXSETCOMMITRET" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETCOMMITRET returned value ' TX-STATUS.
            IF NOT TX-NOT-SUPPORTED THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
      * LIXA supports timeout
            MOVE 5 TO TRANSACTION-TIMEOUT.
            CALL "TXSETTIMEOUT" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETTIMEOUT returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
      * LIXA supports transaction control
            MOVE 1 TO TRANSACTION-CONTROL.
            CALL "TXSETTRANCTL" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETTRANCTL returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
            PERFORM INFO-PARA THRU INFO-PARA.
      * Transaction control must be resetted to avoid error during close
            MOVE 0 TO TRANSACTION-CONTROL.
            CALL "TXSETTRANCTL" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXSETTRANCTL returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
            PERFORM INFO-PARA THRU INFO-PARA.
      * Calling TXBEGIN (tx_begin)
            CALL "TXBEGIN" USING TX-RETURN-STATUS.
            DISPLAY 'TXBEGIN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
      * Calling TXROLLBACK (tx_rollback)
            CALL "TXROLLBACK" USING TX-RETURN-STATUS.
            DISPLAY 'TXROLLBACK returned value ' TX-STATUS.
            IF NOT TX-OK
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
      * Calling TXCLOSE (tx_close)
            CALL "TXCLOSE" USING TX-RETURN-STATUS.
            DISPLAY 'TXCLOSE returned value ' TX-STATUS.
            IF NOT TX-OK
               STOP RUN.
            DISPLAY 'Execution terminated!'.
            STOP RUN.
      * Calling TXINFORM (tx_info)
            INFO-PARA.
            CALL "TXINFORM" USING TX-INFO-AREA TX-RETURN-STATUS.
            DISPLAY 'TXINFORM returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
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
            
O
