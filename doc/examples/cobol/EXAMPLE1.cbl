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
        PROGRAM-ID. EXAMPLE1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      * Include TX definitions
        01 TX-RETURN-STATUS.
           COPY TXSTATUS.
        01 TX-INFO-AREA.
           COPY TXINFDEF.
        PROCEDURE DIVISION.
        000-MAIN.
            DISPLAY 'Executing EXAMPLE1'.
            MOVE ZERO TO TX-RETURN-STATUS.
      * Calling TXOPEN (tx_open)
            CALL "TXOPEN" USING TX-RETURN-STATUS.
            DISPLAY 'TXOPEN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
      * Calling TXBEGIN (tx_begin)
            CALL "TXBEGIN" USING TX-RETURN-STATUS.
            DISPLAY 'TXBEGIN returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
      * Calling TXCOMMIT (tx_commit)
            CALL "TXCOMMIT" USING TX-RETURN-STATUS.
            DISPLAY 'TXCOMMIT returned value ' TX-STATUS.
            IF NOT TX-OK THEN
               DISPLAY 'Exiting...'
               STOP RUN
            END-IF.
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
