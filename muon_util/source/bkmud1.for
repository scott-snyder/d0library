C VAX/DEC CMS REPLACEMENT HISTORY, Element BKMUD1.FOR
C *1    15-SEP-1993 17:20:02 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element BKMUD1.FOR
      SUBROUTINE BKMUD1(LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CREATE BANK MUD1 
C-
C-   Output : LADDR = pointer to MUD1 bank in ZEBCOM
C-
C-   Created  14-JUL-1993 by M.Fortner
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
C----------------------------------------------------------------------
C
       INTEGER LMUD1,MPMUD1(5)
       INTEGER LADDR,KK
C
       DATA MPMUD1/0,0,0,27790,1/
C
C--- BOOK THE MUD1 BANK
C
       CALL UCTOH('MUD1',MPMUD1,4,4)
C
       CALL MZLIFT(IXMAIN,LMUD1,LHEAD,-IZMUD1,MPMUD1,0)
       LADDR=LMUD1
C
      RETURN
      END
