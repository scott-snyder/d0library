      SUBROUTINE BKCAD1(LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CREATE BANK CAD1 
C-
C-   Output : LADDR = pointer to CAD1 bank in ZEBCOM
C-
C-   Created  13-OCT-1988 BY A.P.White
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
C----------------------------------------------------------------------
C
       INTEGER LCAD1,MPCAD1(5)
       INTEGER LADDR,KK
C
       DATA MPCAD1/0,0,0,27790,1/
C
C--- BOOK THE CAD1 BANK
C
       CALL UCTOH('CAD1',MPCAD1,4,4)
C
       CALL MZLIFT(IXMAIN,LCAD1,LHEAD,-IZCAD1,MPCAD1,0)
       LADDR=LCAD1
C
      RETURN
      END
