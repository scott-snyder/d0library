      SUBROUTINE BKCAD2(LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CREATE BANK CAD2 
C-
C-   Output : LADDR = pointer to CAD2 bank in ZEBCOM
C-
C-   Created  13-OCT-1988 BY A.P.White
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
C----------------------------------------------------------------------
C
       INTEGER LCAD2,MPCAD2(5)
       INTEGER LADDR,KK
C
       DATA MPCAD2/0,0,0,27790,1/
C
C--- BOOK THE CAD2 BANK
C
       CALL UCTOH('CAD2',MPCAD2,4,4)
C
       CALL MZLIFT(IXMAIN,LCAD2,LHEAD,-IZCAD2,MPCAD2,0)
       LADDR=LCAD2
C
      RETURN
      END
