      SUBROUTINE BKSACL(LSAMH,LHIT) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book banks 'SACL'.
C-
C-   Created  13-MAR-1992   V.Sirotenko
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSACL.LINK'
      INTEGER LSAMH,LHIT,MMBK(5) 
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
         CALL UCTOH('SACL',MMBK(1),4,4)           ! IDH (hollerith bank name)
         MMBK(2) = 1                              ! NL (total number of links)
         MMBK(3) = 1                              ! NS (number of struct. links)
         CALL MZFORM('SACL','/1B 2I 1F 1I 10F',MMBK(5)) ! NIO (bank format)
         FIRST   = .FALSE.
      ENDIF
      MMBK(4) = IQ(LSAMH-1)
      CALL MZLIFT(IXMAIN,LHIT,LSAMH,-IZSACL,MMBK,0)
      RETURN
      END
