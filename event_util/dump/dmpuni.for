      INTEGER FUNCTION DMPUNI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return value of unit for EVENT.DUMP
C-
C-   Created   9-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
C----------------------------------------------------------------------
      DMPUNI=DUNIT
  999 RETURN
      END
