      SUBROUTINE DMPSTU(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     set a unit number for DUMP facility
C-   Inputs  : 
C-    LUN = unit number
C-
C-   Created  22-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      INTEGER LUN
C----------------------------------------------------------------------
C
      DUNIT=LUN
  999 RETURN
      END
