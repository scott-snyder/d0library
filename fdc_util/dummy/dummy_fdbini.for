      SUBROUTINE FDBINI(RUNNO,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy for now.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-FEB-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUNNO
      LOGICAL IOK
C----------------------------------------------------------------------
      IOK = .FALSE.
      CALL INTMSG(' FDBINI: Dummy called, need DBL3 version from T_U')
C----------------------------------------------------------------------
  999 RETURN
      END
