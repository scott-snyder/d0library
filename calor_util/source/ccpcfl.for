      SUBROUTINE CCPCFL(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calls CCPFL to fill the CCPC bank
C-
C-   Inputs  : None
C-   Outputs : IER  [I]     0 = OK
C-   Controls: None
C-
C-   Created   31-JUL-1991 Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C----------------------------------------------------------------------
C
      IER = 0
      CALL CCPFL('CPC_',IER)
C
  999 RETURN
      END
