C VAX/DEC CMS REPLACEMENT HISTORY, Element MU_STUB_FLAG_PUNCH.FOR
C *1     1-NOV-1993 13:45:14 DARIEN "a-layer stub code from S. Eno"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MU_STUB_FLAG_PUNCH.FOR
      SUBROUTINE MU_STUB_FLAG_PUNCH(I,J,NHITS,IHIT,IAMPUNCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-JUL-1993   Tom Diehl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER IHIT(28,20,16),NHITS(28,20)
      INTEGER I,J,K,L
      LOGICAL IAMPUNCH
C----------------------------------------------------------------------
      IAMPUNCH = .FALSE.

      IF(NHITS(I,J).GE.9) IAMPUNCH = .TRUE.

  999 RETURN
      END
