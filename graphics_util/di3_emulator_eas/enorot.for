      SUBROUTINE ENOROT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Stops rotation of retained segment followed.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   11-MAY-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:NEWDI3.INC'
C
      NOROT = .FALSE.
      IF(NUDI3) NOROT = .TRUE.
C
  999 RETURN
      END
