      LOGICAL FUNCTION TEVSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called before each event
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  20-SEP-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      TEVSAM = .TRUE.
      IF ( DSAM.LT.2 ) GOTO 999
C
  999 RETURN
      END
