      LOGICAL FUNCTION TRKSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SAMUS - handle any procedure needed after a
C-                         full track is finished
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
      TRKSAM = .TRUE.
      IF ( DSAM.LT.2 ) GOTO 999
C
  999 RETURN
      END
