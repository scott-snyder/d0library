      LOGICAL FUNCTION CLNSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set flags/values that depend on FFREAD cards
C-                         read in parameter input
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
      CLNSAM = .TRUE.
      IF ( DSAM.LE.0 ) GOTO 999
C
  999 RETURN
      END
