      LOGICAL FUNCTION MENSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Menu for SAMUS
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  20-SEP-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0COM.INC'        ! Selected MENU ID and command
C----------------------------------------------------------------------
      MENSAM = .TRUE.
      IF ( NMENU .NE. IDSAM ) GOTO 999
C
  999 RETURN
      END
