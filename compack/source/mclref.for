      SUBROUTINE MCLREF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Clear MENUDO event flag.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-AUG-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
C
      INCLUDE 'D0$INC:KEYCOM.INC'
      INTEGER SYS$CLREF
      INTEGER ISTAT
C----------------------------------------------------------------------
C
C **** Clear MENUDO event flag
C
      ISTAT = SYS$CLREF (%VAL(EVENT_FLAG))
      IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C&ENDIF
  999 RETURN
      END
