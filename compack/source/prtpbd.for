      SUBROUTINE PRTPBD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the pasteboard.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-OCT-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER STATUS
      INTEGER SMG$PRINT_PASTEBOARD
C----------------------------------------------------------------------
C&IF VAXVMS
      STATUS = SMG$PRINT_PASTEBOARD(PASTID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'PRTPBD')
      ENDIF
C&ELSE
C&ENDIF
  999 RETURN
      END
