      SUBROUTINE BEGBAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Begin batching updates to the pasteboard.
C-   Use ENDBAT to end batching and to send output to the pasteboard.
C-
C-   Inputs  : None
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
C&IF VAXVMS
      INTEGER SMG$BEGIN_PASTEBOARD_UPDATE
      INTEGER SMG$END_PASTEBOARD_UPDATE
C----------------------------------------------------------------------
      STATUS = SMG$BEGIN_PASTEBOARD_UPDATE(PASTID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'BEGBAT')
      ENDIF
C&ELSE
C&ENDIF
      RETURN
C
      ENTRY ENDBAT
C&IF VAXVMS
      STATUS = SMG$END_PASTEBOARD_UPDATE(PASTID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'ENDBAT')
      ENDIF
C&ELSE
C&ENDIF
  999 RETURN
      END
