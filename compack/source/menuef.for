      SUBROUTINE MENUEF(SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activate/Deactivate the MENUDO event flag. 
C-   Use MSETEF(COMMAND) to set the event flag. The flag is cleared
C-   by MENUDO. The command specified in MSETEF will be returned by
C-   MENUDO when the local event flag is set.
C-
C-   Inputs  : SWITCH   [L]     TRUE to activate event_flag
C-   Outputs :
C-   Controls:
C-
C-   Created   8-AUG-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL SWITCH
C
C
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE 'D0$INC:KEYCOM.INC'
C
      INTEGER SMG$DISABLE_UNSOLICITED_INPUT
      INTEGER SMG$ENABLE_UNSOLICITED_INPUT
      INTEGER ISTAT
C
      EXTERNAL KEYAST
C----------------------------------------------------------------------
      EVENT_MODE = SWITCH
C
      IF ( EVENT_MODE ) THEN
        ISTAT = SMG$ENABLE_UNSOLICITED_INPUT(PASTID,KEYAST,MAINID)
        IF ( ISTAT.eq.0 ) CALL MSGSCR(ISTAT,' ')
      ELSE
        ISTAT = SMG$DISABLE_UNSOLICITED_INPUT (PASTID)
        IF ( ISTAT.eq.0 ) CALL MSGSCR(ISTAT,' ')
      ENDIF
  999 RETURN
      END
