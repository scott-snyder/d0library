      SUBROUTINE INIFLG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book flags for D0USER framework
C-
C-   Created   5-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL DMPINI         ! book flags for event dumps
      CALL FLGBK('CANCEL_INTER',1)
      CALL FLGBK('CANCEL_EXAMINE',1)
      CALL FLGBK('EVENT_DISPLAY',1)
      CALL FLGBK('EXAMINE',1)
      CALL FLGBK('NO_INPUT_FILE',1)
      CALL FLGBK('PAUSE',1)
      CALL FLGBK('QUIT',1)
      CALL FLGBK('STATUS',1)
      CALL FLGBK('SUMMARIES',1)
      CALL FLGBK('WRITE_EVENT',1)
      CALL FLGBK('STORE_HISTOS',1)
      CALL FLGBK('MORE_EVENTS',1)
C
C           Defaults
      CALL FLGSET('NO_INPUT_FILE',.FALSE.)
      CALL FLGSET('QUIT',.FALSE.)
  999 RETURN
      END
