      SUBROUTINE NEW_RECO_RUN(IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     begin run record read, initialize run
C-
C-   Created   8-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NEW_RECO_RUN_PBD
      INTEGER IOS,PREV_RUN,RUNNO
      DATA PREV_RUN/-1/
C----------------------------------------------------------------------
C
      IF(PREV_RUN.EQ.RUNNO()) GOTO 999
      PREV_RUN=RUNNO()
      IF(NEW_RECO_RUN_PBD()) THEN
        IF(IOS.EQ.1) CALL EVTWOS
        CALL FLGSET('SKIP_RUN',.FALSE.)
      ELSE
        CALL FLGSET('SKIP_RUN',.TRUE.)
      ENDIF
  999 RETURN
      END
