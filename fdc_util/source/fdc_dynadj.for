      SUBROUTINE FDC_DYNADJ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get environmental corrections for FDC GAIN.
C-               Adjust global GAIN constant for each FDC subdetector.
C-               
C-               Use FDC_LUMADJ for gain sagging for RUN>LUM_START_RUN
C-               Use FDC_ENVADJ for T,P corrections for RUN>ENV_START_RUN
C-
C-   Created   5-APR-1993   Robert E. Avery
C-                       Based on VTX_DYNADJ by Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C I/O:  
      INTEGER LUM_STATUS,ENV_STATUS
c Locals:
      INTEGER LUM_STAT,ENV_STAT
      INTEGER LUM_START_RUN,LUM_STOP_RUN,ENV_START_RUN,ERR
      INTEGER RUN,LAST_RUN
      INTEGER HALF, UNIT 
      INTEGER LKFGUN 
      REAL    GAIN 
      REAL    GFAC_RCP(0:1,0:1), GFAC_LUM(0:1,0:1),GFAC_ENV      
C
      LOGICAL FIRST,NO_CORRECTION
      LOGICAL DO_LUM,DO_ENV,DO_LIST   ! Corrections SHOULD be applied
      LOGICAL NEW_LUM,NEW_ENV,NEW_LIST ! Corrections ARE TO BE applied
      LOGICAL APPLY_LUM,APPLY_ENV,APPLY_LIST
c Functions:
      INTEGER GZFGUN
      INTEGER RUNNO
C
      SAVE FIRST,GFAC_LUM,GFAC_RCP,GFAC_ENV
      SAVE LUM_STAT,ENV_STAT
      SAVE LUM_START_RUN,ENV_START_RUN
      SAVE APPLY_LUM,APPLY_ENV,APPLY_LIST
c Data:
      DATA FIRST/.TRUE./
      DATA LAST_RUN/-1/
C----------------------------------------------------------------------
      IF (IQ(LHEAD+1) .GT. 1000) GO TO 999            ! MONTE CARLO
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('LUM_START_RUN',LUM_START_RUN,ERR)
        CALL EZGET_i('ENV_START_RUN',ENV_START_RUN,ERR)
        CALL EZGET_l('APPLY_LIST_CORR',APPLY_LIST,ERR)
        CALL EZGET_l('APPLY_LUM_CORR',APPLY_LUM,ERR)
        CALL EZGET_l('APPLY_ENV_CORR',APPLY_ENV,ERR)
        CALL EZRSET
C
        GFAC_ENV = 1.
        DO UNIT =  0, 1
          DO HALF =  0, 1
            GFAC_RCP(HALF,UNIT) = 1.
            GFAC_LUM(HALF,UNIT) = 1.
          ENDDO
        ENDDO
      ENDIF
      LUM_STAT = 0
      ENV_STAT = 0
C
      RUN = RUNNO()
      DO_LUM = RUN.GE.LUM_START_RUN .AND. APPLY_LUM
      DO_ENV = RUN.GE.ENV_START_RUN .AND. APPLY_ENV
      DO_LIST = APPLY_LIST
      NO_CORRECTION = .NOT. (DO_LUM .OR. DO_ENV .OR. DO_LIST)
C
C ****  NORMAL ENTRY
C
      IF (NO_CORRECTION) GO TO 999
      NEW_LIST = .FALSE.
      IF (DO_LIST.AND.(RUN.NE.LAST_RUN)) THEN
        CALL FDC_RCPADJ(RUN,GFAC_RCP,NEW_LIST)
      ENDIF
      NEW_LUM = .FALSE.
      IF (DO_LUM) THEN
        CALL FDC_LUMADJ(RUN,GFAC_LUM,LUM_STAT,NEW_LUM)
      ENDIF
      NEW_ENV = .FALSE.
      IF (DO_ENV) THEN
        CALL FDC_ENVADJ(GFAC_ENV,ENV_STAT,NEW_ENV)
      ENDIF
C
C ****  If nothing has been updated, we're done.  Otherwise, replace the
C ****  gain contents of FGUN (global gain factors).
C
      IF ( NEW_LUM .OR. NEW_ENV .OR. NEW_LIST ) THEN
        DO UNIT =  0, 1
          DO HALF =  0, 1
            GAIN = GFAC_RCP(HALF,UNIT) * GFAC_LUM(HALF,UNIT) * GFAC_ENV
            LKFGUN = GZFGUN(HALF,UNIT)
            C(LKFGUN+1) = GAIN
          ENDDO
        ENDDO
      ENDIF
      LAST_RUN = RUN
      GOTO 999
C
      ENTRY FDC_DYNSTAT(ENV_STATUS,LUM_STATUS)
      ENV_STATUS = ENV_STAT
      LUM_STATUS = LUM_STAT
  999 RETURN
      END
