      LOGICAL FUNCTION TOP_LEPTONS_GOOD_RUN(RUN_NO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests Current Run against list of know bad
C-                         Runs. Returns .TRUE./.FALSE. for Good/Bad.
C-
C-   Inputs  : 
C-              RUN_NO  - Current Event Run Number
C-   Outputs : 
C-              None
C-   Controls: 
C-              
C-   Created   2-NOV-1992   Stephen J. Wimpenny
C-   Modified 13-Nov-1992   Run Range Checking Added + autoflag MonteCarlo Runs
C-                          (Run.No.<10k) as Good
C-   Modified 15-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL DO_RUN_SELECT,DO_RANGE_SELECT,FIRST
      INTEGER I,IER,FIRST_RUN,LAST_RUN
      INTEGER RUN_NO,NO_BAD_RUNS,NO_BAD_RUN_MAX,BAD_RUN_NOS(500)
C
      DATA FIRST,NO_BAD_RUN_MAX/.TRUE.,500/
C
      TOP_LEPTONS_GOOD_RUN=.TRUE.
C
C *** Test for MonteCarlo Runs (Run Range usually 1 - allow up to 10,000)
C *** => automatically flag as good and skip further checking
C
      IF(RUN_NO.LT.10000) GO TO 999
C 
      IF(FIRST) THEN
C
C *** Read Bad Run Numbers test from RCP file
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_RUN_RANGE_SELECT',DO_RANGE_SELECT,IER)
        IF(DO_RANGE_SELECT) THEN
          IF(IER.EQ.0) CALL EZGET('FIRST_RUN',FIRST_RUN,IER)
          IF(IER.EQ.0) CALL EZGET('LAST_RUN',LAST_RUN,IER)
        ENDIF
        IF(IER.EQ.0) CALL EZGET('DO_GOOD_RUN_SELECT',DO_RUN_SELECT,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_GOOD_RUN',' ','F')
C
C *** Now Get Bad Run List
C
        IF(DO_RUN_SELECT) THEN
          CALL EZPICK('BAD_RUN_RCP')
          CALL ERRMSG('Reading BAD_RUN_RCP','GOOD_RUN',' ','W')
          IF(IER.EQ.0) CALL EZGETA('BAD_RUN_NOS',0,0,0,NO_BAD_RUNS,IER)
          IF(NO_BAD_RUNS.LE.NO_BAD_RUN_MAX) THEN
            IF(IER.EQ.0) CALL EZGET('BAD_RUN_NOS',BAD_RUN_NOS,IER)
          ELSE
            CALL ERRMSG('Too Many Bad Runs To Store',
     1        'TOP_LEPTONS_GOOD_RUN',' ','F')
          ENDIF
          CALL EZRSET
          IF (IER.NE.0) CALL ERRMSG('Error in BAD_RUN_RCP',
     &      'TOP_LEPTONS_GOOD_RUN',' ','F')
        ENDIF
        FIRST=.FALSE.
      ENDIF
C
      IF(.NOT.DO_RANGE_SELECT) GO TO 500
C
C *** Test to see if runs lie in requested range
C
      IF(RUN_NO.LT.FIRST_RUN.OR.RUN_NO.GT.LAST_RUN) THEN
        TOP_LEPTONS_GOOD_RUN=.FALSE.
      ENDIF
  500 IF(.NOT.DO_RUN_SELECT) GO TO 999
C
C *** Test against known Bad Run List
C
      IF(NO_BAD_RUNS.LT.1) GO TO 999
      DO I=1,NO_BAD_RUNS
        IF(RUN_NO.EQ.BAD_RUN_NOS(I)) THEN
          TOP_LEPTONS_GOOD_RUN=.FALSE.
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
