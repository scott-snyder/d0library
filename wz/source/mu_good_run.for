      LOGICAL FUNCTION MU_GOOD_RUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : exclude bad runs
C-
C-   Returned value  : true - good run
C-   Controls:         WZ.RCP has list of bad runs
C-
C-   Created  10-MAR-1993   Cecilia Gerber: bad runs for muon analysis. Taken
C-   from Uli's E_GOOD_RUN.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,LAST_STATUS
      DATA FIRST/.TRUE./
      INTEGER IMAX,IER,RUNNO,I,NMAX, FIRST_RUN, LAST_RUN
      PARAMETER( IMAX = 200 )
      INTEGER MUBADRUN(IMAX),RUN
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET ('MU_BAD_RUN',MUBADRUN,IER)
        CALL EZGET ('FIRST_RUN',FIRST_RUN,IER)
        CALL EZGET ('LAST_RUN',LAST_RUN,IER)
        IF(IER.EQ.0)CALL EZGET_SIZE ('MU_BAD_RUN',NMAX,IER)
        IF(IER.NE.0)CALL ERRMSG('RCP','MU_GOOD_RUN',
     &    'error getting RCP parameters','W')
        CALL EZRSET
        IF(NMAX.GT.IMAX)CALL ERRMSG('MUBADRUN array too small',
     &    'MU_GOOD_RUN',' ','F')
        CALL ERRMAX('BAD RUN',-1,-1)
      ENDIF
      RUN=RUNNO()
C
      
      IF ((RUN.LT.FIRST_RUN).OR.(RUN.GT.LAST_RUN)) THEN
        LAST_STATUS=.FALSE.
        GO TO 900
      ENDIF
C
      I=1
      DO WHILE(MUBADRUN(I).LT.99999.AND.I.LT.NMAX)
        IF(RUN.EQ.MUBADRUN(I))THEN
          LAST_STATUS=.FALSE.
          WRITE(MSG,1)RUN
    1     FORMAT(I6)
          CALL errmsg('bad run','MU_GOOD_RUN',MSG,'W')
          GOTO 900
        ENDIF
        I=I+1
      ENDDO
      LAST_STATUS=.TRUE.
  900 MU_GOOD_RUN=LAST_STATUS
C----------------------------------------------------------------------
  999 RETURN
      END
