      LOGICAL FUNCTION E_GOOD_RUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : exclude bad runs
C-
C-   Returned value  : true - good run
C-   Controls:         WZ.RCP has list of bad runs
C-
C-   Created  14-JAN-1993   Ulrich Heintz
C-   Updated  29-JUL-1993   Ulrich Heintz  increase IMAX to 10000 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,LAST_STATUS
      DATA FIRST/.TRUE./
      INTEGER IMAX,IER,RUNNO,I,NMAX
      PARAMETER( IMAX = 10000 )
      INTEGER BADRUN(IMAX),RUN,LAST_RUN/-99999/
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      IF(FIRST)THEN         
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET ('E_BAD_RUN',BADRUN,IER)
        IF(IER.EQ.0)CALL EZGET_SIZE ('E_BAD_RUN',NMAX,IER)
        IF(IER.NE.0)CALL ERRMSG('RCP','E_GOOD_RUN',
     &    'error getting RCP parameters','W') 
        CALL EZRSET
        IF(NMAX.GT.IMAX)CALL ERRMSG('BADRUN array too small',
     &    'E_GOOD_RUN',' ','F')
        CALL ERRMAX('bad run',-1,-1)
      ENDIF
      RUN=RUNNO()
      IF(RUN.NE.LAST_RUN)THEN
        LAST_RUN=RUN
        I=1
        DO WHILE(BADRUN(I).LT.99999.AND.I.LE.NMAX)
          IF(RUN.EQ.BADRUN(I))THEN
            LAST_STATUS=.FALSE.
            WRITE(MSG,1)RUN
    1       FORMAT(I6)
            call errmsg('bad run','E_GOOD_RUN',MSG,'W')
            GOTO 900
          ENDIF
          I=I+1
        ENDDO
        LAST_STATUS=.TRUE.
      ENDIF
  900 E_GOOD_RUN=LAST_STATUS
C----------------------------------------------------------------------
  999 RETURN
      END
