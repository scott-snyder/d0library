      SUBROUTINE DTRK_FIX_DEDX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : "Fixes" MIP values in DTRK banks to account
C-                          for unmonitored fluctuations during the run.
C-                          Status bit in DTRH is checked/set.
C-
C-   Inputs  : None
C-   Outputs : overwrites MIP in DTRK banks
C-   Controls: RCP and database
C-
C-   Created  25-JUL-1995   Norman A. Graf
C-   Updated   2-NOV-1995   NORMAN A. GRAF  Added check on Monte Carlo 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LDTRK,GZDTRK,LDTRH,GZDTRH
      LOGICAL FIXED,FIRST
      DATA FIRST /.TRUE./
      LOGICAL EZERROR, MONTE_CARLO_DATA
      INTEGER IER,NRUNS,RUN,RUNNO,RUNOLD,I,MIP_RUNS(1000)
      REAL MIP,MIPOLD,MIP_CORR(1000),MIP_CORRECTION
C
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CD_FIX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL INRCP('CD_FIX_RCP',IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('DTRK_FIX_DEDX','DTRK_FIX_DEDX',
     &        'Unable to find bank CD_FIX_RCP','F')
          ENDIF
        ENDIF
        CALL EZGETA('DE_DX_RUNS',0,0,0,NRUNS,IER)
        CALL EZGET('DE_DX_RUNS',MIP_RUNS,IER)
        CALL EZGET('DE_DX_CORRS',MIP_CORR,IER)
        CALL EZRSET
        RUNOLD = 0
        IF (IQ(LHEAD+1) .GT. 1000)  MONTE_CARLO_DATA= .TRUE.
      ENDIF
C
C ****  Do we need to correct? Check status bit here...
C
      LDTRH = GZDTRH()
      FIXED = .FALSE.
      IF (LDTRH .GT. 0) THEN
        IF (IBITS(IQ(LDTRH),3,1) .NE. 0) FIXED= .TRUE.
      ENDIF
      IF((.NOT.FIXED) .AND. (.NOT.MONTE_CARLO_DATA)) THEN
C
C ****  Get correction here. Values in RCP are derived from EXAMINE data
C ****  by Paul Rubinov.
C
        RUN = RUNNO()
        IF(RUN.NE.RUNOLD) THEN
          RUNOLD = RUN
          DO I = 1,NRUNS-1
            IF(RUN.GE.MIP_RUNS(I).AND.RUN.LT.MIP_RUNS(I+1)) THEN
              MIP_CORRECTION = MIP_CORR(I)
            ENDIF
          ENDDO
        ENDIF
C
C ****  Loop over CDC tracks...
C
        LDTRK = GZDTRK(0)
        DO WHILE (LDTRK.GT.0)
          MIPOLD = Q(LDTRK+20)
C
C ****  Apply correction here...
C
          MIP = MIPOLD/MIP_CORRECTION
C
          Q(LDTRK+20) = MIP
          LDTRK = LQ(LDTRK)
        ENDDO
C
C ****  Set status bit to indicate correction has been made...
C
        IQ(LDTRH) = IBSET(IQ(LDTRH),3)
      ENDIF
C
  999 RETURN
      END
