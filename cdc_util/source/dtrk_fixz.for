      SUBROUTINE DTRK_FIXZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : "Fixes" z position of DTRK banks caused by
C-                          nonlinearities in the CDC delay line. Fix
C-                          applied is that derived from muon calibration.
C-                          Status bit in DTRH is checked/set.
C-
C-   Inputs  : None
C-   Outputs : overwrites Z of center of gravity in DTRK banks
C-   Controls: RCP
C-
C-   Created  10-APR-1995   NORMAN A. GRAF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LDTRK,GZDTRK,LDTRH,GZDTRH
      LOGICAL FIXED,FIRST
      DATA FIRST /.TRUE./
      LOGICAL EZERROR
      INTEGER IER
      INTEGER POLYORDER,II
      REAL POLYCOEFF(10),ZOLD,DZ
C
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CD_FIX_RCP')
        IF ( EZERROR(IER) ) THEN
          IF(IER.NE.0) THEN
            CALL ERRMSG('DTRK_FIXZ','DTRK_FIXZ',
     &        'Unable to find CD_FIX_RCP','F')
          ENDIF
        ENDIF
        CALL EZGET('POLYORDER',POLYORDER,IER)
        CALL EZGET('POLYCOEFF',POLYCOEFF,IER)
        CALL EZRSET
      ENDIF
C
C ****  Do we need to correct? Check status bit here...
C
      LDTRH = GZDTRH()
      FIXED = .FALSE.
      IF (LDTRH .GT. 0) THEN
        IF (IBITS(IQ(LDTRH),0,1) .NE. 0) FIXED= .TRUE.
        IF (IBITS(IQ(LDTRH),1,1) .NE. 0) FIXED= .TRUE.
        IF (IBITS(IQ(LDTRH),2,1) .NE. 0) FIXED= .TRUE.
      ENDIF
      IF(.NOT.FIXED) THEN
        LDTRK = GZDTRK(0)
C
C ****  Loop over CDC tracks...
C
        DO WHILE (LDTRK.GT.0)
          ZOLD = Q(LDTRK+11)
          DZ   = 0
          IF(ZOLD.NE.0) THEN
            DO II = 1,POLYORDER
              DZ = DZ + POLYCOEFF(II)*ZOLD**(II-1)
            ENDDO
          ENDIF
          Q(LDTRK+11) = Q(LDTRK+11) + DZ
          LDTRK = LQ(LDTRK)
        ENDDO
C
C ****  Set status bit to indicate correction has been made...
C
        IQ(LDTRH) = IBSET(IQ(LDTRH),2)
      ENDIF
C
  999 RETURN
      END
