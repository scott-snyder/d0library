      SUBROUTINE MNHPRO(NMOD,NCEL,ILAT,IPMT,IFLG,TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw scintillator data to times
C-
C-   Inputs  : NMOD    - Module ID
C-             NCEL    - Cell number
C-             ILAT    - Latch bits set
C-             IPMT(2) - Raw PMT counts
C-
C-   Outputs : IFLG    - Flag word for MSCT bank
C-             TIME(2) - Corrected times
C-
C-   Created   3-MAY-1995  M. Fortner
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NMOD,NCEL,ILAT,IPMT(2)
      INTEGER  I,NPMT,IFLG,MCEL,NSCT,ITYP
      REAL TZER(2),TSLP(2),TIME(2),TMIN,TMAX
      REAL ADCMIN, ADCMAX
      DATA TMIN,TMAX/-800.,800./
      DATA ADCMIN,ADCMAX/-100.,4000./
C
C               Get calibration constants
C
      CALL MNMDAT(NMOD,ITYP,MCEL,NSCT)
      IF (ITYP.LT.0.OR.NCEL.GT.MCEL*4) RETURN
      NPMT = MCEL/NSCT
      CALL GTMSTC(NMOD,NCEL,TZER,TSLP)
C
C               Test for missing PMTs
C
      IFLG = 0
      IF (MOD(ILAT,2).EQ.0) IFLG=IFLG+1
      IF (ILAT.LT.2.AND.NPMT.EQ.2) IFLG=IFLG+2
C
C               Calculate time, and flag if bad
C
      DO I = 1,NPMT
        TIME(I) = FLOAT(IPMT(I))
        IF (TIME(I).LT.ADCMIN.OR.TIME(I).GT.ADCMAX) THEN
          IFLG = IFLG + I*4
          TIME(I) = -10000
        ELSE
          TIME(I) = (TZER(I)-TIME(I))*TSLP(I) 
          IF (TIME(I).LT.TMIN.OR.TIME(I).GT.TMAX) THEN
            IFLG = IFLG + I*16
            TIME(I) = -10000
          ENDIF
        ENDIF
      ENDDO
      DO I=1,2
        IF ( TSLP(I).LT.0.0 ) THEN
          IFLG = IFLG + 2**(I+5)
        END IF
      END DO
C
C		Flag EF and A-stub scintillators
      IF (ITYP.EQ.1) IFLG = IFLG + 256
      IF (ITYP.EQ.2) IFLG = IFLG + 512
C
  999 RETURN
      END
