      SUBROUTINE MNHIT(NMOD,NCEL,IPTR,ILAT,IPMT,JERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process scintillator hit information
C-
C-   Inputs  : NMOD    - Module ID
C-             NCEL    - Cell number
C-             IPTR    - Pointer to hit in MUHP
C-             ILAT    - Latch bits set
C-             IPMT(2) - Raw PMT counts
C-   Outputs : JERR    - 0 if hit is OK
C-
C-   Created   15-OCT-1992  M. Fortner
C-   Updated   17-DEC-1994  R. Markeloff. Reverse direction of time
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NMOD,NCEL,IPTR,ILAT,IPMT(2),JERR
      INTEGER  I,NPMT,IFLG,NCEL1
      REAL TZER(2),TSLP(2),XYZ(3),DXYZ(3),TIME(2),TMIN,TMAX
      REAL ADCMIN, ADCMAX
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      DATA TMIN,TMAX/-800.,800./
      DATA ADCMIN,ADCMAX/-100.,4000./
C
C               Get geometry and calibration constants
C
      JERR = 0
      CALL MNGTCN(NMOD,NCEL,NPMT,XYZ,DXYZ,TZER,TSLP,JERR)
      IF ( JERR.NE.0 ) GOTO 100    ! Invalid address
C
C               Calculate time and store hit
C
      IFLG = 0
      IF (MOD(ILAT,2).EQ.0) IFLG=IFLG+1
      IF (ILAT.LT.2.AND.NPMT.EQ.2) IFLG=IFLG+2
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
      IF ( NMOD.EQ.302.OR.NMOD.EQ.192 ) THEN
        IFLG = IFLG + 256
      END IF
      CALL MSCTFL(2,NMOD,NCEL,IFLG,IPTR,NPMT,TIME,XYZ,DXYZ)
C
C               Try second cell as separate scintillator
C
      IF (NPMT.EQ.1) THEN
        NCEL1 = NCEL + 4
        CALL MNGTCN(NMOD,NCEL1,NPMT,XYZ,DXYZ,TZER,TSLP,JERR)
        IF ( JERR.NE.0 ) GOTO 100    ! Invalid address
        IFLG = 1 - ILAT/2
        TIME(1) = FLOAT(IPMT(2))
        TIME(2) = 0.
        IF (TIME(I).LT.ADCMIN.OR.TIME(I).GT.ADCMAX) THEN
          IFLG = IFLG + 4
          TIME(1) = -10000
        ELSE
          TIME(1) = (TIME(1)-TZER(1))*TSLP(1)
          IF (TIME(1).LT.TMIN.OR.TIME(1).GT.TMAX) THEN
            IFLG = IFLG + 16
            TIME(1) = -10000
          ENDIF
        ENDIF
        DO I=1,2
          IF ( TSLP(I).LT.0.0 ) THEN
            IFLG = IFLG + 2**(I+5)
          END IF
        END DO
        CALL MSCTFL(2,NMOD,NCEL1,IFLG,IPTR,NPMT,TIME,XYZ,DXYZ)
      ENDIF
C
C               Error handling
C
  100 IF (JERR.NE.0) THEN
        MESSID = 'MNHIT: illegal scint number'
        WRITE(MESSAG,115) NMOD,NCEL
  115   FORMAT('Module = ',I3,' Cell = ',I3,' ')
        CALLER='MNHIT'
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
      ENDIF
C
  999 RETURN
      END
