      SUBROUTINE L2_FLFSEC(ZVTX,PHIMINI,PHIMAXI,THEMINI,THEMAXI,ON,
     &                     NUM_ON)
C--------------------------------------------------------------------------
C
C    Purpose and Methods : Find sectors in FDC chamber along a road
C
C    Input  : ZVTX                            = Z vertex position for road
C             PHIMINI,PHIMAXI,THEMINI,THEMAXI = road parameters
C    Output : ON(0:1,0:1,0:7,0:35)            = true for sectors on the road
C
C-   Created   x-FEB-1989   Daria Zieminska
C-   Updated   8-FEB-1990   Jeffrey W. Bantly  remove ZEBSTP refs
C-   Updated   9-APR-1990   Jeffrey Bantly  allow for negative angle inputs
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup of PARAMS,RCP, passed
C-                                          ZVTX for proper FDC road
C-                                          calculations
C-   Updated  19-AUG-1991   Robert E. Avery  Use correct Quadrant type,
C-                                          get cell length's from STP.
C-   Updated  10-MAR-1992   Qizhong Li-Demarteau  speed up for full_tracking
C-   Updated  11-MAR-1992   Susan K. Blessing   Fix VZEROing of ON.
C-   Updated  13-OCT-1992   Robert E. Avery   Use ZVTX information
C-   Updated  21-DEC-1992   Susan K. Blessing   Based on Srini's changes.
C-    Look at phi sectors first and unpack only what is needed, then do
C-    theta sectors.  Change DEGRAD to more accurate RADIAN.  Reduce
C-    redundant calculations.  Use actual width of each sector in phi.
C-   Updated  11-JAN-1993   Yi-Cheng Liu  , for L2_FDC
C--------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INCLUDE 'D0$INC:SECTLIST.INC'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,LOC,ICALL
      INTEGER STAT,IER
C
      INTEGER ICRT
      INTEGER CRATE_PHI(0:3,0:1)
      INTEGER CRATE_THETA(0:7,0:1)
      INTEGER NUM_ON
C
      REAL ZVTX,Z
      REAL PHIMINI,PHIMAXI,THEMINI,THEMAXI
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX,THETA1,THETA2
      REAL PHISEC,PHILO,PHIHI,THELO,THEHI
      REAL XC,YC,ZC,RC,RCD,LENGTH
      REAL R,CELL_LEN(0:5,2),LEN_SEC(0:5,2)
      REAL CLD,ALPHA1,ALPHA2,R1,R2,LOPHI,HIPHI
      REAL EXTPHI,EXTTHE,EXTLEN
      REAL DIMEN(6)
      REAL    PRECIS
      PARAMETER( PRECIS = 0.000001 )
C  FUNCTION:
      INTEGER FDC_QUADTYPE
C
      LOGICAL ON(0:1,0:1,0:7,0:35),FIRST,PHICHK,THECHK
      LOGICAL FOUND_PHI
      LOGICAL THELIM
C
      SAVE FIRST,CELL_LEN,LEN_SEC
      SAVE EXTPHI,EXTTHE,EXTLEN
      DATA FIRST/.TRUE./
      DATA EXTPHI,EXTTHE,EXTLEN/3*0./
C
      DATA CRATE_PHI    / 55, 35, 25, 45,           ! Hard-wired in !
     &                   105, 85, 95,115 /          ! ( Yi-Cheng Liu )
      DATA CRATE_THETA  / 15,  5,  5, 15, 15,  5,  5, 15,
     &                    75, 65, 65, 75, 75, 75, 65, 65 /
C
C-------------------------------------------------------------------------
C
C  mark the FADC crate corresponding to the hit sector to avoid wasting
C  time in later unpacking business. (YI-CHENG LIU)
C
      DO ICRT=0,11                         ! there are 11 FADC crates
        CRTLST(ICRT)=.FALSE.
      ENDDO
      NUM_ON = 0
C
C  no need to search for sectors for the full_tracking case
C
      IF (ABS(PHIMINI - 0.0) .LE. PRECIS .AND.
     &    ABS(PHIMAXI - TWOPI) .LE. PRECIS) THEN
        CALL VFILL(ON,1152,-1)
        GOTO 999
      ENDIF
C
      EXTPHI = 0.0
      EXTTHE = 0.0
      EXTLEN = 0.0
      IF (FIRST) THEN
        DO SECTOR =  0, MXSECT
C  A type
          CALL GTL2FWTX(0,5,SECTOR,DIMEN)
          CELL_LEN(SECTOR,1) = DIMEN(1)
          R = SQRT(DIMEN(4)**2 + DIMEN(5)**2) - DIMEN(2)
          LEN_SEC(SECTOR,1) = ATAN2(DIMEN(1),R) + EXTLEN
C  B type
          CALL GTL2FWTX(0,4,SECTOR,DIMEN)
          CELL_LEN(SECTOR,2) = DIMEN(2)
          R = SQRT(DIMEN(4)**2 + DIMEN(5)**2) - DIMEN(1)
          LEN_SEC(SECTOR,2) = ATAN2(DIMEN(2),R)
        ENDDO
        FIRST=.FALSE.
      END IF
C
      PHIMIN=PHIMINI
      PHIMAX=PHIMAXI
      THEMIN=THEMINI
      THEMAX=THEMAXI
      LOPHI = 2.*TWOPI
      HIPHI = -2.*TWOPI
      CALL VZERO(ON,1152)
C
C Adjust road limits if PHIMAX is gt 2*pi
      IF (PHIMAX.GT.TWOPI) THEN
        PHIMAX=PHIMAX-TWOPI
        PHIMIN=PHIMIN-TWOPI
      ENDIF
C
      DO 100 HALF=0,1
C
C Check theta limits
C   reset these theta angle checks to include zvertx=+-60.0 cm.
C   original values are 2.67,3.05,0.09,0.47 respectively.
C
        IF (HALF.EQ.0) THEN
          THELO=2.34
          THEHI=3.08
        ELSE
          THELO=0.06
          THEHI=0.80
        END IF
        THELIM=.FALSE.
        IF (THEMIN.GE.THELO.AND.THEMIN.LE.THEHI) THELIM=.TRUE.
        IF (THEMAX.GE.THELO.AND.THEMAX.LE.THEHI) THELIM=.TRUE.
        IF (THEMIN.LE.THELO.AND.THEMAX.GE.THEHI) THELIM=.TRUE.
C
        IF (THELIM) THEN
          FOUND_PHI = .FALSE.
          UNIT=1
          QUAD=0
C                       ! Loop over Phi sectors
          DO 200 SECTOR=0,35
            PHILO=(10.*FLOAT(SECTOR))*RADIAN - EXTPHI
            PHIHI=(10.*FLOAT(SECTOR+1))*RADIAN + EXTPHI
            PHICHK=.FALSE.
            IF (PHIHI.GT.TWOPI) THEN
              PHIHI=PHIHI-TWOPI
              PHILO=PHILO-TWOPI
            ENDIF
            IF (PHIMIN.LT.0. .AND. PHIHI.GT.PI) THEN
              IF ((PHIMIN+TWOPI) .LE. PHIHI) PHICHK=.TRUE.
            ELSEIF (PHIMAX.GT.PI .AND. PHILO.LT.0.) THEN
              IF (PHIMAX .GE. (PHILO+TWOPI)) PHICHK=.TRUE.
            ELSE
              IF (PHIMIN.GE.PHILO.AND.PHIMIN.LE.PHIHI) PHICHK=.TRUE.
              IF (PHIMAX.GE.PHILO.AND.PHIMAX.LE.PHIHI) PHICHK=.TRUE.
              IF (PHIMIN.LE.PHILO.AND.PHIMAX.GE.PHIHI) PHICHK=.TRUE.
            ENDIF
            IF (PHICHK) THEN
C
              FOUND_PHI = .TRUE.
              ON(HALF,UNIT,QUAD,SECTOR)=.TRUE.
C
              NUM_ON = NUM_ON + 1
              ICRT = (CRATE_PHI(SECTOR/9,HALF)-5)/10
              CRTLST(ICRT) = .TRUE.
C
              IF (PHILO.LT.LOPHI) LOPHI = PHILO
              IF (PHIHI.GT.HIPHI) HIPHI = PHIHI
            END IF
  200     CONTINUE
C                       ! Loop over Theta quads and sectors
          IF (.NOT.FOUND_PHI) GO TO 100
C
          UNIT=0
          DO 300 QUAD=0,7
            IF (QUAD.LE.3) THEN
              PHISEC=(45.+90.*FLOAT(QUAD))*RADIAN
            ELSE
              PHISEC=(90.*FLOAT(QUAD-4))*RADIAN
            END IF
C Crude phi check of entire quadrant, widest sector is a little under 2 radian
            PHILO=PHISEC-1.
            PHIHI=PHISEC+1.
            PHICHK=.FALSE.
            IF (PHIHI.GT.TWOPI) THEN
              PHIHI=PHIHI-TWOPI
              PHILO=PHILO-TWOPI
            ENDIF
            IF (PHIMIN.LT.0. .AND. PHIHI.GT.PI) THEN
              IF ((PHIMIN+TWOPI) .LE. PHIHI) PHICHK=.TRUE.
            ELSEIF (PHIMAX.GT.PI .AND. PHILO.LT.0.) THEN
              IF (PHIMAX .GE. (PHILO+TWOPI)) PHICHK=.TRUE.
            ELSE
              IF (PHIMIN.GE.PHILO.AND.PHIMIN.LE.PHIHI) PHICHK=.TRUE.
              IF (PHIMAX.GE.PHILO.AND.PHIMAX.LE.PHIHI) PHICHK=.TRUE.
              IF (PHIMIN.LE.PHILO.AND.PHIMAX.GE.PHIHI) PHICHK=.TRUE.
            ENDIF
            IF (PHICHK) THEN
              DO 400 SECTOR=0,5
                LENGTH = CELL_LEN(SECTOR,FDC_QUADTYPE(QUAD,HALF))
C
C Better phi check of sector
                PHILO=PHISEC-LEN_SEC(SECTOR,FDC_QUADTYPE(QUAD,HALF))
                PHIHI=PHISEC+LEN_SEC(SECTOR,FDC_QUADTYPE(QUAD,HALF))
                PHICHK=.FALSE.
                IF (PHIHI.GT.TWOPI) THEN
                  PHIHI=PHIHI-TWOPI
                  PHILO=PHILO-TWOPI
                ENDIF
                IF (PHIMIN.LT.0. .AND. PHIHI.GT.PI) THEN
                  IF ((PHIMIN+TWOPI) .LE. PHIHI) PHICHK=.TRUE.
                ELSEIF (PHIMAX.GT.PI .AND. PHILO.LT.0.) THEN
                  IF (PHIMAX .GE. (PHILO+TWOPI)) PHICHK=.TRUE.
                ELSE
                  IF (PHIMIN.GE.PHILO.AND.PHIMIN.LE.PHIHI) PHICHK=.TRUE.
                  IF (PHIMAX.GE.PHILO.AND.PHIMAX.LE.PHIHI) PHICHK=.TRUE.
                  IF (PHIMIN.LE.PHILO.AND.PHIMAX.GE.PHIHI) PHICHK=.TRUE.
                ENDIF
                IF (PHICHK) THEN
C
                  WIRE = 0
                  CALL GTL2FALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
C
                  CLD = SQRT(XC**2+YC**2)
                  ALPHA1 = MIN(ABS(LOPHI-PHISEC),ABS(HIPHI-PHISEC))
                  ALPHA2 = MAX(ABS(LOPHI-PHISEC),ABS(HIPHI-PHISEC))
C
                  IF (SECTOR.LT.3) THEN
                    IF (SECTOR.EQ.1) THEN
                      R1 = (CLD-5.74)*TAN(ALPHA1)
                      R2 = CLD*TAN(ALPHA2)
                      IF (R1.GT.LENGTH) GOTO 300
                      IF (R2.GT.LENGTH) R2 = LENGTH
                      RC = SQRT((CLD-5.74)**2 + R1**2)
                      RCD = SQRT(CLD**2 + R2**2)
                    ELSE
                      R1 = CLD*TAN(ALPHA1)
                      R2 = (CLD+5.74)*TAN(ALPHA2)
                      IF (R1.GT.LENGTH) GOTO 300
                      IF (R2.GT.LENGTH) R2 = LENGTH
                      RC = SQRT(CLD**2 + R1**2)
                      RCD = SQRT((CLD+5.74)**2 + R2**2)
                    ENDIF
                  ELSE
                    R1 = (CLD-5.33)*TAN(ALPHA1)
                    R2 = (CLD+5.33)*TAN(ALPHA2)
                    IF (R1.GT.LENGTH) GOTO 300
                    IF (R2.GT.LENGTH) R2 = LENGTH
                    RC = SQRT((CLD-5.33)**2 + R1**2)
                    RCD = SQRT((CLD+5.33)**2 + R2**2)
                  ENDIF
C
                  Z = ZC - ZVTX
                  IF (Z.EQ.0.0) Z = PRECIS
C
                  THETA1=ATAN2(RC-EXTTHE,Z)
                  THETA2=ATAN2(RCD+EXTTHE,Z)
C
                  IF (HALF.EQ.0) THEN
                    THELO=THETA2
                    THEHI=THETA1
                  ELSE
                    THELO=THETA1
                    THEHI=THETA2
                  END IF
                  THECHK=.FALSE.
                  IF (THEMIN.GE.THELO.AND.THEMIN.LE.THEHI) THECHK=.TRUE.
                  IF (THEMAX.GE.THELO.AND.THEMAX.LE.THEHI) THECHK=.TRUE.
                  IF (THEMIN.LE.THELO.AND.THEMAX.GE.THEHI) THECHK=.TRUE.
                  IF (THECHK)  ON(HALF,UNIT,QUAD,SECTOR)=.TRUE.
C
                  NUM_ON = NUM_ON + 1
                  ICRT = (CRATE_THETA(QUAD,HALF)-5)/10
                  CRTLST(ICRT) = .TRUE.
C
                END IF
  400         CONTINUE
            END IF
  300     CONTINUE
        END IF
  100 CONTINUE
C-------------------------------------------------------------------------
  999 RETURN
      END
