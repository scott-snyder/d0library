C+
      INTEGER FUNCTION SAGTUB (STATION, SECTION, TUBE,
     &                         RTUBE, VTUBE, TUBE_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS tubes geometry parameters
C-
C-   Returned value : -1     - bad description of tube
C-                    1 or 2 - returned number of tubes
C-   Inputs  : STATION number
C-             SECTION number
C-             TUBE number
C-   Outputs : RTUBE(3,2) - vectors coordinates of tubes center from
C-                           center of station
C-             VTUBE(3,2) - tubes axis vectors, |VTUBE| = 1
C-             TUBE_TYPE(2) - tubes types
C-   Controls:
C-
C-   Created  20-SEP-1990   Alexander Efimov
C-   Updated  18-OCT-1990   Alexander Efimov
C-   Updated  25-MAR-1991   Andrei Kiryunin: Change definition of layers
C-   Updated   9-APR-1991   Andrei Kiryunin: Take into account special
C-                          geometry for C stations.
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH.
C-   Updated  16-NOV-1992   Alexander Efimov: add Ailer angles for the
C-                          SAMUS stations.
C-   Updated  18-NOV-1992   Alexander Efimov: take into account crack
C-                          in U plane (2 mm).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER N_STATIONS, N_SECTIONS, N_LAYS
      PARAMETER (N_STATIONS=6, N_SECTIONS=6, N_LAYS=4)
      INTEGER STATION, SECTION, TUBE
      REAL    RTUBE(3,2),VTUBE(3,2), DTUBE(2), STUBE
      INTEGER TUBE_TYPE(2)
      INTEGER TYPE, N_TUBE, NCR, L, NL, NP, LAY, NT1, NT2
      REAL    XTB, YTB, ZTB, XCR(5), YCR(5), ABSDT, TLEN
      INTEGER LSSTH, LSSTA, LSSEC, SAGTYP, SAGTBL
      INTEGER GZSSTH, GZSSTA, GZSSEC
      INTEGER IOK, IF_SAGTUB, IFI, J
      REAL CS, STEP
      DATA CS /0.70710677/                 ! COS(PI/4)
C
      SAGTUB = -1
C
C ****  Get addresses of the banks SSTH, SSTA and SSEC
C
      IF (STATION .LT. 1 .OR. STATION .GT. N_STATIONS) GO TO 999
      IF (SECTION .LT. 1 .OR. SECTION .GT. N_SECTIONS) GO TO 999
      LSSTH = GZSSTH()
      IF (LSSTH.EQ.0) GOTO 999
      LSSTA = GZSSTA(STATION)
      IF (LSSTA.EQ.0) GOTO 999
      LSSEC = GZSSEC(STATION,SECTION)
      IF (LSSEC.EQ.0) GOTO 999
C
C ****  Get needed constants
C
      TYPE = IC(LSSEC+2)      
      N_TUBE = IC(LSSEC+4)    
      IF (TUBE .LT. 1 .OR. TUBE .GT. N_TUBE) GO TO 999
      XTB = C(LSSEC+6)            
      YTB = C(LSSEC+7)            
      ZTB = C(LSSEC+8)            
C
C ****  Define layer of this tube
C
      NL = IC(LSSEC+10)
      IF (NL.LT.1 .OR. NL.GT.N_LAYS) GOTO 999
      L=TUBE-1
      L = MOD(L,N_LAYS)
      IF (IC(LSSEC+9).GT.0) THEN
        LAY=NL+L
        IF (LAY.GT.N_LAYS) LAY=LAY-N_LAYS
      ELSE
        LAY=NL-L
        IF (LAY.LT.1) LAY=LAY+N_LAYS
      ENDIF
C
C ****  Define tube's coordinates and angles
C
      RTUBE(1,1) = XTB
      RTUBE(2,1) = YTB
      RTUBE(3,1) = ZTB + C(LSSTH+15+LAY)
      STEP = C(LSSTH+15) * IC(LSSEC+9) * (TUBE - 1)
      VTUBE(3,1) = 0.0
      IF      (TYPE .EQ. 1) THEN
        RTUBE(1,1) = RTUBE(1,1) + STEP
        VTUBE(1,1) = 0.0
        VTUBE(2,1) = 1.0
      ELSE IF (TYPE .EQ. 2) THEN
        RTUBE(2,1) = RTUBE(2,1) + STEP
        VTUBE(1,1) = 1.0
        VTUBE(2,1) = 0.0
      ELSE IF (TYPE .EQ. 3) THEN
        IF (TUBE .GT. 148) STEP = STEP + 0.2    ! crack in U plane
        RTUBE(1,1) = RTUBE(1,1) + STEP * CS
        RTUBE(2,1) = RTUBE(2,1) - STEP * CS
        VTUBE(1,1) = + CS
        VTUBE(2,1) = + CS
      ELSE
        GO TO 999
      END IF
C
C ****  Define tube type (by length)
C
      NP = IC(LSSEC+11)
      CALL SALOOP (NP, C(LSSEC+12), RTUBE(1,1), RTUBE(2,1),
     +                 VTUBE(1,1), VTUBE(2,1), NCR, XCR, YCR)
      IF (NCR .EQ. 2) THEN
        IF_SAGTUB = 1
        RTUBE(1,1) = 0.5 * (XCR(1) + XCR(2))
        RTUBE(2,1) = 0.5 * (YCR(1) + YCR(2))
        DTUBE(1) = 0.5 * SQRT ((XCR(1)-XCR(2))**2 + (YCR(1)-YCR(2))**2)
      ELSEIF (NCR .EQ. 4) THEN
        IF (STATION.NE.3.AND.STATION.NE.6) GOTO 999  
        IF_SAGTUB = 2
        RTUBE(1,1) = 0.5 * (XCR(1) + XCR(2))
        RTUBE(2,1) = 0.5 * (YCR(1) + YCR(2))
        DTUBE(1) = 0.5 * SQRT ((XCR(1)-XCR(2))**2 + (YCR(1)-YCR(2))**2)
        RTUBE(1,2) = 0.5 * (XCR(3) + XCR(4))
        RTUBE(2,2) = 0.5 * (YCR(3) + YCR(4))
        RTUBE(3,2) = RTUBE(3,1)
        DTUBE(2) = 0.5 * SQRT ((XCR(3)-XCR(4))**2 + (YCR(3)-YCR(4))**2)
        CALL UCOPY (VTUBE(1,1), VTUBE(1,2), 3)
      ELSE
        GOTO 999
      ENDIF
C
C ****  Define length and type of tube(s)
C
      NT1 = IC(LSSTH+20)
      NT2 = IC(LSSTH+23)
      TUBE_TYPE(1) = 0
      TUBE_TYPE(2) = 0
      DO IFI = 1, IF_SAGTUB
        STUBE = 1.0E+13
        DO TYPE = 1, NT1+NT2
          IF (TYPE.LE.NT1) THEN
            TLEN = C(LSSTH+21)+(TYPE-1)*C(LSSTH+22)
          ELSE
            TLEN = C(LSSTH+23+TYPE-NT1)
          ENDIF
          TLEN = TLEN * 0.5
          ABSDT=ABS(DTUBE(IFI)-TLEN)
          IF (ABSDT .LT. STUBE) THEN
            TUBE_TYPE(IFI) = TYPE
            STUBE = ABSDT
          END IF
        END DO
      END DO
C
      SAGTUB = IF_SAGTUB
  999 CONTINUE
      RETURN
      END
