      SUBROUTINE PFPHI_ROAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw phi road limit for FDC.
C-      Plot arc showing width of PHI road
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-SEP-1991   Sharon Hagopian
C       Revised March 5, 1991 to get phi from PARH bank S. Hagopian
C-   Updated  28-FEB-1992   Robert E. Avery   For FDC, only draw road if
C-      theta of road overlaps the requested theta chamber (N or S).
C-   Updated  18-MAY-1992   Robert E. Avery   Change radius to 75 cm
C-                                              (same as for CDC).
C-   Updated  14-OCT-1992   Robert E. Avery   Don't draw full (2pi) road.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER FDCROAD, HALF
      INTEGER LPARH,NROAD,IER
      INTEGER IP,INX
      INTEGER IVERSION
      INTEGER NZV
C
      REAL A1,A2,PHI1,PHI2,PHIMIN,PHIMAX
      REAL RDET,ZDET(0:1),ZCH 
      REAL ZV(5),DZV(5)
      REAL THETA1,THETA2,THEMIN,THEMAX
      REAL THETACH
C
      LOGICAL GZPARH
      LOGICAL EZERROR
C
      DATA RDET /75./
      DATA ZDET /-90.,90./
C_________________________________________________________________
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE',
     &    'PFPHI_ROAD','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC DRAW ROAD',FDCROAD)
      CALL PUGETV('FDC HALF',HALF)
      CALL EZRSET
C      
      IF(FDCROAD.LE.0) GOTO 999
C
C get road phi limits from PARH bank
      LPARH=GZPARH()
      IF(LPARH.LE.0)GO TO 999
C
C Check if is in old format, without road info
      IVERSION=IQ(LPARH+1)
      IF(IVERSION.LT.1)GO TO 999     
      NROAD=IQ(LPARH+10)
      IF(NROAD.LE.0)GO TO 999
C
C  Get z vertex. Use ZV(1); set to 0 if no primary vertex
C
      CALL ZVERTE(NZV,ZV,DZV)
      IF (NZV.EQ.0) ZV(1)=0.
      ZCH = ZDET(HALF) - ZV(1)
      IF ( ZCH.EQ.0.0 ) ZCH = 0.01
      THETACH=ATAN(RDET/ZCH)
      IF (THETACH.LT.0) THETACH = THETACH + PI
C
      CALL PUOPEN
      CALL PXCOLR('YEL')
      DO IP=1,NROAD 
        INX=LPARH+11+5*(IP-1)
C
C  Check theta limits of road:
C
        THETA1=Q(INX+2)
        THETA2=Q(INX+3)
        IF(THETA1.LT.0) THETA1 = 0
        IF(THETA2.LT.0) THETA2 = 0
        THEMIN=AMIN1(THETA1,THETA2)
        THEMAX=AMAX1(THETA1,THETA2)
        IF ( HALF.EQ.0 ) THEN
          IF ( THEMAX.LT.THETACH ) GOTO 100
        ELSE
          IF ( THEMIN.GT.THETACH ) GOTO 100
        ENDIF
C
C  Don't draw full road
C
        PHI1=Q(INX)
        PHI2=Q(INX+1)
        IF ( (PHI2-PHI1) .GE. 0.99*TWOPI ) GOTO 100
C
C  If OK, then draw:
C 
        A1=(180.*PHI1)/PI
        A2=(180.*PHI2)/PI
        CALL JARC(0.,0.,0.,RDET,0,A1,A2)
  100   CONTINUE
      ENDDO
      CALL JRCLOS
C
  999 RETURN
      END
