      SUBROUTINE PZTHETA_ROAD(IDET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw line showing width of THETA road
C-
C-   Inputs  : IDET - 1 for CDC, 2 FOR VTX, 3 FOR FDC, 0 assume CDC
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-MAR-1991   Sharon Hagopian
C-   Updated  14-OCT-1992   Robert E. Avery  Use ZVTX to determine 
C-                              roads, to reflect change in FLFSEC.FOR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE          
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER IDET
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC/LIST'
      INTEGER LPARH,NROAD,CDCROAD
      INTEGER IP,INX,NZV
      REAL ZCH,RCH,R1,R2,RMAX,ZMAX,THETACH
      REAL Z0,Z1,Z2,ZV(5),DZV(5)
      REAL THETA1,THETA2,THEMIN,THEMAX,PHI
      REAL RDET(3),ZDET(3)
      LOGICAL GZPARH,GZDGEH
      DATA RDET/74.,17.,74./
      DATA ZDET/90.,60.,90./
C_________________________________________________________________
      IF (IDET.LT.1.OR.IDET.GT.3)GO TO 999
      ZCH=ZDET(IDET)
      RCH=RDET(IDET)
      THETACH=ATAN(RCH/ZCH)
C get road THETA limits from PARH bank
      LPARH=GZPARH()
      IF(LPARH.LE.0)GO TO 999
      NROAD=IQ(LPARH+10)
      IF(NROAD.LE.0)GO TO 999
C
C  Get z vertex. Use ZV(1); set to 0 if no primary vertex
C
      CALL ZVERTE(NZV,ZV,DZV)
      IF (NZV.EQ.0) ZV(1)=0.
      CALL PUOPEN
      CALL PXCOLR('YEL')
      DO 100 IP=1,NROAD
      INX=LPARH+13+5*(IP-1)
      THETA1=Q(INX)
      THETA2=Q(INX+1)
      IF(THETA1.LT.0)THETA1=THETA1+TWOPI
      IF(THETA2.LT.0)THETA2=THETA2+TWOPI
      THEMIN=AMIN1(THETA1,THETA2)
      THEMAX=AMAX1(THETA1,THETA2)
      RMAX=RCH+5.
      ZMAX=ZCH+5.
      PHI=Q(INX-1)
      IF(PHI.LT.0)PHI=PHI+TWOPI
      IF(THETA1.GT.THETACH.AND.THETA2.LT.(PI-THETACH))THEN
        Z2=RCH/TAN(THEMIN)
        Z1=RCH/TAN(THEMAX)
        Z1=Z1 + ZV(1)
        Z2=Z2 + ZV(1)
        IF(PHI.GT.PI)RMAX=-1.*RMAX
        R1=RMAX
        R2=RMAX
      ELSE
        IF(THEMIN.GT.HALFPI)THEN
          Z1=-ZMAX
          Z2=-ZMAX
          R1=(-ZCH-ZV(1))*TAN(THEMIN)
          R2=(-ZCH-ZV(1))*TAN(THEMAX)
        ELSE
          Z1=ZMAX
          Z2=ZMAX
          R1=(ZCH-ZV(1))*TAN(THEMIN)
          R2=(ZCH-ZV(1))*TAN(THEMAX)
        ENDIF
        IF(PHI.GT.PI)THEN
          R1=-1.*R1
          R2=-1.*R2
        ENDIF 
      ENDIF 
      CALL JMOVE(Z1,R1)
  100 CALL JDRAW(Z2,R2)
      CALL JRCLOS
C
  999 RETURN
      END       
