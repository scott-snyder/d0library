
      SUBROUTINE PZTHETAR
C-----------------------------------------------------------------------
C
C  showing width of road in THETA
C
C
C created March 5, 1991 S. Hagopian
C
C-----------------------------------------------------------------------
      IMPLICIT NONE          
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC/LIST'
      INTEGER LPARH,NROAD
      INTEGER IP,INX,NZV
      REAL ZCH,RCH,R1,R2,RMAX,ZMAX,THETACH
      REAL Z0,Z1,Z2,ZV(5),DZV(5)
      REAL THETA1,THETA2,THEMIN,THEMAX,PHI
      CHARACTER*3 COLORS(4)
      CHARACTER*15 LABELS(4)
      LOGICAL GZPARH,FIRST,GZDGEH
      DATA ZCH,RCH/74.,90./
      DATA FIRST /.TRUE./
      DATA COLORS/'GRE','RED','CYA','YEL'/
      DATA LABELS/'MUON','ELEC','TAUS','VEES'/
C_________________________________________________________________
C GET CDC CHAMBER LIMITS
      IF(FIRST)THEN
        IF (LDGEH .LE. 0) LDGEH = GZDGEH()
        IF(LDGEH.GT.0)THEN
          ZCH = C(LDGEH + 14) 
          RCH = C(LDGEH + 12)
        ENDIF
        THETACH=ATAN(RCH/ZCH)
        FIRST=.FALSE.
      ENDIF
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
      CALL PXCOLR('GRE')
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
        Z1=Z1 +ZV(1)
        Z2=Z2+ ZV(1)
        IF(PHI.GT.PI)RMAX=-1.*RMAX
        R1=RMAX
        R2=RMAX
      ELSE
        R1=ZCH*TAN(THEMIN)
        R2=ZCH*TAN(THEMAX)
        IF(PHI.GT.PI)THEN
          R1=-1.*R1
          R2=-1.*R2
        ENDIF 
        IF(THEMIN.GT.HALFPI)THEN
          Z1=-ZMAX
          Z2=-ZMAX
          R1=-1.*R1
          R2=-1.*R2            
        ELSE
          Z1=ZMAX
          Z2=ZMAX
        ENDIF
      ENDIF 
      CALL JMOVE(Z1,R1)
  100 CALL JDRAW(Z2,R2)
      CALL JRCLOS
      CALL LEGEND_LINE(COLORS,LABELS,4)
C
  999 RETURN
      END       
