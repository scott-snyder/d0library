      SUBROUTINE MUSLIN2(SW,SX,SZ,SX2,SZ2,SXZ,XG,ZG,SL,VXG,VSL,CHI) 
C.        FITS A STRAIGHT LINE WITH INPUT BEING THE VARIOUS SUMS
C
C         INPUT : SW,SX,SZ,SX2,SZ2,SXZ -- sums of weights, x, z,
C                 x**2, z**2 x*z of points being fitted   
C         OUTPUT PARAMETERS 
C             XG,ZG     X,Y OF CENTERS OF GRAVITY
C             SL        DX/DZ = SLOPE OF FITTED LINE    
C             VXG,VSL   ERROR**2 OF XG, SL (IF WEIGHTS IN CORRECT UNITS)  
C             CHI       CHISQ SUM
C   D Hedin 12/88
      IMPLICIT NONE
      REAL SX,SZ,SW,SX2,SXZ,SZ2 
      REAL XG,ZG,SL,VXG,VSL,CHI  
        REAL SX2A,SXZA,SZ2A
C
C      IF(SW.EQ.0.) GO TO 30
      VXG = 1./SW   
      XG = SX*VXG   
      ZG = SZ*VXG   
      SX2A= SX2 - SX**2/SW      ! GET CENTERED AROUND AVERAGE
      SZ2A= SZ2 - SZ**2/SW
      SXZA= SXZ -SX*SZ/SW 
      IF(SZ2A.EQ.0.) GO TO 30
      VSL = 1./SZ2A  
      SL = SXZA*VSL  
      CHI = SX2A-SL*SXZA 
      IF(CHI.LT.0.) THEN
        CHI=0.
      ENDIF
      RETURN
C         NOT ENOUGH POINTS FOR FIT 
   30 XG = 10000. 
      VXG = 10000.   
      SL = 100000.   
      VSL = 100000. 
      CHI = -100000. 
      RETURN
      END   
