      SUBROUTINE LINFIT (N,X,Z,WPL,XG,ZG,SL,VXG,VSL,CHI2) 
C.        FITS A STRAIGHT LINE THROUGH POINTS IN TWO DIMENSIONS 
C.            ORIGIN = V.CHABAUD '74, STATUS = FIELD PROVEN 
C   
C         INPUT PARAMETERS  
C             N         NUMBER OF MEASURED POINTS   
C             X,Z       N MEASURED COORDINATE PAIRS 
C             WPL       N WEIGHTS   
C   
C         OUTPUT PARAMETERS 
C             XG,ZG     CENTER OF GRAVITY = ORIGIN OF FITTED LINE   
C             SL        DX/DZ = SLOPE OF FITTED LINE    
C             VXG,VSL   ERROR**2 OF XG, SL  
C             CHI2      (CHISQ/(N-2)**.5 FOR N DEGREES OF FREEDOM 
C   DH 5/88 SKIP OVER LARGE NEGATIVE VALUES  
C   DH 3/92 add check if not enough hits
C-   Updated   4-MAY-1992   M. Analyst  fix CHI2 ; DH 5-7 add sqrt back
C   MF 11/93 remove errmsg for unix/ibm
      IMPLICIT NONE
      INTEGER N,I,NN
      REAL SX,SZ,SW,SX2,SXZ,SZ2
      REAL X(N),Z(N),WPL(N) 
      REAL XG,ZG,SL,VXG,VSL,CHI2  
      INTRINSIC SQRT
C
      INCLUDE 'D0$INC:ZEBCOM.INC'   ! HTD 5/9/92
      CHARACTER*60 ERROR_MSG        ! HTD 5/9/92
C   
      IF (N.EQ.1)    GO TO 30   
      SX = 0.   
      SZ = 0.   
      SW = 0.   
      NN=0
      DO 10 I = 1,N 
      IF(ABS(Z(I)).GT.9000..OR.ABS(X(I)).GT.9000.) GO TO 10
      NN=NN+1
      SX = SX+X(I)*WPL(I)   
      SZ = SZ+Z(I)*WPL(I)   
      SW = SW+WPL(I)    
   10 CONTINUE
      IF(NN.LE.1) GO TO 30  
      VXG = 1./SW   
      XG = SX*VXG   
      ZG = SZ*VXG   
      SX2 = 0.  
      SXZ = 0.  
      SZ2 = 0.  
      DO 20 I = 1,N 
      IF(ABS(Z(I)).GT.9000..OR.ABS(X(I)).GT.9000.) GO TO 20
      SX2 = SX2+WPL(I)*(X(I)-XG)**2 
      SXZ = SXZ+WPL(I)*(X(I)-XG)*(Z(I)-ZG)  
      SZ2 = SZ2+WPL(I)*(Z(I)-ZG)**2 
   20 CONTINUE
C
      VSL = 1./SZ2  
      SL = SXZ*VSL  
      CHI2 = SX2-SL*SXZ 
      IF(CHI2.LT.0.) THEN
        CHI2=0.
      ENDIF
      IF (NN.EQ.2) THEN
        CHI2 = 0. 
        GO TO 40
      ELSE
        CHI2=SQRT(CHI2/FLOAT(NN-2))
        GO TO  40 
      END IF
C         NOT ENOUGH POINTS FOR FIT 
   30 XG = X(1) 
      ZG = Z(1) 
      VXG = 1.   
      SL = 1.   
      VSL = 100000. 
      CHI2 = 100000. 
   40 RETURN    
      END   
