      SUBROUTINE FITLIN(X,Y,WT,N,ALFA,XG,YG,VG,VALFA,CHISQ) 
C-----------------------------------------------------------------
C
C   Straight line fit to N indpendent measurements
C   (y-YG)*cos(ALFA) = (x-XG)*sin(ALFA)
C  
C   Input: 
C             X,Y       measured coordinate pairs 
C             WT        weights=1./sigma**2   
C             N         number of points   
C   
C   Output: 
C             ALFA      arctan(dy/dx) 
C             XG,YG     center of gravity of fitted line
C             VG        variance of center of gravity
C             VALFA     variance of alfa
C             CHISQ     chi square per d.f. 
C
C   Created  MAY 1987      Daria Zieminska
C   Updated  18-MAR-1993   Ed Oltman   Count non-zero weights for DOF
C-   Updated  27-JAN-1994   Al Clark  Fix VMS-UNIX difference problem: Change 
C-                XHISQ calc from "difference of sums" to "sum of residuals".
C-                Remove unreferenced variable. 
C   
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER N,I,NMAX
      REAL Y(N),X(N),WT(N),ALFA,XG,YG,VG,VALFA,CHISQ
      PARAMETER (NMAX=100)
      REAL SX,SY,S1,SXX,SXY,SYY,SINUS,COSINU,DOF
C   
      SX  = 0.   
      SY  = 0.   
      S1  = 0.   
      SXX = 0.  
      SYY = 0.  
      SXY = 0.  
      DOF = -2.
      DO 100 I = 1,N 
        SX  = SX+X(I)*WT(I)   
        SY  = SY+Y(I)*WT(I)   
        S1  = S1+WT(I)
        IF (WT(I) .GT. 0.) DOF = DOF + 1.
  100 CONTINUE  
      XG=SX/S1
      YG=SY/S1
      VG=1./S1
      DO 200 I=1,N
        SXX = SXX+WT(I)*(X(I)-XG)**2 
        SXY = SXY+WT(I)*(Y(I)-YG)*(X(I)-XG)  
        SYY = SYY+WT(I)*(Y(I)-YG)**2 
  200 CONTINUE
      ALFA    = (ATAN2(2.*SXY,SXX-SYY))/2.
      VALFA   = 1./(SXX+SYY) 
      SINUS=SIN(ALFA)
      COSINU=COS(ALFA)
      IF (DOF .GT. 0) THEN
        CHISQ = 0.
        DO 300 I = 1,N
          CHISQ = CHISQ +
     &      WT(I)*( (X(I)-XG)*SINUS - (Y(I)-YG)*COSINU )**2
  300   CONTINUE
        CHISQ = CHISQ/DOF
      ELSE
        CHISQ=0.
      ENDIF
      RETURN
      END   
