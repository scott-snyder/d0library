      SUBROUTINE DFTLIN(X,Y,WT,N,PHI,XG,YG,ERRD,ERRPHI,CHISQ) 
C-----------------------------------------------------------------
C-
C-  Purpose and Methods: Straight line fit to N independent measurements
C-                       (y-YG)*cos(PHI) = (x-XG)*sin(PHI)
C-  
C-   Input: 
C-             X,Y       measured coordinate pairs 
C-             WT        weights=1./sigma**2   
C-             N         number of points   
C-   
C-   Output: 
C-             PHI       arctan(dy/dx) 
C-             XG,YG     center of gravity of fitted line
C-             ERRD      variance of center of gravity
C-             ERRPHI    variance of phi
C-             CHISQ     chi square 
C-
C-   Created  6-Nov-1989  joey thompson: Based on FITLIN
C-   
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,I
      REAL Y(N),X(N),WT(N),PHI,XG,YG,ERRD,ERRPHI,CHISQ
      REAL SX,SY,S1,SXX,SXY,SYY,VAR,SINUS,COSINU
C   
      SX  = 0.   
      SY  = 0.   
      S1  = 0.   
      SXX = 0.  
      SYY = 0.  
      SXY = 0.  
      DO 100 I = 1,N 
        SX  = SX+X(I)*WT(I)   
        SY  = SY+Y(I)*WT(I)   
        S1  = S1+WT(I)    
  100 CONTINUE  
      XG=SX/S1
      YG=SY/S1
      ERRD=1./SQRT(S1)
      DO 200 I=1,N
        SXX = SXX+WT(I)*(X(I)-XG)**2 
        SXY = SXY+WT(I)*(Y(I)-YG)*(X(I)-XG)  
        SYY = SYY+WT(I)*(Y(I)-YG)**2 
  200 CONTINUE
      PHI     = (ATAN2(2.*SXY,SXX-SYY))/2.
      ERRPHI   = 1./SQRT(SXX+SYY) 
      SINUS=SIN(PHI)
      COSINU=COS(PHI)
      CHISQ=SXX*SINUS**2-2.*SXY*SINUS*COSINU+SYY*COSINU**2
  999 RETURN
      END   
