      SUBROUTINE TRFITER(N,VAX,VAR,X,Y,ORIGIN,SLOPE,CHISQ,WEIGHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Chi**2 of N points
C-
C-   Inputs  : N-# of points,X - absisa,Y-points,
C-   Outputs : CHISQ,SLOPE
C-   Controls: 
C-
C-   Created  24-AUG-1991   A.Klatchko
C-   Updated   5-JAN-1992   A.Klatchko  OUT IF NOT ENOUGH HITS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,I,J,K
      REAL X(*),Y(*),CHISQ,SLOPE,X1,X2,SXSQ,SXY,SX,SX2,SY,DIFF,ORIGIN
      REAL VAR(*),WEIGHT(2,2),VAX,VARSQ
C----------------------------------------------------------------------
      IF(N .LT. 3)GOTO 999
      VARSQ = 0.0
      DO I = 1,N
        VARSQ = VARSQ + VAR(I)*VAR(I)
      ENDDO
      IF(VARSQ .EQ. 0.0)VARSQ = VAX*VAX
      SXSQ=0.0
      SXY=0.0
      SX=0.0
      SY=0.0
      SX2=0.0
      CHISQ = 0.0
      DO I=1,N
       SXSQ = SXSQ + X(I)*X(I)
       SY = SY + Y(I)
       SXY = SXY + X(I)*Y(I)
       SX = SX + X(I)
      ENDDO 
C
      X1 = (SXSQ*SY - SXY*SX)/(N*SXSQ - SX*SX)
      X2 = (N*SXY - SX*SY)/(N*SXSQ - SX*SX)
C
      SLOPE = X2
      ORIGIN=X1
      DO I =1,N
        IF(VAR(I) .NE. 0.0)THEN
          DIFF = (Y(I) - X1 - X(I)*X2)/VAR(I)
        ELSE  
          DIFF = (Y(I) - X1 - X(I)*X2)/VAX
        ENDIF  
       CHISQ = CHISQ + DIFF*DIFF 
      ENDDO
C 
      CHISQ = CHISQ/(N-1)
C
      WEIGHT(1,1) = REAL(N)/VARSQ
      WEIGHT(1,2) = SX/VARSQ
      WEIGHT(2,1) = WEIGHT(1,2)
      WEIGHT(2,2) = SXSQ/VARSQ
  999 RETURN
      END
