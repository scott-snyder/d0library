      SUBROUTINE FDILIN(HALF,A1,B1,C1,D1,A2,B2,C2,D2,
     &                       X1,Y1,Z1,X2,Y2,Z2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the endpoints of the line of intersection 
C-                         of two planes in an FDC Half
C-
C-   Inputs  : HALF = FDC Half where planes are drawn
C-             A1,B1,C1,D1 = Constants of equation of plane 1
C-             A2,B2,C2,D2 = Constants of equation of plane 2
C-   Outputs : X1,Y1,Z1,X2,Y2,Z2 = X,Y,Z of two points on line
C-
C-   Created   8-JUN-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER HALF
C
      REAL    A1,B1,C1,D1,A2,B2,C2,D2
      REAL    D1P,D2P,CONST1,CONST2
      REAL    X1,Y1,Z1,X2,Y2,Z2,DIR
C----------------------------------------------------------------------
      DIR=-1.
      IF(HALF.EQ.1) DIR=1.
D      IF( (A1*A2+B1*B2+C1*C2) .LT.  0.001 ) THEN
D       WRITE(*,*) ' FDILIN - PLANES ALMOST PARALLEL'
D      ENDIF
C
      Z1=100.*DIR
      D1P=C1*Z1+D1
      D2P=C2*Z1+D2
      CONST1=B2*A1-B1*A2
      IF( CONST1.EQ.0.0 ) CONST1=0.000001
      CONST2=B2*D1P-B1*D2P
      X1=-1.*CONST2/CONST1
      IF( B1.NE.0.0 ) THEN
        Y1=-1.*(A1*X1+C1*Z1+D1)/B1
      ELSEIF( B2.NE.0.0 ) THEN
        Y1=-1.*(A2*X1+C2*Z1+D2)/B2
      ELSE
        B1 = 0.00001
        Y1=-1.*(A1*X1+C1*Z1+D1)/B1
      ENDIF
C
      Z2=140.*DIR
      D1P=C1*Z2+D1
      D2P=C2*Z2+D2
      CONST1=B2*A1-B1*A2
      IF( CONST1.EQ.0.0 ) CONST1=0.000001
      CONST2=B2*D1P-B1*D2P
      X2=-1.*CONST2/CONST1
      IF( B1.NE.0.0 ) THEN
        Y2=-1.*(A1*X2+C1*Z2+D1)/B1
      ELSEIF( B2.NE.0.0 ) THEN
        Y2=-1.*(A2*X2+C2*Z2+D2)/B2
      ELSE
        B1 = 0.00001
        Y2=-1.*(A1*X2+C1*Z2+D1)/B1
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
