      SUBROUTINE FPLANE(NPOINTS,X,Y,Z,A,B,C,D,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the equation of a plane given
C-                         three points that are non-collinear.
C-
C-   Inputs  : NPOINTS = Number of X,Y,Z points
C-             X,Y,Z   = Locations of the points
C-   Outputs : A,B,C,D = Constants of the equation of the fitted plane
C-             IERR    = Non-zero if no plane can be fit
C-
C-   Created   6-JUN-1990   Jeffrey Bantly
C-   Updated   8-NOV-1990   Jeffrey Bantly  add array size checks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPOINTS,IERR,IPNT,I
      REAL    X(*),Y(*),Z(*)
      REAL    A,B,C,D,XY21,XY31,YZ21,YZ31
C----------------------------------------------------------------------
      IERR=0
      IF( NPOINTS.LT.3 ) THEN           ! Not enough points for a plane
        IERR=1
        GOTO 999
      ENDIF
      IF( NPOINTS.GT.3 ) THEN           ! More than three points may not
      ENDIF                             ! precisely determine a plane
C
C   Check size of array values.
C
      DO 5 I=1,3
        IF(X(I).GT.1000000.) X(I)=1000000.
        IF(Y(I).GT.1000000.) Y(I)=1000000.
        IF(Z(I).GT.1000000.) Z(I)=1000000.
        IF(X(I).LT.-1000000.) X(I)=-1000000.
        IF(Y(I).LT.-1000000.) Y(I)=-1000000.
        IF(Z(I).LT.-1000000.) Z(I)=-1000000.
    5 CONTINUE
C
C   Check for co-linearity when denominators of ratio test below will be zero
C
      IF( (Y(2)-Y(1)).EQ.0.0 ) THEN
        IF( (Y(3)-Y(1)).EQ.0.0 ) THEN
          IERR=2
          GOTO 999
        ELSEIF( ABS(Y(3)-Y(1)).LE.0.0001 ) THEN
          IERR=3
          GOTO 999
        ELSE
          GOTO 10
        ENDIF
      ENDIF
      IF( (Y(3)-Y(1)).EQ.0.0 ) THEN
        IF( ABS(Y(2)-Y(1)).LE.0.0001 ) THEN
          IERR=3
          GOTO 999
        ELSE
          GOTO 10
        ENDIF
      ENDIF
      IF( (Z(2)-Z(1)).EQ.0.0 ) THEN
        IF( (Z(3)-Z(1)).EQ.0.0 ) THEN
          IERR=2
          GOTO 999
        ELSEIF( ABS(Z(3)-Z(1)).LE.0.01 ) THEN
          IERR=4
          GOTO 999
        ELSE
          GOTO 10
        ENDIF
      ENDIF
      IF( (Z(3)-Z(1)).EQ.0.0 ) THEN
        IF( ABS(Z(2)-Z(1)).LE.0.01 ) THEN
          IERR=4
          GOTO 999
        ELSE
          GOTO 10
        ENDIF
      ENDIF
C
C  Check if points are almost co-linear but far apart in x,y, and z
C
      XY21=(X(2)-X(1))/(Y(2)-Y(1))
      XY31=(X(3)-X(1))/(Y(3)-Y(1))
      YZ21=(Y(2)-Y(1))/(Z(2)-Z(1))
      YZ31=(Y(3)-Y(1))/(Z(3)-Z(1))
      IF( (ABS(XY21-XY31)+ABS(YZ21-YZ31)) .LE. 0.002 ) THEN
        IERR=5
        GOTO 999
      ENDIF
C
   10 CONTINUE
C
C  Calculate coefficients of plane equations given the determinant below
C
C        |                 |
C        |  X   Y   Z   1  |
C        |                 |
C        |  X1  Y1  Z1  1  |
C        |                 |  =  0.
C        |  X2  Y2  Z2  1  |
C        |                 |
C        |  X3  Y3  Z3  1  |
C        |                 |
C
      A=(Y(2)*Z(3)-Y(3)*Z(2)) - (Y(1)*Z(3)-Y(3)*Z(1)) -
     &          (Y(2)*Z(1)-Y(1)*Z(2))
      B=(X(3)*Z(2)-X(2)*Z(3)) - (X(3)*Z(1)-X(1)*Z(3)) -
     &          (X(1)*Z(2)-X(2)*Z(1))
      C=(X(2)*Y(3)-X(3)*Y(2)) - (X(1)*Y(3)-X(3)*Y(1)) -
     &          (X(2)*Y(1)-X(1)*Y(2))
      D=X(1)*(Y(3)*Z(2)-Y(2)*Z(3)) + Y(1)*(X(2)*Z(3)-X(3)*Z(2)) +
     &          Z(1)*(X(3)*Y(2)-X(2)*Y(3))
C
C----------------------------------------------------------------------
  999 RETURN
      END
