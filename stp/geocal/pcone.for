C DEC/CMS REPLACEMENT HISTORY, Element PCONE.FOR
C *1     9-NOV-1988 10:09:22 HARRY "Convert polytrapezoid to polycone"
C DEC/CMS REPLACEMENT HISTORY, Element PCONE.FOR
      SUBROUTINE PCONE (Z1,Y1,Z2,Y2,NP,Z,RMIN,RMAX,NZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Polytrapezoid to polycone
C-
C-   Inputs  : 
C-             Z1(*),Y1(*) Points on ``minimum'' side of polytrapezoid
C-             Z2(*),Y2(*) Points on ``maximum'' side of polytrapezoid
C-             NP          Number of Points
C-   Outputs : 
C-             Z(*),RMIN(*),RMAX(*)  Polycone parameters
C-             NZ          Number of Z divisions
C-   Controls: None
C-
C-   Created  26-OCT-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER      I,J,K,L,M,N,II,III,NP,NZ
      REAL      ANGLE,PI,PIBY2,Z1(*),Z2(*),Y1(*),Y2(*),DZ,DY
      REAL      Z(*),RMIN(*),RMAX(*),X,DSMALL
      PARAMETER( PI = 3.14159265 )
      PARAMETER( PIBY2 =  PI/2.0 )
      PARAMETER( DSMALL = 1.E-4 )
C----------------------------------------------------------------------
C
C ****  Compute remaing RMINs and RMAXes. scan thru points I
C
C ****  Fix ends to form closed closed path; this takes care of
C       polycone boundary conditions
C
      Z1(1) = Z2(2)
      Y1(1) = Y2(2)
      Z2(1) = Z1(2)
      Y2(1) = Y1(2)
C
      Z1(NP+2) = Z2(NP+1)
      Y1(NP+2) = Y2(NP+1)
      Z2(NP+2) = Z1(NP+1)
      Y2(NP+2) = Y1(NP+1)
C      
      J = 2
      K = 2
      I = 0
C
C ****  Get initial angle of polytrapezoid
C
      DZ = Z1(J+1) - Z1(J)
      DY = Y1(J+1) - Y1(J)
      ANGLE = ATAN2 (DY,DZ)
C
C ******************************
C ****  PROCESS FIRST POINT ****
C ******************************
C
C ****  CHECK FOR EQUAL Z-POSITIONS
C
      IF ( ABS(Z1(J)-Z2(K)) .LT. DSMALL ) THEN
        I = I + 1
        Z(I) = 0.5*(Z1(J)+Z2(K)) ! Z-position
        RMIN(I) = AMIN1(Y1(J),Y2(K))
        RMAX(I) = AMAX1(Y1(J),Y2(K))
        J = J + 1
        K = K + 1
C
C ****  CHECK FOR EQUAL Y-POSITIONS
C
      ELSEIF ( ABS(Y1(J)-Y2(K)) .LT. DSMALL ) THEN
        I = I + 1
        RMIN(I) = 0.5*(Y1(J)+Y2(K))
        RMAX(I) = RMIN(I)
C
C ****  Choose correct Z-position for first point
C
        IF ( ANGLE .LE. PIBY2 ) THEN
          Z(I) = Z1(J) ! Z-position
          J = J + 1
        ELSE
          Z(I) = Z2(K)
          K = K + 1
        ENDIF
C
      ENDIF
C
C *************************************************
C ****  CONTINUE TO SELECT POINTS ADJACENT IN Z ***
C *************************************************
  230 CONTINUE
C
      IF ( ((K-1).GT.NP) .AND. ((J-1).GT.NP) ) GOTO 290
C
C ****  SELECT NEXT POINT
C
      IF ( ABS(Z(I)-Z1(J)) .LT. ABS(Z(I)-Z2(K)) ) THEN
C
C ****  Z1(J) is next point; find the two K-points on either side
C       of point J and compute intersect of vertical line from this
C       point to line joining the two K-points.
C
        I = I + 1
        Z(I) = Z1(J)
        X = (Y2(K)-Y2(K-1))/(Z2(K)-Z2(K-1)) ! Gradient of k-line
        X = X*(Z(I)-Z2(K)) + Y2(K)
        RMIN(I) = AMIN1(X,Y1(J))
        RMAX(I) = AMAX1(X,Y1(J))
        J = J + 1
C
      ELSEIF ( ABS(Z(I)-Z2(K)) .LT. ABS(Z(I)-Z1(J)) ) THEN
C
C ****  Z2(K) is next point
C
        I = I + 1
        Z(I) = Z2(K)
        X = (Y1(J)-Y1(J-1))/(Z1(J)-Z1(J-1))
        X = X*(Z(I)-Z1(J)) + Y1(J)
        RMIN(I) = AMIN1(X,Y2(K))
        RMAX(I) = AMAX1(X,Y2(K))
        K = K + 1
C
      ELSE
C
C ****  Points have same Z coordinate.
C
C ****  CHECK FOR EQUAL Y-POSITIONS
C
        IF ( ABS(Y1(J)-Y2(K)) .LT. DSMALL ) THEN
C
C ****  Points are COINCIDENT
C
          I = I + 1
          Z(I) = 0.5*(Z1(J)+Z2(K)) ! Z-position
          RMIN(I) = 0.5*(Y1(J)+Y2(K))
          RMAX(I) = RMIN(I)
          J = J + 1
          K = K + 1
        ELSE
C
C ****  Points are VERTICAL
C
          I = I + 1
          Z(I) = 0.5*(Z1(J)+Z2(K)) ! Z-position
          RMIN(I) = AMIN1(Y1(J),Y2(K))
          RMAX(I) = AMAX1(Y1(J),Y2(K))
          J = J + 1
          K = K + 1
        ENDIF
      ENDIF
      GOTO 230
  290 CONTINUE
C
C ****  Return number of Z-divisions
C
      NZ = I
  999 RETURN
      END
