C DEC/CMS REPLACEMENT HISTORY, Element PTRAP.FOR
C *1     9-NOV-1988 10:08:34 HARRY "Convert polyline to polytrapezoid"
C DEC/CMS REPLACEMENT HISTORY, Element PTRAP.FOR
      SUBROUTINE PTRAP (ANGLE1,ANGLE2,THICK,Z,Y,NP,Z1,Y1,Z2,Y2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert (Z,Y) points to a polytrapezoid.
C-                         ANGLE1, ANGLE2 determine how the ends are
C-                         ``cut'' relative to the horizontal direction
C-                         (+Z). THICK is the thickness of the section.
C-                         The set of points (Z1,Y1) and (Z2,Y2) are
C-                         determined so that Z1(1) <= Z2(1).
C-
C-   Inputs  : ANGLE1      Angle of initial cross-section (in degrees)
C-             ANGLE2      Angle of final cross-section
C-             THICK       Thickness of polytrapezoid
C-             Z(*),Y(*)   Coordinates of points along polytrapezoid
C-             NP          Number of points.
C-
C-   Outputs : Z1(*),Y1(*) Points on one face of polytrapezoid
C-             Z2(*),Y2(*) Points on other face
C-   Controls: None
C-
C-   Created  10-OCT-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL         ANGLE1,ANGLE2,THICK,Z(*),Y(*),A1,A2
      REAL         Z1(*),Y1(*),Z2(*),Y2(*)
      INTEGER      I,J,K,L,M,N,NP,II,NMAX
      REAL         PI,PIBY2,DZ,DY,X,DZ1,DY1,DZ2,DY2,SMALL
      REAL         COSSUM,COSDIF,SINSUM,SINDIF,ZPRIME,YPRIME,SUM,DIF
      REAL         ZPLUS,ZMINUS,YPLUS,YMINUS,SINA,COSA
      PARAMETER( PI = 3.14159265)
      PARAMETER( PIBY2 =  PI/2.0D0 )
      PARAMETER( SMALL =  1.E-6 )
      PARAMETER( NMAX = 1000 )
      REAL    A(0:NMAX)
C----------------------------------------------------------------------
C
C ****  Compute angle of each trapezoidal section relative to +Z axis
C
      DO 210 I =  1,NP-1
        DZ = Z(I+1) - Z(I)
        DY = Y(I+1) - Y(I)
        A(I) = ATAN2 (DY,DZ)
  210 CONTINUE
      A1 = PI*ANGLE1/180.0
      A2 = PI*ANGLE2/180.0
      IF ( A1 .EQ. 0.0 ) THEN
        A(0) = PI - A(1)
      ELSE
        A(0) = 2.0*PI - A(1)
      ENDIF
      IF ( A2 .EQ. 0.0 ) THEN
        A(NP) = PI - A(NP-1)
      ELSE
        A(NP) = 2.0*PI - A(NP-1)
      ENDIF
C
C ****  Compute points of POLYTRAPEZOID
C
      DO 220 I =  1,NP
C
C ****  Compute line along which adjacent trapezoids meet
C
        DIF = 0.5*(A(I-1)-A(I))
        SUM = 0.5*(A(I-1)+A(I))
        COSDIF = COS(DIF)
        COSSUM = COS(SUM)
        SINSUM = SIN(SUM)
        ZPRIME = 0.5*THICK*ABS(SINSUM/COSDIF)
        YPRIME = 0.5*THICK*ABS(COSSUM/COSDIF)
        ZPLUS  = Z(I) + ZPRIME
        ZMINUS = Z(I) - ZPRIME
        YPLUS  = Y(I) + YPRIME
        YMINUS = Y(I) - YPRIME
C
C ****  Resolve two-fold ambiguity in solution
C
        Z1(I) = ZMINUS
        Z2(I) = ZPLUS
        IF ( A(I) .GT. PIBY2 ) THEN
          Y1(I) = YMINUS
          Y2(I) = YPLUS
        ELSE
          Y1(I) = YPLUS
          Y2(I) = YMINUS
        ENDIF
C
C ****  Check classification of points 
C
        IF ( I .GT. 1 ) THEN
          DZ1 = Z1(I) - Z1(I-1)
          DY1 = Y1(I) - Y1(I-1)
          X = SQRT(DZ1*DZ1+DY1*DY1)
          DZ1 = DZ1/X
          DY1 = DY1/X
C
          DZ2 = Z2(I) - Z2(I-1)
          DY2 = Y2(I) - Y2(I-1)
          X = SQRT(DZ2*DZ2+DY2*DY2)
          DZ2 = DZ2/X
          DY2 = DY2/X
C
          X = DZ1*DZ2 + DY1*DY2
          IF ( ABS(X-1.0) .GT. SMALL ) THEN
            X = Z2(I)
            Z2(I) = Z1(I)
            Z1(I) = X
            X = Y2(I)
            Y2(I) = Y1(I)
            Y1(I) = X
          ENDIF
        ENDIF
  220 CONTINUE
  999 RETURN
      END
