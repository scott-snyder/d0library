      SUBROUTINE MUCYL(R,DIRC,XA,IP,XH,IHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds hit poit of muon with a cylinder (cal)
C-
C-   Inputs  : R      : Radious of cylinder
C-             DIRC(3): Direction cosines of track
C-             XA(3)  : Point track originating from
C-             IP     : -1=point outside cyl. 1= point inside
C-
C-   Outputs :       XH(3)  : Hit point
C-                   IHIT   : If 0 then no hit
C-   Controls:
C-
C-   Created  28-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  R,DIRC(3),XA(3),XH(3)
      INTEGER  IP,IHIT
      REAL ALPHA,BETA,AA,BP,CC,DELTA,DIST1,DIST2
      REAL X1,X2,Y1,Y2,Z1,Z2,DOTP,EPS,RHO1,RHO2
      DATA EPS /1.E-5/
C
      IHIT = 1
C
      IF(ABS(DIRC(1)) .LT. EPS .AND. ABS(DIRC(2)) .LT. EPS ) THEN
        IHIT = 0
        GOTO 999
      ENDIF
C
      AA = DIRC(1)**2 + DIRC(2)**2
      BP = DIRC(1) * XA(1) + DIRC(2) * XA(2)
      CC = XA(1)**2 + XA(2)**2 - R**2
      DELTA = BP**2 - AA * CC
C
      IF(DELTA .LT. 0.0) THEN
        IHIT = 0
        GOTO 999
      ENDIF
      DELTA = SQRT(DELTA)
C
      RHO1 = - (BP - DELTA) / AA
      RHO2 = - (BP + DELTA) / AA
C
      X1 = XA(1) + RHO1 * DIRC(1)
      X2 = XA(1) + RHO2 * DIRC(1)
      Y1 = XA(2) + RHO1 * DIRC(2)
      Y2 = XA(2) + RHO2 * DIRC(2)
      Z1 = XA(3) + RHO1 * DIRC(3)
      Z2 = XA(3) + RHO2 * DIRC(3)
C
      DOTP = DIRC(1) * (X1 - XA(1)) +
     &       DIRC(2) * (Y1 - XA(2)) + DIRC(3) * (Z1 - XA(3))
C
      IF(IP .GT. 0) THEN
        IF(DOTP .GT. 0.0) THEN
          XH(1) = X1
          XH(2) = Y1
          XH(3) = Z1
        ELSE
          XH(1) = X2
          XH(2) = Y2
          XH(3) = Z2
        ENDIF
      ELSE
        DIST1 = SQRT( (X1 - XA(1))**2 + (Y1 - XA(2))**2 +
     &                 (Z1 - XA(3))**2 )
        DIST2 = SQRT( (X2 - XA(1))**2 + (Y2 - XA(2))**2 +
     &                 (Z2 - XA(3))**2 )
        IF(DIST1 .LT. DIST2) THEN
          XH(1) = X1
          XH(2) = Y1
          XH(3) = Z1
        ELSE
          XH(1) = X2
          XH(2) = Y2
          XH(3) = Z2
        ENDIF
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
