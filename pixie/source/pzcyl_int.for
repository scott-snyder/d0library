      SUBROUTINE PZCYL_INT(X0,Y0,DX,DY,RADIUS,LENGTH,
     &                    X1,Y1,Z1,X2,Y2,Z2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a track (x0,y0,dx,dy), and a closed
C-   cylinder (a can) of radius R and half length L, return the 
C-   two points of intersection with that cylinder. (Solves quadratic
C-   equation for line intersecting circle).
C-
C-   Inputs  : X0,Y0,DX,DY      Track definition
C-             RADIUS,LENGTH    Cylinder definition
C-   Outputs : X1,Y1,Z1         First point
C-             X2,Y2,Z2         Second point
C-
C-   Created   8-AUG-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Input:
      REAL    X0,Y0,DX,DY      
      REAL    RADIUS,LENGTH    
C  Output:
      REAL    X1,Y1,Z1         
      REAL    X2,Y2,Z2         
C  Local:
      REAL    B, DESC 
      REAL    R0_SQ, DR_SQ 
C----------------------------------------------------------------------
      R0_SQ = X0**2+Y0**2
      DR_SQ = DX**2+DY**2
      B = X0*DX + Y0*DY
C
      DESC = B**2 - DR_SQ * (R0_SQ - RADIUS**2)
      IF ( DESC .LE. 0  ) THEN          ! Track doesn't go through cyl.
        GO TO 900
      ENDIF
C
      Z1 = ( -B + SQRT(DESC) ) / DR_SQ
      Z1 = MIN(Z1,LENGTH)
      Z1 = MAX(Z1,-LENGTH)
C
      X1 = X0 + Z1 * DX
      Y1 = Y0 + Z1 * DY
      IF ( ABS(Z1) .EQ. LENGTH ) THEN
        IF ( (X1**2 + Y1**2) .GT. (RADIUS**2) ) THEN
          GOTO 900
        ENDIF
      ENDIF
C 
      Z2 = ( -B - SQRT(DESC) ) / DR_SQ
      Z2 = MIN(Z2,LENGTH)
      Z2 = MAX(Z2,-LENGTH)
C 
      X2 = X0 + Z2 * DX
      Y2 = Y0 + Z2 * DY
      IF ( ABS(Z2) .EQ. LENGTH ) THEN
        IF ( (X2**2 + Y2**2) .GT. (RADIUS**2) ) THEN
          GOTO 900                      ! Track is outside in Z.
        ENDIF
      ENDIF
C
      GO TO 999
 900  CONTINUE
C
C on error return 0's
C
      X1 = 0.0
      Y1 = 0.0
      Z1 = 0.0
      X2 = 0.0
      Y2 = 0.0
      Z2 = 0.0
 999  CONTINUE
      RETURN
      END
