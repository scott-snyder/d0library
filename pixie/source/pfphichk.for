      FUNCTION PFPHICHK(X,Y,PHI1,PHI2,PHI3,PHI4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if the point X,Y is within the allowed 
C-   bounds of PHI. Ok if between PHI1 and PHI2, or between PHI3 and PHI4.
C-   Note: PHI1 may be < 0, and PHI4 may be > 2*pi, but PHI2 and PHI3 
C-   are assumed to be between 0 and 2*pi.
C-
C-   Returned value  :  1 if X,Y is between PHI1 and PHI2
C-                     -1 if X,Y is between PHI3 and PHI4
C-                      0 Otherwise
C-   Inputs  : X,Y,PHI1,PHI2,PHI3,PHI4
C-
C-   Created   7-NOV-1991   Robert E. Avery
C-   Updated  21-NOV-1991   Robert E. Avery  Return integer to indicate
C-                              which bounds point is within. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER PFPHICHK 
C Input:
      REAL    X,Y
      REAL    PHI1,PHI2,PHI3,PHI4
c Local:
      REAL    ATAN2
      REAL    ANGLE 
C----------------------------------------------------------------------
      PFPHICHK = 0
      IF ( (X.NE.0.0) .OR. (Y.NE.0.0)) THEN
        ANGLE = ATAN2(Y,X)
      ELSE
        ANGLE = 0.0
      ENDIF
      IF(ANGLE.GT.TWOPI) ANGLE = ANGLE - TWOPI
      IF(ANGLE.LT. 0.0) ANGLE = ANGLE + TWOPI
C
      IF(ANGLE.GE.PHI1 .AND. ANGLE.LE.PHI2) THEN
        PFPHICHK = 1
      ENDIF
      IF(ANGLE.GE.PHI3 .AND. ANGLE.LE.PHI4) THEN
        PFPHICHK = -1
      ENDIF
      IF(PHI1.LT.0.0) THEN
        IF((ANGLE-TWOPI).GE.PHI1) THEN
          PFPHICHK = 1
        ENDIF
      ENDIF
      IF(PHI4.GT.TWOPI) THEN
        IF((ANGLE+TWOPI).LE.PHI4) THEN
          PFPHICHK = -1
        ENDIF
      ENDIF
C
  999 RETURN
      END
