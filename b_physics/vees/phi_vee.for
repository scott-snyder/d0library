      REAL FUNCTION PHI_VEE( X1, Y1, X2, Y2 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to calculate spherical phi angle
C-                         for vector with initial point
C                          (x1, y1, z1) and final point (x2, y2, z2).
C-   Returned value  :     PHI_VEE - phi angle
C-   Inputs  :             initial point (x1, y1, z1)
C                          final   point (x2, y2, z2).
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-SEP-1991   Vladimir Burtovoy
C-   Updated   7-NOV-1991   Daria Zieminska  D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF/LIST'
      REAL X1,Y1,X2,Y2
      DOUBLE PRECISION R(2),RR,R12,PDOUBL
C
      R(1) = X2 - X1
      R(2) = Y2 - Y1
      R12  = R(1)*R(1) + R(2)*R(2)
      IF( R12 .EQ. 0.D0) THEN
        PHI_VEE = 0.0
        GO TO 999 
      ENDIF
      R12     = DSQRT( R12 )
      RR      = R(1)
      IF (RR.GT.R12) THEN
        PHI_VEE=0.0
        GO TO 999
      ENDIF  
      PDOUBL  = DACOS( RR/R12 )
      PHI_VEE = PDOUBL
      IF( R(2) .LT. 0.0 ) PHI_VEE = TWOPI - PHI_VEE
 999  RETURN
      END
