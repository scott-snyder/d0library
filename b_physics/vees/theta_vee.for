      REAL FUNCTION THETA_VEE( X1, Y1, Z1, X2, Y2, Z2 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to calculate spherical theta angle
C-                         for vector with initial point
C                          (x1, y1, z1) and final point (x2, y2, z2).
C-   Returned value  :     THETA_VEE - theta angle
C-   Inputs  :             initial point (x1, y1, z1)
C                          final   point (x2, y2, z2).
C-
C-   Created  29-SEP-1991   Vladimir Burtovoy
C-   Updated   7-NOV-1991   Daria Zieminska  D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL X1,Y1,Z1,X2,Y2,Z2
      DOUBLE PRECISION R(3),RM,RR
      R(1) = X2 - X1
      R(2) = Y2 - Y1
      R(3) = Z2 - Z1
      RM   = R(1)*R(1) + R(2)*R(2) + R(3)*R(3)
      RM   = DSQRT( RM )
      IF( RM .EQ. 0.D0) THEN
         THETA_VEE = 0.0
         GO TO 999 
      ENDIF
      RR   = R(3)
      IF (RR.GT.RM) THEN
        THETA_VEE=0.0
        GO TO 999
      ENDIF  
      THETA_VEE = DACOS(RR/RM)
 999  RETURN
      END
