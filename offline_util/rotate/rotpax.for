      SUBROUTINE ROTPAX(ROT,PAX)
C---------------------------------------------------------------------
C-                                                                   -
C-       Calcluate the Principal Axis rotation parameters from a     -
C-       rotation matrix                                             -
C-                                                                   -
C-       Calculates parameters from antisymmetrizing the matrix      -
C-       Method described in *************?                          -
C-                                                                   -
C-    INPUT:                                                         -
C-    ROT  = rotation matrix, to be used according to CERN routine   -
C-            conventions new(I) = sum(J) ROT(I,J)*old(J)            -
C-                                                                   -
C-    OUTPUT:                                                        -
C-    PAX(1) = THETA = theta of the rotation axis, radians           -
C-                  range 0 to PI/2                                  -
C-    PAX(2) = PHI   = phi of the rotation axis, radians             -
C-                  range 0 to 2 PI                                  -
C-    PAX(3) = OMEGA = angle of rotation about the axis              -
C-                  positive rotation is counterclockwise, from old  -
C-                  coordinate to new coordinate                     -
C-                  range 0 to 2 PI                                  -
C-    ERRORS:                                                        -
C-          if the reconstructed rotation matrix fails to match      -
C-                  ROT within a tolerance TOL                       -
C-                                                                   -
C-                       JTL December, 1986                          -
C-                                                                   -
C---------------------------------------------------------------------
      IMPLICIT NONE
      REAL PAX(3)
      REAL*8 ROT(3,3)
      real *8 THETA,PHI,OMEGA
      real *8 WX,WY,WZ !unnormalized rotation vector components
      real *8 UXUX,UXUY,UYUY,UXUZ,UYUZ,UZUZ !products of unit vectors
      real *8 TRACE !trace of the rotation matrix
      real *8 SNOMEG, CSOMEG !sine, cosine of rotation angle
      real *8 SGN !sign of wz
      LOGICAL OK,RPAXOK
      INCLUDE 'D0$INC:PI.DEF'

C...extract antisymmetric part of the rotation matrix
      WZ = (ROT(2,1) - ROT(1,2))/2
      WX = (ROT(3,2) - ROT(2,3))/2
      WY = (ROT(1,3) - ROT(3,1))/2
      SNOMEG = SQRT(WX**2 + WY**2 + WZ**2)
      IF (SNOMEG.NE.0) SNOMEG = SIGN(SNOMEG,WZ)
      TRACE = ROT(1,1) + ROT(2,2) + ROT(3,3)
      CSOMEG = (TRACE - 1)/2
      OMEGA = ATAN2(SNOMEG,CSOMEG)
      IF (ABS(SNOMEG).GE.0.5*(1-CSOMEG)) THEN
       SGN = SIGN(1.0D0,WZ)
       CALL POLANG(WX*SGN,WY*SGN,WZ*SGN,THETA,PHI)
      ELSE
       IF (OMEGA.EQ.0) THEN
        THETA = 0
        PHI = 0
       ELSE
C...must base reconstruction on symmetric parts
        UXUX = (ROT(1,1) - CSOMEG)/(1 - CSOMEG)
        UYUY = (ROT(2,2) - CSOMEG)/(1 - CSOMEG)
        UZUZ = (ROT(3,3) - CSOMEG)/(1 - CSOMEG)
        UXUY = .5*(ROT(1,2) + ROT(2,1))/(1 - CSOMEG)
        UXUZ = .5*(ROT(1,3) + ROT(3,1))/(1 - CSOMEG)
        UYUZ = .5*(ROT(2,3) + ROT(3,2))/(1 - CSOMEG)
C	type *,'symmetric reconstruction attempted'
        IF (UZUZ.EQ.0) THEN
         THETA = HALFPI
         IF(ABS(UXUY).GE.1.) THEN
          PHI = SIGN(DBLE(PI/4),UXUY)
         ELSE
          PHI = ASIN(UXUY)/2
         ENDIF
        ELSE
 	 CALL POLANG(UXUZ,UYUZ,UZUZ,THETA,PHI)
        ENDIF
       ENDIF  
      ENDIF
      PAX(1) = THETA
      PAX(2) = PHI
      PAX(3) = OMEGA
C...      check validity of reconstruction
CXXXXXXXX      OK = RPAXOK(ROT,PAX)

      RETURN
      END
