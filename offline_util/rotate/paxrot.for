      SUBROUTINE PAXROT(PAX,ROT)
C---------------------------------------------------------------------
C-                                                                   -
C-       Calcluate the rotation matrix from Principal Axis           -
C-       rotation parameters                                         -
C-                                                                   -
C-       Calculates parameters from antisymmetrizing the matrix      -
C-       Method described in *************?                          -
C-                                                                   -
C-    INPUT:                                                         -
C-    PAX(1) = THETA = theta of the rotation axis, radians           -
C-    PAX(2) = PHI   = phi of the rotation axis, radians             -
C-    PAX(3) = OMEGA = angle of rotation about the axis              -
C-                  positive rotation is counterclockwise, from old  -
C-                  coordinate to new coordinate                     -
C-                                                                   -
C-    OUTPUT:                                                        -
C-    ROT  = rotation matrix, to be used according to CERN routine   -
C-            conventions new(I) = sum(J) ROT(I,J)*old(J)            -
C-                                                                   -
C-                       JTL December, 1986                          -
C-                                                                   -
C---------------------------------------------------------------------
      IMPLICIT NONE
      REAL PAX(3)
      REAL*8  ROT(3,3)
      real *8 THETA,PHI,OMEGA
      real *8 UX,UY,UZ !normalized unit vector along rotation axis
      real *8 SNTHET,CSTHET !sine, cosine of  THETA
      real *8 SNOMEG, CSOMEG !sine, cosine of rotation angle
      INCLUDE 'D0$INC:PI.DEF/NOLIST'

      THETA = PAX(1)
      PHI = PAX(2)
      OMEGA = PAX(3)
      UZ = COS(THETA)
      SNTHET = SIN(THETA)
      UX = SNTHET*COS(PHI)
      UY = SNTHET*SIN(PHI)
      CSOMEG = COS(OMEGA)
      SNOMEG = SIN(OMEGA)

      
      ROT(1,1) = CSOMEG + (1-CSOMEG)*(UX**2)
      ROT(2,2) = CSOMEG + (1-CSOMEG)*(UY**2)
      ROT(3,3) = CSOMEG + (1-CSOMEG)*(UZ**2)

      ROT(1,2) =  -SNOMEG*UZ + (1-CSOMEG)*UX*UY
      ROT(1,3) =   SNOMEG*UY + (1-CSOMEG)*UX*UZ
      ROT(2,3) =  -SNOMEG*UX + (1-CSOMEG)*UY*UZ
 
      ROT(2,1) =   SNOMEG*UZ + (1-CSOMEG)*UX*UY
      ROT(3,1) =  -SNOMEG*UY + (1-CSOMEG)*UX*UZ
      ROT(3,2) =   SNOMEG*UX + (1-CSOMEG)*UY*UZ

      RETURN
      END
