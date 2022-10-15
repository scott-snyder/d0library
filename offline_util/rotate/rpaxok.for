      LOGICAL FUNCTION RPAXOK(ROT,PAX)
C---------------------------------------------------------------------
C-                                                                   -
C-       Check whether rotation matrix is correctly reconstructed    -
C-                                                                   -
C-                                                                   -
C-    INPUT:                                                         -
C-    ROT  = rotation matrix, to be used according to CERN routine   -
C-            conventions new(I) = sum(J) ROT(I,J)*old(J)            -
C-                                                                   -
C-    PAX(1) = THETA = theta of the rotation axis, radians           -
C-                  range 0 to PI/2                                  -
C-    PAX(2) = PHI   = phi of the rotation axis, radians             -
C-                  range 0 to 2 PI                                  -
C-    PAX(3) = OMEGA = angle of rotation about the axis              -
C-                  positive rotation is counterclockwise, from old  -
C-                  coordinate to new coordinate                     -
C-                  range 0 to 2 PI                                  -
C-    OUTPUT:                                                        -
C-    RPAXOK = .TRUE.                                                -
C-          if the rotation matrix reconstructed from PAX matches    -
C-                  ROT within a tolerance TOL                       -
C-                                                                   -
C-                       JTL December, 1986                          -
C-                                                                   -
C---------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, j
      REAL PAX(3)
      REAL*8 ROT(3,3)
      REAL RTEST(3,3) !reconstructed rotation matrix for checking
      REAL DIFFS(3,3) !changes from original rotation matrix
      REAL DIFF !sum differences of reconstructed and starting matrix
      REAL TOL  !tolerance for reconstruction of the matrix
      REAL VASUM
      INCLUDE 'D0$INC:PI.DEF'

      DATA TOL/1.E-4/

      CALL PAXROT(PAX,RTEST)
C     CALL VSUB(RTEST,ROT,DIFFS,9)
      do i=1, 3
        do j=1,3
          DIFFS(I, j) = RTEST(I, j) - ROT(I, j)
        enddo
      enddo
C
      DIFF = VASUM(DIFFS,9)

      RPAXOK = DIFF.LT.TOL
      IF (.NOT.RPAXOK) THEN
            WRITE(6,100)ROT,PAX,RTEST,DIFF,TOL
      ENDIF
100      FORMAT(' ERROR IN ROTPAX: ROTATION MATRIX NOT RECONSTRUCTED'/
     +   ' INPUT MATRIX:'/ 3(3D15.5/),
     +   ' THETA =',E12.5, ' PHI =',E12.5,' OMEGA =',E12.5/
     +   ' RECONSTRUCTED MATRIX:'/ 3(3E15.5/),
     +   ' SUM OF ABS DIFFERENCES =',E12.5,'; TOLERANCE WAS',E12.5)

      RETURN
      END
