      SUBROUTINE ROTMLT(PAX1,PAX2,PAX3)
C--------------------------------------------------------------------------
C
C     THIS SUBROUTINE MULTIPLIES TWO ROTATIONS STORED AS PRINCIPLE AXIS
C     PARAMETERS.  RESULTS ARE RETURNED AS PRINCIPLE AXIS PARAMETERS.
C
C     INPUT:  PAX1,  PAX2
C     OUTPUT: PAX3
C
C     PAXi(1) -- POLAR ANGLE OF ROTATION AXIS
C     PAXi(2) -- AZIMUTHAL ANGLE OF ROTATION AXIS
C     PAXi(3) -- ROTATION ABOUT AXIS
C
C     AUTHOR:    S KAHN          18 JUNE 1987
C
C---------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, J, K
      REAL PAX1(3), PAX2(3), PAX3(3)
      REAL*8  ROT1(3,3), ROT2(3,3), ROT3(3,3)
C
      CALL PAXROT(PAX1,ROT1)       ! convert rotation 1 to matrix
      CALL PAXROT(PAX2,ROT2)       ! convert rotation 2 to matrix
C
C     MULTIPLY ROTATION MATRICES
C
      DO 10 I = 1, 3
      DO 10 J = 1, 3
      ROT3(I,J) = 0.
      DO 10 K = 1, 3
      ROT3(I,J) = ROT3(I,J) + ROT1(I,K)*ROT2(K,J)
  10  CONTINUE
C
      CALL ROTPAX(ROT3,PAX3)
C
      RETURN
      END


