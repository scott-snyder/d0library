      SUBROUTINE D_MATUNI(MAT)
C  MAKE MAT A UNIT MATRIX
      REAL MAT(4,4)
      DO I=1,4
        DO J=1,4
          MAT(I,J)=0.
          IF(I.EQ.J) MAT(I,J)=1.
        ENDDO
      ENDDO
      END
