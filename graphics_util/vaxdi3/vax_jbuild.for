      SUBROUTINE JBUILD(MAT,IT,P1,P2,P3,P4,P5,P6,P7)
      REAL*4 MAT(4,4),MAT2(4,4)
      COMMON/DI3PR/JUNIT
C  CONVERT THE LATEST TRANSFORMATION TO A MATRIX
      CALL JTRANS(MAT2,IT,P1,P2,P3,P4,P5,P6,P7)
C  COMBINE THE TRANSFORMATIONS
      CALL J_MATMUL(MAT,MAT2)
      END
