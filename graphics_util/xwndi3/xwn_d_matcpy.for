      SUBROUTINE D_MATCPY(MAT,MAT2)
C  COPY MAT2 INTO MAT
      REAL MAT(4,4),MAT2(4,4)
      DO I=1,4
        DO J=1,4
          MAT(I,J)=MAT2(I,J)
        ENDDO
      ENDDO
      END
