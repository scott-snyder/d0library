      SUBROUTINE J_MATMUL(MAT,MAT2)
C  FORMS MAT2 X MAT AND PUTS RESULTS INTO MAT
      REAL*4 MAT(4,4),MAT2(4,4),MAT3(4,4)
C
      DO I=1,4
        DO J=1,4
          DO K=1,4
C!!!THIS SHOULD BE (J,K)*(K,I) BUT FIX ITS CALLING ROUTINES FIRST
            MAT3(I,J)=MAT3(I,J)+MAT2(K,J)*MAT(I,K)
          ENDDO
        ENDDO
      ENDDO
C
      DO I=1,4
        DO J=1,4
          MAT(I,J)=MAT3(I,J)
          MAT3(I,J)=0.
        ENDDO
      ENDDO
      END
