      SUBROUTINE D_MATMUL(MATIN,MATOUT)
C  FORMS MATIN X MATOUT AND PUTS RESULTS INTO MATOUT
      REAL MATIN(4,4),MATOUT(4,4),MATTMP(4,4)
      DO I=1,4
        DO J=1,4
          MATTMP(I,J)=0.
        ENDDO
      ENDDO
      DO I=1,4
        DO J=1,4
          DO K=1,4
            MATTMP(I,J)=MATTMP(I,J)+MATIN(I,K)*MATOUT(K,J)
          ENDDO
        ENDDO
      ENDDO
      DO I=1,4
        DO J=1,4
          MATOUT(I,J)=MATTMP(I,J)
        ENDDO
      ENDDO
      END
