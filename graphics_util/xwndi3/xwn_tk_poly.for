      SUBROUTINE TK_POLY(N,P)
      REAL P(3,*)
      CALL TK_MOVE(P(1,1),P(2,1),P(3,1))
      DO I=2,N
        CALL TK_DRAW(P(1,I),P(2,I),P(3,I))
      ENDDO
      CALL TK_DRAW(P(1,1),P(2,1),P(3,1))
      END
