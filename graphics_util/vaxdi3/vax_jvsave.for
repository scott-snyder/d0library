      SUBROUTINE JVSAVE(MAT)
      INCLUDE 'D0$INC:DI3INC.INC'
      REAL*4 MAT(4,4)
      DO 10 I=1,4
        DO 20 J=1,4
          MAT(I,J)=MVIEW(I,J)
   20   CONTINUE
   10 CONTINUE
      END
