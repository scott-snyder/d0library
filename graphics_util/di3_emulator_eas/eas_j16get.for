      SUBROUTINE J16GET(ICODE, MODMX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To return the current value of a specific
C-                         4-by-4 transformatio matrix.
C-
C-   Inputs  : ICODE
C-                 1- Current viewing transformation.
C-                 2- Inverse viewing transformation.
C-                 3- Current modeling transformation.
C-   Outputs : MODMX(4,4)
C-                 The current value of the matrix.
C-
C-
C-   Created   8-NOV-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICODE
      REAL MODMX(4,4)
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INTEGER I,J
C
      GOTO ( 1,  2,  3), ICODE
C
    1 CONTINUE
      GOTO 999
C
    2 CONTINUE
      GOTO 999
C
    3 CONTINUE
      DO 10 I=1,4
         DO 10 J=1,4
            MODMX(I,J) = MODMAT(I,J)
   10 CONTINUE
      GOTO 999
C
  999 RETURN
      END
