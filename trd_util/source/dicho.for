      INTEGER FUNCTION DICHO(X,A,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search cell number in a one dimension distribution
C-                         given the value and the cell limits.
C-
C-   Returned value  : DICHO=CELL NUMBER 
C-   Inputs  :         X : Input Value 
C-                     N: Number of Cells+1
C-                     A: Cell limits.
C-   Outputs :         
C-   Controls:
C-
C-   Created  12-FEB-1996   A. Zylberstejn
C-   Modified 19-March-19996 Y.Ducros
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I,I1,I2,J,N
      REAL X,A(41)
      IF(X.LT.A(1)) THEN
        DICHO=0
        GO TO 999
      END IF
      IF(X.GT.A(N))THEN
        DICHO=N
        GO TO 999
      END IF
      IF(X.EQ.A(N))THEN
        DICHO=N-1
        GO TO 999
      END IF
      I1=1
      I2=N
      DICHO=0
      DO WHILE (I2-I1.GT.1)
        J=(I1+I2)/2
        IF(X.EQ.A(J)) THEN
          DICHO=J
          GO TO 999
        END IF
        IF(X.LT.A(J))THEN
          I2=J
        ELSE !(X.GT.A(J))
          I1=J
        END IF
      END DO
   10 CONTINUE
      DICHO=I1
  999 CONTINUE
      RETURN
      END
