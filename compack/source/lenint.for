      INTEGER FUNCTION LENINT(M)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the number of digits needed to print an
C-                         integer. Useful for VAX-FORTRAN where a FORMAT
C-                         statment may use a variable field descriptor.
C-
C-   Inputs  : M: Integer to find length of.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER M,I,J,K
      INTEGER ARR(9)
      DATA ARR/10,100,1000,10000,100000,1000000,10000000,100000000,
     *           1000000000/
C----------------------------------------------------------------------
      J=1
      K=IABS(M)
      DO I=1,9
        IF((K-ARR(I)).LT.0) GOTO 100
        J=J+1
      ENDDO
  100 CONTINUE
      IF(M.LT.0) J=J+1
      LENINT=J
      RETURN
      END
