      SUBROUTINE UZEROD(DAR,I,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ZEROES A DOUBLE PRECISION ARRAY
C-
C-   Inputs  : DAR = DOUBLE PRECISION ARRAY
C-             I   = STARTING ELEMENT OF ARRAY
C-             N   = NUMBER OF ELEMENTS
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-DEC-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION DAR(*)
      INTEGER I,J,N
C----------------------------------------------------------------------
      DO 10 J = I , I+N-1 
        DAR(J) = 0.0
   10 CONTINUE
  999 RETURN
      END
