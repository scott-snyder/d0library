      FUNCTION HMATRIX_VECT(LINK,I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS THE VALUE OF A SINGLE PRECISION
C-   number from a Zebra bank formatted as a single precision Vector
C-
C-   Returned value  : Single precision vector element
C-   Inputs  : LINK . Pointer to bank in /ZEBSTP
C-             I = Index into array
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER I,LINK
      REAL HMATRIX_VECT
C----------------------------------------------------------------------
      HMATRIX_VECT = C(LINK+I)
  999 RETURN
      END
