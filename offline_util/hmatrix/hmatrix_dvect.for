      FUNCTION HMATRIX_DVECT(LINK,I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS THE VALUE OF A DOUBLE PRECISION
C-   number from a Zebra bank formatted as a double precision Vector
C-
C-   Returned value  : Double precision vector element
C-   Inputs  : LINK . Pointer to bank in /ZEBSTP
C-             I = Index into array
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-DEC-1990   Rajendran Raja
C-   Updated   7-APR-1995   Alan M. Jonckheere  Change call DGET -> DDGET
C-      to avoid conflict with new intrinsic function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER I,IND,LINK
      DOUBLE PRECISION HMATRIX_DVECT,HM
C----------------------------------------------------------------------
      IND = 2*I -1
      CALL DDGET(LINK+IND,HM)
      HMATRIX_DVECT=HM
  999 RETURN
      END
