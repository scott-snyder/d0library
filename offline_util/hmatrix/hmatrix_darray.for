      FUNCTION HMATRIX_DARRAY(LINK,I,J,NIDIM,NJDIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets a Double precision array element from
C-   a Zebra bank which has A double precison matrix in it 
C-   DARRAY(NIDIM,NJDIM)
C-
C-   Returned value  : Double precision value
C-   Inputs  : LINK . Pointer to bank in /ZEBSTP
C-             I,J index of aray
C-             NIDIM,NJDIM maximum value of I and J
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
      INTEGER I,J,NIDIM,NJDIM,HMINDEX,IND,LINK
      DOUBLE PRECISION HMATRIX_DARRAY,HM
C----------------------------------------------------------------------
      IND = 2*HMINDEX(I,J,NIDIM,NJDIM)-1
      CALL DDGET(LINK+IND,HM)
      HMATRIX_DARRAY=HM
  999 RETURN
      END
