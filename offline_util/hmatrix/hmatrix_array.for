      FUNCTION HMATRIX_ARRAY(LINK,I,J,NIDIM,NJDIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets a SINGLE precision array element from
C-   a Zebra bank which has A SINGLE precison matrix in it 
C-   ARRAY(NIDIM,NJDIM)
C-
C-   Returned value  : SINGLE precision value
C-   Inputs  : LINK . Pointer to bank in /ZEBSTP
C-             I,J index of aray
C-             NIDIM,NJDIM maximum value of I and J
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER I,J,NIDIM,NJDIM,HMINDEX,IND,LINK
      REAL HMATRIX_ARRAY
C----------------------------------------------------------------------
      IND = HMINDEX(I,J,NIDIM,NJDIM)
      HMATRIX_ARRAY = C(LINK+IND)
  999 RETURN
      END
