      SUBROUTINE SMXINV3(A,NDIM,IFAIL)  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find the inverse of a matrix column by column 
C-                         for NDIM < 25
C-
C-   Inputs  : A matrix to be inverted
C-   Outputs : A = inverse of the original A; IFAIL =0 OK; =3 NDIM too high
C-   Controls: 
C-
C-   Created  22-JUN-1992   H.J.Martin from NUMERICAL RECIPES p.38
C-   Updated  22-JUN-1992   Daria Zieminska  enforce D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NDIM,IFAIL,INDX(100),K,IDX
      REAL A(NDIM,*),Y(625),D
C   
      IFAIL=0
      IF(NDIM.GT.25)THEN
        IFAIL=3
        GO TO 999
      END IF
      CALL VZERO(Y(1),625)
      DO 1 K=1,NDIM
        IDX=NDIM*(K-1)+K
        Y(IDX)=1.0
  1   CONTINUE
      CALL LUDCMP(A(1,1),NDIM,NDIM,INDX(1),D,IFAIL)
      DO 2 K=1,NDIM
        IDX=NDIM*(K-1)+1
        CALL LUBKSB(A(1,1),NDIM,NDIM,INDX(1),Y(IDX),IFAIL)
  2   CONTINUE
      CALL UCOPY2(Y(1),A(1,1),NDIM*NDIM)
  999 RETURN
      END
