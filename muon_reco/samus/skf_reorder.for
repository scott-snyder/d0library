      SUBROUTINE SKF_REORDER(a,N1,N2,index)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Reorders array A according to index array.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER N1,N2
      REAL    A(N1,N2)
      INTEGER INDEX(N2),IND

      INTEGER I,J
      INTEGER MAXN,MAXM
      PARAMETER (MAXN=200, MAXM=3)
      REAL    TMP(MAXM,MAXN)
C<<
      DO I=1,N2
        IND = INDEX(I)
        DO J=1,N1
          TMP(J,I) = A(J,IND)
        END DO
      END DO
C<<
      DO I=1,N2
        DO J=1,N1
          A(J,I) = TMP(J,I)
        END DO
      END DO
C<<
  999 RETURN
      END
