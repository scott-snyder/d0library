      SUBROUTINE LUBKSB(A,N,NP,INDX,B,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SOLVE SET OF N LINEAR EQNS.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-MAY-1991   AKL  - NUMERICAL RECEPES p.36
C-   Updated  27-DEC-1991   A.Klatchko  BUFFER OVERFLOW 
C-   Updated  15-JAN-1992   A.Klatchko  reduce buffer to E+10 
C-   Updated  22-JUN-1992   Daria Zieminska  change dimensions (use *)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,II,LL,N,NP,INDX(*),ERROR
      REAL A(NP,*),B(*),SUM
C----------------------------------------------------------------------
      ERROR = 0
      II=0
      DO 12 I =1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF(II .NE. 0)THEN
          DO 11 J=II,I-1
            IF(ABS(A(I,J)) .GE. 1.E+18 .OR. ABS(B(J)) .GE. 1.E+18)THEN
              ERROR = -1
              GOTO 999
            ENDIF
   11     SUM = SUM - A(I,J)*B(J)
        ELSEIF (SUM .NE. 0.0)THEN
          II=I
        ENDIF
   12 B(I) = SUM
      DO 14 I=N,1,-1
        SUM=B(I)
        DO 13 J=I+1,N
            IF(ABS(A(I,J)) .GE. 1.E+18 .OR. ABS(B(J)) .GE. 1.E+18)THEN
              ERROR = -1
              GOTO 999
            ENDIF
   13   SUM = SUM - A(I,J)*B(J)
   14 B(I) = SUM/A(I,I)    
  999 RETURN
      END
