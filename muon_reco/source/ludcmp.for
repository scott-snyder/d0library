      SUBROUTINE LUDCMP(A,N,NP,INDX,D,IFAIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :LU decomposition 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-MAY-1991   AKL after NUMERICAL RECIPES p.35
C-   Updated  22-JUN-1992   Daria Zieminska  change dimensions (use *)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,NP,INDX(*),NMAX,I,J,K,IMAX,IFAIL
      PARAMETER (NMAX=100)
      REAL A(NP,*),VV(NMAX),TINY,D,AAMAX,SUM,DUM
      PARAMETER (TINY=1.0E-20)
C----------------------------------------------------------------------
      IFAIL=0
      D=1.
      DO 12 I=1,N
         AAMAX=0.0
         DO 11 J=1,N
   11    IF (ABS(A(I,J)).GT.AAMAX)AAMAX=ABS(A(I,J))
         IF(AAMAX.EQ.0)THEN
C           CALL ERRMSG('MATRIX IS SINGULAR','LUDCMP',
C     &        'TRY LOWER ORDER','W') 
           IFAIL = -1
           GOTO 999  
         ENDIF  
   12 VV(I)=1./AAMAX
C
      DO 19 J=1,N
        DO 14 I=1,J-1
          SUM=A(I,J)
          DO 13 K=1,I-1
   13     SUM=SUM-A(I,K)*A(K,J)
   14   A(I,J)=SUM
        AAMAX=0.0
        DO 16 I=J,N
          SUM=A(I,J)
          DO 15 K=1,J-1
   15     SUM=SUM-A(I,K)*A(K,J) 
          A(I,J)=SUM
          DUM=VV(I)*ABS(SUM)
          IF (DUM .GE. AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
   16   CONTINUE
        IF (J .NE. IMAX) THEN
          DO 17 K=1,N
            DUM = A(IMAX,K)
            A(IMAX,K)=A(J,K)
   17     A(J,K)=DUM
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J) = IMAX
        IF (A(J,J) .EQ. 0.0)A(J,J)=TINY
        IF (J .NE. N) THEN
          DUM = 1./A(J,J)
          DO 18 I = J+1,N
   18     A(I,J) = A(I,J)*DUM 
        ENDIF
   19 CONTINUE    
  999 RETURN
      END
