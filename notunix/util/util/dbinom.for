      FUNCTION DBINOM(X,K)
C-   Updated  10-JAN-1994   R. J. Genik II  Do loop limits swtiched to
C-   avoid overflow errors. Now will only overflow if the value of binomial
C-   coefficient is too large. 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     PARAMETER (IMX = (2**31)-1)
      PARAMETER (IMX = 2147483647)
      CHARACTER NAME*(*)
      CHARACTER*80 ERRTXT

      PARAMETER (NAME = 'KBINOM')

      IF(K .LT. 0) THEN
       H=0
      ELSEIF(K .EQ. 0) THEN
       H=1
      ELSE
       H=X
       A=H
       DO 1 I = K,2,-1
       A=A-1
    1  H=H*(A/I)
      ENDIF
      DBINOM=H
      RETURN

      ENTRY KBINOM(N,K)

    3 IF(N .GE. 0) THEN
       N1=N
       IS=1
      ELSE
       N1=K-N-1
       IS=(-1)**K
      ENDIF
      IF(K .LT. 0 .OR. K .GT. N1) THEN
       IH=0
      ELSEIF(K .EQ. 0 .OR. K .EQ. N1) THEN
       IH=IS
      ELSE
       IF(K+K .LE. N1) THEN
        K1=K
       ELSE
        K1=N1-K
       ENDIF
       H=N1
       A=H
       DO 2 I = 2,K1
       A=A-1
    2  H=H*(A/I)
       IF(H .LE. IMX) THEN
        IH=IS*NINT(H)
       ELSE
        IH=0
        WRITE(ERRTXT,101) N,K
        CALL MTLPRT(NAME,'B100.1',ERRTXT)
       ENDIF
      ENDIF
      KBINOM=IH
      RETURN
  101 FORMAT('INTEGER RANGE EXCEEDED, N =',I10,',  K = ',I10)
      END
