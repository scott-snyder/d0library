      SUBROUTINE STRINT (SUBSTR,I,STRING,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a string out of string SUBSTR and
C-                         integer I.
C-
C-   Inputs  : 
C-             SUBSTR      Sub-string to be concatenated with integer
C-             I           Integer to be presented as a string
C-
C-   Outputs : 
C-             STRING      STRING = SUBSTR//"I"
C-             N           Length of STRING
C-
C-   Created  27-JUN-1988   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       I,J,K,L,N
      CHARACTER*(*) SUBSTR,STRING
      PARAMETER( K = 10 )
      CHARACTER*10  STR
      CHARACTER*(*) FORM
      PARAMETER( FORM = '(I10)' )
C----------------------------------------------------------------------
      WRITE(UNIT=STR,FMT=FORM) I
      DO 10 J = 1,K
        IF ( STR(J:J) .NE. ' ' ) GOTO 20
   10 CONTINUE
   20 CONTINUE
      L = LEN (SUBSTR)
      STRING = SUBSTR(1:L)//STR(J:K)
      N = L + K - J + 1
  999 RETURN
      END
