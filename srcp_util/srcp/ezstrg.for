      SUBROUTINE EZSTRG (NAME,II,JJ,KK,STRING,NN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : From the string NAME and the triplet of
C-                         integers (II,JJ,KK) create a string with
C-                         the format:
C-
C-                         STRING = 'NAME(II:JJ) KK'.
C-
C-      If KK = 0 that argument is ignored.
C-
C-   Inputs  : NAME        Name of SRCP parameter
C-             II,JJ,KK    Indices
C-
C-   Outputs : STRING      Required string
C-             NN          Length of string
C-
C-   Controls: None
C-
C-   Example :
C-
C-      If NAME = 'BONG', and (II,JJ,KK) = (1,4,2)
C-      then
C-
C-      STRING = 'BONG(1:4) 2'
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) NAME
      CHARACTER*132 CTEMP
      INTEGER       II,JJ,KK
      CHARACTER*(*) STRING
      INTEGER       NN
C
      INTEGER I,J,K,L,N,INN(2)
      CHARACTER*132  STR
C----------------------------------------------------------------------
C
      L = LEN(NAME)
      INN(1) = II
      INN(2) = JJ
      CALL WORD (NAME(1:L),I,J,N)
      CTEMP = NAME(I:J)//'('
      CALL VNUMI(2,INN,CTEMP(1:J-I+2),':',') ',STR,L)
C
      IF ( KK .NE. 0 ) THEN
        K = LEN(STRING)
        CALL STRINT (STR(1:L),KK,STRING(1:K),NN)
      ELSE
        STRING = STR(1:L)
        NN = L
      ENDIF
C
  999 RETURN
      END
