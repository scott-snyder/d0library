      SUBROUTINE CHOP (STRING,OUTSTR,LENSTR,NSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Chop a character string into words and
C-                         return an array of sub-strings and their
C-                         lengths.
C-
C-   Inputs  : STRING      String to be chopped up
C-   Outputs : OUTSTR(*)   Array of sub-strings
C-             LENSTR(*)   Array giving lengths of sub-strings
C-             NSTR        Number of sub-strings
C-   Controls: None
C-
C-   Created  22-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LENSTR(*),NSTR,I,J,L,N,II,NN
      CHARACTER*(*) STRING,OUTSTR(*)
C----------------------------------------------------------------------
      NN = LEN (STRING)
      II = 1
      NSTR = 0  ! ZERO Item counter
C
  100 CONTINUE
C
      L = 0
      CALL WORD (STRING(II:NN),I,J,L)
      IF ( L .GT. 0 ) THEN
        NSTR = NSTR + 1
        OUTSTR(NSTR) = STRING(II+I-1:II+J-1)
        LENSTR(NSTR) = L
        II = II + J
        GOTO 100
      ELSE
        GOTO 999
      ENDIF
C
  999 RETURN
      END
