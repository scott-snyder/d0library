      SUBROUTINE PARSE (STRING,DLIMIT,OUTSTR,LENSTR,NSTR)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Chop a character string into sub-strings using
C                         a specified delimiter character and return
C                         an array of sub-strings and their lengths.
C
C   Inputs  : STRING   C*(*) String to be chopped up
C             DLIMIT   C*1   Character delimiting the sub-strings
C   Outputs : OUTSTR   C*(*) Array of sub-strings
C             LENSTR   I*(*) Array giving lengths of sub-strings
C             NSTR     I     Number of sub-strings
C   Controls: None
C
C   Created  22-NOV-1988   Harrison B. Prosper
C   Updated  15-MAR-1989   K. Wyatt Merritt   to use arbitrary delimiter
C                                              character
C-   Updated  23-MAR-1989   Harrison B. Prosper   
C                           Generalized to work on sub-strings
C                           containing spaces by replacing WORD
C                           with SWORDS
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LENSTR(*),NSTR,I,J,L,N,II,NN,LN
C
      CHARACTER*1 DLIMIT
      CHARACTER*(*) STRING,OUTSTR(*)
C----------------------------------------------------------------------
C
      NN = LEN (STRING)
      II = 1
      NSTR = 0  ! ZERO Item counter
C
C      Strip off blanks
C
      CALL SWORDS (STRING(II:NN),I,J,LN)
      II = I
      LN = J
C
  100 CONTINUE
      L = 0
C
C      Get next delimited token
C
      CALL TOKEN(STRING(II:LN),DLIMIT,I,J,L)
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
