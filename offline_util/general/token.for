      SUBROUTINE TOKEN(STRING,DLIMIT,I,J,N)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Finds first sub-string bounded by the character
C                         given as a delimiter from specified string, and
C                         returns start and end position of the sub-string.
C                         Useful for splitting a string into "words".
C
C   Inputs  : STRING  C*(*) String to be searched
C             DLIMIT  C*1   Character used to delimit tokens (sub-strings)
C
C   Outputs : I           Start of token
C             J           End of token
C             N           Length of token
C
C
C   Created  15-MAR-1989   K. Wyatt Merritt   Adaptation of WORD.FOR,
C                               created by Harrison Prosper
C-   Updated  29-MAR-1990   Harrison B. Prosper  
C-      Cleaned up a little
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*1 DLIMIT
      CHARACTER*(*) STRING
      INTEGER       I,J,K,L,N,LSTR
C----------------------------------------------------------------------
C
      I = 1
      J = 0
      N = 0
      LSTR = LEN (STRING)
C
      DO 10 K = 1,LSTR
        IF ( STRING(K:K) .NE. DLIMIT ) THEN
          I = K
          N = INDEX (STRING(K:LSTR),DLIMIT) - 1
          IF ( N .LE. 0 ) THEN
            N = LSTR - I + 1
          ENDIF
          J = I + N - 1
          GOTO 999
        ENDIF
   10 CONTINUE
  999 RETURN
      END
