      SUBROUTINE WORD (STRING,I,J,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find first sub-string bounded by blanks or
C-                         tabs from the specified string, and return
C-                         the start and end position of the sub-string.
C-                         This routine is useful for splitting a string
C-                         into "words". See also SWORDS.
C-
C-   Inputs  : STRING      String to be searched
C-
C-   Outputs : I           Start of "word"
C-             J           End of "word"
C-             N           Length of "word"
C-
C-   Created   9-MAR-1988   Harrison B. Prosper
C-   Updated  15-APR-1994   Harrison B. Prosper
C-    Handle strings bounded by NULLs
C-   Updated  17-JAN-1995   sss
C-    Fix TAB parameter.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER       I,J,N,K,LSTR
C
      CHARACTER*1 TAB,SPACE,NUL
      PARAMETER( TAB   = CHAR(9) )
      PARAMETER( SPACE = ' ' )
      PARAMETER( NUL   = CHAR(0) )
C----------------------------------------------------------------------
C
      I = 1
      J = 0
      N = 0
      LSTR = LEN (STRING)
C
      DO 10 K = 1,LSTR
        IF ( STRING(K:K) .NE. SPACE .AND.
     &       STRING(K:K) .NE. NUL   .AND.
     &       STRING(K:K) .NE. TAB ) THEN
          I = K
          N = INDEX (STRING(K:LSTR),SPACE) - 1
          IF ( N .LE. 0 ) THEN
            N = INDEX (STRING(K:LSTR),TAB) - 1
            IF ( N .LE. 0 ) THEN
              N = INDEX (STRING(K:LSTR),NUL) - 1
              IF ( N .LE. 0 ) THEN
                N = LSTR - I + 1
              ENDIF
            ENDIF
          ENDIF
          J = I + N - 1
          GOTO 999
        ENDIF
   10 CONTINUE
C
  999 RETURN
      END
