      SUBROUTINE WORD_BOUNDED_BY (STRING,DELIMITER,I,J,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find first sub-string bounded by the delimiter
C-                         or blanks or tabs 
C_                         from the specified string, and return
C-                         the start and end position of the sub-string.
C-                         This routine is useful for splitting a string
C-                         into "words". See also SWORDS.
C-
C-   Inputs  : STRING      String to be searched
C-             DELIMITER   character to delimit string other than blank or tab
C-
C-   Outputs : I           Start of "word"
C-             J           End of "word"
C-             N           Length of "word"
C-
C-   Created  18-JAN-1994   James T. Linnemann    from WORD (H. Prosper)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      CHARACTER*1 DELIMITER
      INTEGER       I,J,N,K,LSTR
C
      CHARACTER*1 TAB,SPACE
      PARAMETER( TAB   = '	' )
      PARAMETER( SPACE = ' ' )
C----------------------------------------------------------------------
C
      I = 1
      J = 0
      N = 0
      LSTR = LEN (STRING)
C
      DO 10 K = 1,LSTR
C...find first non-delimiter character
        IF ( STRING(K:K) .NE. DELIMITER .AND.
     &       STRING(K:K) .NE. SPACE .AND.
     &       STRING(K:K) .NE. TAB ) THEN
          I = K
C...find next delimiter (try DELIMITER first)
          N = INDEX (STRING(K:LSTR),DELIMITER) - 1
          IF ( N .LE. 0 ) THEN
            N = INDEX (STRING(K:LSTR),SPACE) - 1
            IF ( N .LE. 0 ) THEN
              N = INDEX (STRING(K:LSTR),TAB) - 1
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
