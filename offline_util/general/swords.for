      SUBROUTINE SWORDS (STRING,I,J,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find position of first and last non-blank
C-                         and non-tab characters in specified string.
C-                         This routine is a useful for removing
C-                         trailing and leading blanks.
C-
C-   Inputs  : STRING      String to be searched
C-
C-   Outputs : I           Start of "words"
C-             J           End of "words"
C-             N           Length of "words"
C-
C-   Created   9-MAR-1988   Harrison B. Prosper
C-   Updated  20-DEC-1988   Harrison B. Prosper
C-   Updated  15-APR-1994   Harrison B. Prosper   
C-      Handle strings bounded also by nulls
C-   Updated  26-JAN-1995   sss
C-      Fix TAB parameter.  Delete a couple unused variables.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       I,J,K,N,LSTR
      CHARACTER*(*) STRING
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
      CALL WORD (STRING(1:LSTR),I,J,N)
C
C ****  If string contains non-blank characters do a back-search
C
      IF ( N .GT. 0 ) THEN
        DO 10 K = LSTR,I,-1
          IF ( STRING(K:K) .NE. SPACE .AND.
     &         STRING(K:K) .NE. NUL   .AND.
     &         STRING(K:K) .NE. TAB ) THEN
            J = K
            GOTO 20
          ENDIF
   10   CONTINUE
      ENDIF
   20 CONTINUE
      N = J-I+1
C
  999 RETURN
      END
