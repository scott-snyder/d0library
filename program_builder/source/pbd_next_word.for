      SUBROUTINE PBD_NEXT_WORD (STRING,TOKEN,POS,LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine finds the next token for a given 
C-                         input string.  It skips the leading blanks of the
C-                         input string and saves each character until  
C-                         either a word delimiter ( blank or comment 
C-                         character '!') encounterd or the end of string.  
C-
C-   Inputs  : STRING  - Input string to search for a word
C-   Outputs : TOKEN   - Word found from the input string
C-                       Blanks if no word found
C-             POS     - Starting position of TOKEN
C-             LENGTH  - Length of TOKEN
C-                       0 if no word found
C-   Controls: 
C-
C-   Created  02-JUL-1991   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) STRING              ! INPUT CHARACTER STRING
      CHARACTER*(*) TOKEN               ! TOKEN TO BE RETURNED
      INTEGER*2     POS                 ! STARTING POSITION OF TOKEN
      INTEGER*2     LENGTH              ! TOKEN LENGTH
      INTEGER*2     TEMPLEN,I,J
      LOGICAL       FOUND
      CHARACTER*1   BLANK
      CHARACTER*1   COMMENT

      DATA BLANK /' '/
      DATA COMMENT /'!'/

      TOKEN = BLANK
      LENGTH = 0
      TEMPLEN = LEN (STRING)
C
C     Get rid of the leading blanks
C
      I = 1
      DO WHILE ( I .LE. TEMPLEN .AND. STRING(I:I) .EQ. BLANK )
        I = I + 1
      END DO

      POS = I
      J = 0
      FOUND = .FALSE.

      DO WHILE ( I .LE. TEMPLEN .AND. .NOT. FOUND )
        IF ( STRING (I:I) .EQ. BLANK .OR.
     &       STRING (I:I) .EQ. COMMENT ) THEN
          FOUND = .TRUE.
        ELSE
          J = J + 1
          TOKEN ( J:J ) = STRING(I:I)
        END IF
        I = I + 1
      END DO

      LENGTH = J

      RETURN
      END
