      SUBROUTINE FIND_TOKEN(TOKEN,NTEXT,TEXT,ROW,COLUMN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the first occurrence of the specified
C-   token in the given array of strings.
C-
C-   Inputs  : TOKEN    [C*]    Token to look for
C-             NTEXT    [I]     Number of elements in text buffer
C-             TEXT(*)  [C*]    Text buffer
C-   Outputs : ROW      [I]     Position within array of strings
C-             COLUMN   [I]     Position within string
C-   Controls: None
C-
C-   Created  13-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) TOKEN
      INTEGER NTEXT
      CHARACTER*(*) TEXT(*)
      INTEGER ROW
      INTEGER COLUMN
C----------------------------------------------------------------------
      INTEGER I,ISTART,IEND,LINE
C----------------------------------------------------------------------
C
C ****  Loop of text buffer
C
      CALL SWORDS (TOKEN(1:LEN(TOKEN)),ISTART,IEND,I)
      ROW    = 0
      COLUMN = 0
      DO LINE = 1, NTEXT
        I = INDEX(TEXT(LINE),TOKEN(ISTART:IEND))
        IF ( I .GT. 0 ) THEN
          ROW    = LINE
          COLUMN = I
          GOTO 999
        ENDIF
      ENDDO
  999 RETURN
      END
