      SUBROUTINE READ_NEW_LINE (LUN, LINE_NUM, LINE, LENGTH, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads a new line, cleans it and returns it
C-                         if not empty.
C-
C-   Inputs  : LUN :      Logical Unit Number to be read;
C-             LINE_NUM : line counter in the input file.
C-
C-   Outputs : LINE :   the new line;
C-             LENGTH : line length;
C-             ERR :    error code.
C-
C-   Controls: None.
C-
C-   Created  28-FEB-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      INTEGER       LUN, LINE_NUM, LENGTH, ERR
      CHARACTER*(*) LINE
C
C----------------------------------------------------------------------
C
   10 READ (LUN,'(A80)',END=100) LINE
      LINE_NUM = LINE_NUM + 1
      CALL CLEAN_LINE (LINE, LENGTH)
      IF(LENGTH.EQ.0) GOTO 10
      ERR = PARSER_SUCCESS
      RETURN
C
  100 ERR = PARSER_END_OF_FILE
      RETURN
      END
