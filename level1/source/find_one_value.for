      SUBROUTINE FIND_ONE_VALUE (LINE, LENGTH, PNTR, VAL, TYPE, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decodes and returns in VAL a single value
C-                         integer or real. PNTR must point on the first
C-                         character of the string to be decoded.
C-
C-   Inputs  : LINE :   character string to be decoded;
C-             LENGTH : string length;
C-             PNTR:    index pointing on the first character to be
C-                      decoded.
C-
C-   Outputs : PNTR:  index pointing on the next character to be decoded;
C-             VAL :  searched integer if found;
C-             TYPE : 1 => INTEGER, 2 or 3 => REAL;
C-             ERR :  error code.
C-
C-   Controls: None.
C-
C-   Created  28-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek
C-                            Removed extra RETURN statements to meet D0
C-                            standards.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      CHARACTER*(*) LINE
      INTEGER       LENGTH, PNTR, TYPE, ERR
      REAL          VAL
C
      REAL          VALUE
C
      INTEGER       I, J
C
C----------------------------------------------------------------------
C
      ERR = PARSER_SYNTAX_ERROR
      IF(PNTR.GT.LENGTH) GOTO 999
      VAL = VALUE(LINE(PNTR:LENGTH),I,J,TYPE)
      IF((I.NE.1).OR.(TYPE.GT.3)) GOTO 999
      PNTR = PNTR + J
      ERR  = PARSER_SUCCESS
  999 RETURN
      END
