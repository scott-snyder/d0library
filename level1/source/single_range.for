      SUBROUTINE SINGLE_RANGE (LINE, LENGTH, PNTR, I1, I2, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Searchs for and decodes a single range
C-                         defined as : 'I1:I2'
C-
C-   Inputs  : LINE : Character string to be decoded;
C-             LENGTH : String length;
C-             PNTR : Index of the first character to be decoded.
C-
C-   Outputs : PNTR : Index of the next character to be decoded;
C-             I1,I2 : range bounds;
C-             ERR : Error code.
C-
C-   Controls: None.
C-
C-   Created   7-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek (MSU)
C-                            Removed extra RETURN statements to meet D0
C-                            standards 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      CHARACTER*(*) LINE
      INTEGER       LENGTH, PNTR, I1, I2, ERR
C
      REAL          VALUE
C
      INTEGER       I, J, TYPE
C
C----------------------------------------------------------------------
C
      ERR = PARSER_SYNTAX_ERROR
      IF(PNTR.GT.LENGTH) GOTO 999
      I1 = VALUE(LINE(PNTR:LENGTH),I,J,TYPE)
      IF ((I.NE.1).OR.(TYPE.NE.1)) GOTO 999
      PNTR = PNTR+J
      IF(PNTR.GT.LENGTH) GOTO 999
      IF(LINE(PNTR:PNTR).EQ.':') THEN
        PNTR = PNTR+1
        IF(PNTR.GT.LENGTH) GOTO 999
        I2 = VALUE(LINE(PNTR:LENGTH),I,J,TYPE)
        IF ((I.NE.1).OR.(TYPE.NE.1)) GOTO 999
        PNTR = PNTR+J
      ELSE
        I2 = I1
      ENDIF
      ERR = PARSER_SUCCESS
  999 RETURN
      END
