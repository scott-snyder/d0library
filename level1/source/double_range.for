      SUBROUTINE DOUBLE_RANGE (LINE, LENGTH, PNTR, I1, I2, J1, J2, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Searchs for and decodes a double range
C-                         defined as : (I1:I2,J1:J2).
C-
C-   Inputs  : LINE : Character string to be decoded;
C-             LENGTH : String LENGTH;
C-             PNTR : Index of the first character to be decoded.
C-
C-   Outputs : PNTR : Index of the next character to be decoded;
C-             I1,I2 : first range bounds;
C-             J1,J2 : second range bounds;
C-             ERR : Error code.
C-
C-   Controls: None.
C-
C-   Created   7-FEB-1990   Sylvain Tisserant (MSU)
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
      INTEGER       LENGTH, PNTR, I1, I2, J1, J2, ERR
C
C----------------------------------------------------------------------
C
      ERR = PARSER_SYNTAX_ERROR
      IF(PNTR.GT.LENGTH) GOTO 999
      IF(LINE(PNTR:PNTR).NE.'(') GOTO 999
      PNTR = PNTR+1
      CALL SINGLE_RANGE (LINE, LENGTH, PNTR, I1, I2, ERR)
      IF(ERR.NE.0) GOTO 999
      ERR = PARSER_SYNTAX_ERROR
      IF(PNTR.GT.LENGTH) GOTO 999
      IF(LINE(PNTR:PNTR).NE.',') GOTO 999
      PNTR = PNTR+1
      CALL SINGLE_RANGE (LINE, LENGTH, PNTR, J1, J2, ERR)
      IF(ERR.NE.0) GOTO 999
      ERR = PARSER_SYNTAX_ERROR
      IF(PNTR.GT.LENGTH) GOTO 999
      IF(LINE(PNTR:PNTR).NE.')') GOTO 999
      PNTR = PNTR+1
      ERR = PARSER_SUCCESS
  999 RETURN
      END
