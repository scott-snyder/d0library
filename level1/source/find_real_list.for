      SUBROUTINE FIND_REAL_LIST (LINE, LENGTH, PNTR, NB_LIST,
     +                           LIST, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for a list of reals.
C-
C-   Inputs  : LINE :    Character string to be decoded;
C-             LENGTH :  LINE length;
C-             PNTR :    Index pointing on the first character to be
C-                       decoded;
C-             NB_LIST : maximum number of expected entries.
C-
C-   Outputs : NB_LIST : number of found integers;
C-             LIST :    list of the reals;
C-             ERR :     error code.
C-
C-   Controls: None.
C-
C-   Created  28-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek
C-                            Removed extra RETURN statements to meed D0
C-                            standards. 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      CHARACTER*(*) LINE
      INTEGER       LENGTH, PNTR, NB_LIST, ERR
      REAL          LIST(1)
C
      REAL          VALUE
C
      INTEGER       MAX, I, J, TYPE
C
C----------------------------------------------------------------------
C
      ERR = PARSER_SYNTAX_ERROR
      IF(PNTR.GT.LENGTH) GOTO 999
      IF(LINE(PNTR:PNTR).NE.'(') GOTO 999
C
      MAX     = NB_LIST
      NB_LIST = 0
   10 PNTR = PNTR + 1
      IF(PNTR.GT.LENGTH) GOTO 999
      NB_LIST = NB_LIST + 1
      IF(NB_LIST.GT.MAX) THEN
        ERR = PARSER_TOO_LONG_LIST
        GOTO 999
      ENDIF
      LIST(NB_LIST) = VALUE(LINE(PNTR:LENGTH),I,J,TYPE)
      IF((I.NE.1).OR.(TYPE.GT.3)) GOTO 999
      PNTR = PNTR + J
      IF(PNTR.GT.LENGTH) GOTO 999
      IF(LINE(PNTR:PNTR).EQ.',') GOTO 10
      IF(LINE(PNTR:PNTR).EQ.')') THEN
        PNTR = PNTR + 1
        ERR  = PARSER_SUCCESS
      ENDIF
  999 RETURN
      END
