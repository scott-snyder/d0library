      SUBROUTINE WHAT_TO_DO (LUN, LINE_NUM,
     +                       NB_KEY, KEY_LIST, KEY_LEN, INDEX,
     +                       LINE, LENGTH, PNTR, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads a new line, then compares the key word,
C-                         beginning the line, with respect to a list. Returns
C-                         the index number of the matching string in the list.
C-                         That identifies the action to be performed. If the
C-                         key word is missing (line beginning with a '(' )
C-                         previous value of INDEX is unchanged, except if its
C-                         value was zero. In this case an error is generated.
C-                         That allows to keep the last defined action or to
C-                         force a new action definition (e.g. at the beginning
C-                         of a new SECTION).
C-
C-   Inputs  : LUN :              Logical Unit Number to be used as input;
C-             LINE_NUM :         line counter in the input file;
C-             NB_KEY :           number of entries in the list;
C-             KEY_LIST(NB_KEY) : list of expected key words;
C-             KEY_LEN(NB_KEY) :  key word lengths;
C-             INDEX :            previous action definition, or 0 to force
C-                                a new action definition.
C-
C-   Outputs : LINE :   Character string to be decoded;
C-             LENGTH : LINE length;
C-             PNTR :   Index pointing on the first character after the found
C-                      key word;
C-             INDEX :  position of the matching string in the proposed list;
C-             ERR :    error code.
C-
C-   Controls: None.
C-
C-   Created   8-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek
C-                            Removed extra RETURN statements to meet D0
C-                            standards 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      INTEGER       LUN, LINE_NUM, NB_KEY, KEY_LEN(1), INDEX
      CHARACTER*(*) KEY_LIST(1)
      CHARACTER*(*) LINE
      INTEGER       LENGTH, PNTR, ERR
C
      INTEGER   L
C
C----------------------------------------------------------------------
C
C     Reads a new line
C     ================
C
   10 CALL READ_NEW_LINE (LUN, LINE_NUM, LINE, LENGTH, ERR)
      IF(ERR.NE.0) GOTO 999
C
C     Is first character '(' ?
C     ========================
C
      ERR = PARSER_SUCCESS
      IF(LINE(1:1).EQ.'(') THEN
        IF(INDEX.EQ.0) ERR = PARSER_MISSING_ACTION
        PNTR  = 1
        GOTO 999
      ENDIF
C
C     Seeks a matching key word
C     =========================
C
      DO INDEX = 1, NB_KEY
        L = KEY_LEN(INDEX)
        IF(LENGTH.GE.L) THEN
          IF(LINE(1:L).EQ.KEY_LIST(INDEX)) THEN
            PNTR = L+1
            IF(PNTR.GT.LENGTH) GOTO 10
            GOTO 999
          ENDIF
        ENDIF
      ENDDO
      ERR = PARSER_UNKNOWN_ACTION
  999 RETURN
      END
