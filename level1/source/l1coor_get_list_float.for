      SUBROUTINE L1COOR_GET_LIST_FLOAT(RET_GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse a FLOAT constant from an input COOR message
C-
C-   Inputs  : none
C-   Outputs : GOOD     Success flag. .TRUE. indicates success.
C-   Controls: none
C-
C-   Created  29-JUL-1991   Zhengzhi Zhang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      CHARACTER*1 L1COOR_NEXT_CHAR
      EXTERNAL L1COOR_NEXT_CHAR
C
      LOGICAL RET_GOOD
C
      CHARACTER*1 CUR_CHAR
      INTEGER NUMBER
      INTEGER MANY_DIGITS
      LOGICAL ASSERTED
      LOGICAL GOOD
C
      RET_GOOD = .TRUE.
C
C       Mark current item as integer part of a decimal
      LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_FLOAT_INT
C
C       Replace '.' with '1' to preserve possible 0's after '.'
      number = 1
      CUR_CHAR = l1coor_next_char()

      MANY_DIGITS = 1
      DO WHILE ((CUR_CHAR .LE. '9') .AND. (CUR_CHAR .GE. '0'))
        MANY_DIGITS = MANY_DIGITS + 1
czzz          Truncate beyond 10 decimal digits
        IF (MANY_DIGITS .LT. 10)
     +    NUMBER = NUMBER * 10 + ICHAR(CUR_CHAR) - ICHAR('0')
        CUR_CHAR = L1COOR_NEXT_CHAR()
      END DO
      IF(MANY_DIGITS .GE.10)
     +   CALL INTMSG(' L1SIM> WARNING: Floating number being truncated')
C
C       scan off spaces
      DO WHILE (CUR_CHAR .EQ. ' ')
        CUR_CHAR = L1COOR_NEXT_CHAR()
      END DO
C
C       Put the number on the list
C       Duplicate the last thing on the list
      LIST_TOP = LIST_TOP + 1
      IF (LIST_TOP .GT. LIST_SIZE) THEN
        RET_GOOD = .FALSE.
        IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
          PARSE_STATUS = PARSE_FAILURE
        ENDIF
        GOTO 999
      ENDIF
      LIST(LIST_TOP,OBJ_INDEX) = LIST(LIST_TOP-1,OBJ_INDEX)
      LIST(LIST_TOP,OBJ_T_INDEX) = LIST(LIST_TOP-1,OBJ_T_INDEX)
      LIST(LIST_TOP,ITEM_INDEX) = NUMBER
      LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_FLOAT_DEC


C----------------------------------------------------------------------
  999 RETURN
      END
