      SUBROUTINE L1UTIL_TRANSLATE_RS_LINE(LINE, SEND_LINE, BAD_LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translates a line from a Reference Set definition
C-     file into a line suitable to send as part of a COOR message to the TCC.
C-     This routine also flags comment lines. Moreover, it verifies that the
C-     syntax of the resulting COOR message is correct.
C-
C-   Inputs  : LINE      The input line from the reference set definition file.
C-                       Also, the translated COOR message is returned in this
C-                       string.
C-                       
C-   Outputs : SEND_LINE A flag indicating whether the resulting message should
C-                       be sent. 
C-                       .TRUE.  indicates the message should be sent. 
C-                       .FALSE. indicates the input line was a comment, and
C-                               should not be sent.
C-                               
C-             BAD_LINE  A flag indicated whether the syntax of the resulting 
C-                       message is valid:
C-                       .TRUE.  indicates a syntax error or that the resulting
C-                               message couldn't fit in input string.
C-                       .FALSE. indicates that the syntax of the message was 
C-                               correct and that the translated message can be
C-                               sent to TCC.
C-                               
C-   Controls: none
C-
C-      ENTRY L1UTIL_TRANSLATE_LTRS_LINE(LINE, SEND_LINE, BAD_LINE)
C-                         same thing for Large Tile Reference Sets. 
C-                         The final syntax checking is slightly different for
C-                         Large Tiles,  otherwise everything else is the same.
C-   
C-   Created  19-NOV-1991   Philippe Laurens, Steven Klocek
C-   Updated  17-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      - Removes leading spaces from output line
C-                      - Squeezes out multiple spaces from output line
C-                      - Converts each ASCII TAB to a single space prior to
C-                        any other conversion.
C-   Updated   6-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                          Add Entry L1UTIL_TRANSLATE_LTRS_LINE 
C-                          for Large Tile Reference Sets.
C-
C----------------------------------------------------------------------
C
C       This routine is only used with online code, but is distributed as 
C       part of an offline library. To be able to do this, we only generate 
C       useful code on VMS machines.
C&IF VAXVMS
      IMPLICIT NONE
C
      INCLUDE '(STR$ROUTINES)'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
C
      CHARACTER*512 SPACES
      EXTERNAL SPACES
C
      CHARACTER*(*) LINE
      LOGICAL*1 SEND_LINE
      LOGICAL*1 BAD_LINE
C
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      INTEGER MAX_LINE_LEN
      PARAMETER (MAX_LINE_LEN = 512)
C
      CHARACTER*1 TAB
      PARAMETER (TAB = CHAR(9))
C
      INTEGER LINE_LEN
      CHARACTER*512 WORK, THRESH_WORK
      INTEGER LINE_END
      INTEGER CHAR_POS
      INTEGER THRESH_POS, BEGIN_PAREN, END_PAREN, FIRST_DIGIT, DPOINT
      INTEGER LAST_DIGIT, LEFT_LENGTH, RIGHT_LENGTH
      INTEGER FIRST_MINUS, SECOND_MINUS, COLON
      INTEGER ETA_POS, PHI_POS
      LOGICAL OK, ERROR, MATCH
      CHARACTER*10 RS_TYPE 
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/ .TRUE. /
C
C   This is where the entry point L1UTIL_TRANSLATE_LTRS_LINE catches up
C   with the (same) code, after setting the RS_TYPE flag to 'LARGE_TILE'.
C
      RS_TYPE = 'TRIG_TOWER'
   10 CONTINUE
C
C       First, convert each ASCII TAB into a space
C
      LINE_LEN = LEN(LINE)
      DO CHAR_POS = 1, LINE_LEN
        IF (LINE(CHAR_POS:CHAR_POS) .EQ. TAB) THEN
          LINE(CHAR_POS:CHAR_POS) = ' '
        ENDIF
      END DO
C
C       If the line begins with '!' or is completely blank it is a comment 
C       and should not be sent
C
      BAD_LINE = .FALSE.
      IF ((LINE(1:1) .EQ. '!') .OR. (LINE .EQ. ' ')) THEN
        SEND_LINE = .FALSE.
        GOTO 999
      ENDIF
      SEND_LINE = .TRUE.
C
      IF (FIRST .EQV. .TRUE.) THEN
        CALL L1COOR_INIT_KEYWORD_TABLE 
        FIRST = .FALSE.
      ENDIF
C
C       The syntax is case insensitive
C
      CALL UPCASE(LINE,LINE)
C
C       Verify several properties of the syntax
C
C
C       Verify the ETA range
C
      WORK = LINE
      ETA_POS = INDEX(WORK,'TT_ETA')
      IF (ETA_POS .GT. 0) THEN
        END_PAREN = INDEX(WORK(ETA_POS:LINE_LEN),')') + ETA_POS -1
        IF (END_PAREN .LE. ETA_POS -1) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        OK = .FALSE.
        CALL L1UTIL_RS_LINE_PATTERN(WORK(ETA_POS:END_PAREN), 
     &    'TT_ETAs(sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(ETA_POS:END_PAREN), 
     &    'TT_ETAs(s-sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(ETA_POS:END_PAREN), 
     &    'TT_ETAs(s-sns:s-sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(ETA_POS:END_PAREN), 
     &    'TT_ETAs(s-sns:sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(ETA_POS:END_PAREN), 
     &    'TT_ETAs(sns:sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        IF (OK .EQV. .FALSE.) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        WORK(ETA_POS:END_PAREN) = ' '
      ENDIF
C
C       Verify the PHI range
C
      PHI_POS = INDEX(WORK,'TT_PHI')
      IF (PHI_POS .GT. 0) THEN
        END_PAREN = INDEX(WORK(PHI_POS:LINE_LEN),')') + PHI_POS -1
        IF (END_PAREN .EQ. PHI_POS -1) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        OK = .FALSE.
        CALL L1UTIL_RS_LINE_PATTERN(WORK(PHI_POS:END_PAREN), 
     &    'TT_PHIs(sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(PHI_POS:END_PAREN), 
     &    'TT_PHIs(sns:sns)', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        IF (OK .EQV. .FALSE.) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        WORK(PHI_POS:END_PAREN) = ' '
      ENDIF
C
C       Verify the Threshold definition
C
      THRESH_POS = INDEX(WORK,'THRESH_ET')
      IF (THRESH_POS .GT. 0) THEN
        END_PAREN = INDEX(WORK(THRESH_POS:LINE_LEN),'GEV')
     &    + THRESH_POS-1
        IF (END_PAREN .EQ. THRESH_POS -1) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        END_PAREN = END_PAREN+2
        OK = .FALSE.
        CALL L1UTIL_RS_LINE_PATTERN(WORK(THRESH_POS:END_PAREN),
     &    'THRESH_ETsns(sns)sGEV', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(THRESH_POS:END_PAREN),
     &    'THRESH_ETsns(sn.s)sGEV', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        CALL L1UTIL_RS_LINE_PATTERN(WORK(THRESH_POS:END_PAREN),
     &    'THRESH_ETsns(sn.ns)sGEV', MATCH)
        IF (MATCH .EQV. .TRUE.) OK = .TRUE.
C
        IF (OK .EQV. .FALSE.) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        WORK(THRESH_POS:END_PAREN) = ' '
      ENDIF
C
C       The entire line should have been erased by now
C
      IF (WORK .NE. ' ') THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
C       The threshold should be the last thing on the line
C
      IF ((THRESH_POS .LE. ETA_POS) .OR. (THRESH_POS .LE. PHI_POS)) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
C       The Threshold value in GeV must be replaced with a value in MeV
C
      LINE_END = TRULEN(LINE)
      THRESH_POS = INDEX(LINE,'THRESH_ET')
      IF (THRESH_POS .LT. 1) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
      BEGIN_PAREN = INDEX(LINE(THRESH_POS:LINE_END), '(') 
     &  + THRESH_POS - 1
      IF (BEGIN_PAREN .EQ. THRESH_POS -1) THEN
        BAD_LINE = .TRUE. 
        GOTO 999
      ENDIF
C
      END_PAREN = INDEX(LINE(THRESH_POS:LINE_END), ')') + THRESH_POS -1
      IF ((END_PAREN .EQ. THRESH_POS -1) .OR. (END_PAREN .LT.
     &  BEGIN_PAREN)) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
      FIRST_DIGIT = STR$FIND_FIRST_IN_SET(LINE(BEGIN_PAREN:END_PAREN),
     &  '0123456789') + BEGIN_PAREN - 1
      IF (FIRST_DIGIT .EQ. BEGIN_PAREN -1) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
      DPOINT = INDEX(LINE(FIRST_DIGIT:END_PAREN),'.')
     &  + FIRST_DIGIT -1
      LAST_DIGIT = STR$FIND_FIRST_NOT_IN_SET(
     &  LINE(MAX(FIRST_DIGIT,DPOINT+1):END_PAREN), '0123456789')
C
C       Has a decimal point
      IF (DPOINT .GT. FIRST_DIGIT) THEN
C
C       Has nothing after it
        IF (LAST_DIGIT .EQ. 1) THEN
          THRESH_WORK  = LINE(FIRST_DIGIT:DPOINT-1) // '000'
C
C       Has somthing after it
        ELSE
          THRESH_WORK = LINE(FIRST_DIGIT:DPOINT-1) // '000'
          LEFT_LENGTH = DPOINT - FIRST_DIGIT
          RIGHT_LENGTH = MIN(LAST_DIGIT-1,3)
          THRESH_WORK(LEFT_LENGTH+1:LEFT_LENGTH+RIGHT_LENGTH)
     &      = LINE(DPOINT+1:DPOINT+RIGHT_LENGTH)
        ENDIF
C
C       No decimal point after the number
      ELSE
        THRESH_WORK = LINE(FIRST_DIGIT:FIRST_DIGIT+LAST_DIGIT-2) 
     &    // '000'
      ENDIF
C
C       Replace the value
C
      CALL STR$REPLACE(WORK, LINE, BEGIN_PAREN+1, END_PAREN-1,
     &  THRESH_WORK(1:TRULEN(THRESH_WORK)))
C
      IF (BEGIN_PAREN + TRULEN(THRESH_WORK) + LINE_END - END_PAREN +1
     &  .GT. LINE_LEN) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
      LINE = WORK
C
C       Replace the keywords
C
      CALL L1UTIL_REPLACE_RS_KEYWORD(LINE, 'TT_ETA', 'MAGN_ETA', 
     &  WORK, OK)
      IF (OK .EQV. .FALSE.) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
      LINE = WORK
C
      CALL L1UTIL_REPLACE_RS_KEYWORD(LINE, 'TT_PHI', 'PHI', WORK, OK)
      IF (OK .EQV. .FALSE.) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
      LINE = WORK
C
      CALL L1UTIL_REPLACE_RS_KEYWORD(LINE, 'THRESH_ET', ' ', WORK, OK)
      IF (OK .EQV. .FALSE.) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
      LINE = WORK
C
      CALL L1UTIL_REPLACE_RS_KEYWORD(LINE, 'GEV', ' ', WORK, OK)
      IF (OK .EQV. .FALSE.) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
      LINE = WORK
C
C       Fix the ETA ranges if necessary
C
      IF (INDEX(LINE, 'MAGN_ETA') .EQ. 0) GOTO 2000
C
      LINE_END = TRULEN(LINE)
      FIRST_MINUS = INDEX(LINE, '-')
      IF ((FIRST_MINUS .EQ. 1) .OR. (FIRST_MINUS .EQ. LINE_END)) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
C       No negative ETA
      IF (FIRST_MINUS .EQ. 0) THEN
        WORK = 'SIGN_ETA(POS) ' // LINE(1:LINE_END)
        IF (14 + LINE_END .GT. LINE_LEN) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
        LINE = WORK
C
C       Only one value, which is negative
      ELSE
        END_PAREN = FIRST_MINUS + INDEX(LINE(FIRST_MINUS:LINE_END), ')')
     &    - 1
        IF (END_PAREN .EQ. FIRST_MINUS -1) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        COLON = FIRST_MINUS + INDEX(LINE(FIRST_MINUS:END_PAREN),':') - 1
        IF ((COLON + 1 .GT. END_PAREN) .OR. (COLON .EQ. LINE_END)) THEN
          BAD_LINE = .TRUE.
          GOTO 999
        ENDIF
C
        IF (COLON .EQ. FIRST_MINUS -1) THEN
          WORK = 'SIGN_ETA(NEG) ' 
     &      // LINE(1:FIRST_MINUS-1)
     &      // LINE(FIRST_MINUS+1:LINE_END)
          IF (13 - LINE_END .GT. LINE_LEN) THEN
            BAD_LINE = .TRUE.
            GOTO 999
          ENDIF
          LINE = WORK
C
C       Range, all negative
C
        ELSE
          SECOND_MINUS = COLON + INDEX(LINE(COLON:END_PAREN), '-') - 1
          IF ((SECOND_MINUS + 2 .GT. END_PAREN) .OR.
     &      (FIRST_MINUS + 2 .GT. COLON)) THEN
            BAD_LINE = .TRUE.
            GOTO 999
          ENDIF
          IF (SECOND_MINUS .NE. COLON -1) THEN
            WORK = 'SIGN_ETA(NEG) '
     &        // LINE(1:FIRST_MINUS-1)
     &        // LINE(SECOND_MINUS+1:END_PAREN-1)
     &        // ':'
     &        // LINE(FIRST_MINUS+1:COLON-1)
     &        // LINE(END_PAREN:LINE_END)
            IF (LINE_END + 12 .GT. LINE_LEN) THEN
              BAD_LINE = .TRUE.
              GOTO 999                   
            ENDIF
            LINE  = WORK
C
C       Range including Positive and Negative
C
          ELSE
            WORK = 'SIGN_ETA(POS) '
     &        // LINE(1:FIRST_MINUS-1)
     &        // '1:'
     &        // LINE(COLON+1:LINE_END)
     &        // ' SIGN_ETA(NEG) '
     &        // LINE(1:FIRST_MINUS-1)
     &        // '1:'
     &        // LINE(FIRST_MINUS+1:COLON-1)
     &        // LINE(END_PAREN:LINE_END)
            IF (31 + FIRST_MINUS - END_PAREN + 2*LINE_END 
     &        .GT. LINE_LEN) THEN
              BAD_LINE = .TRUE.
              GOTO 999
            ENDIF
            LINE = WORK
          ENDIF
        ENDIF
      ENDIF
C
C
C
 2000 CONTINUE
C
C       Squeeze out multiple spaces
C
      WORK = SPACES(LINE,1)
      LINE = WORK
C
C       Check the syntax of the resulting line
C
      CALL L1COOR_GET_LIST(LINE, ERROR)
      IF (ERROR .EQV. .TRUE.) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
C     Check the line syntax, 
C     L1COOR_REFSET_VERIFY is an entry point to in L1COOR_REFSET_INIT for
C     performing syntax checking without doing assignment.
C           
      IF ( RS_TYPE .EQ. 'LARGE_TILE' ) THEN
        CALL L1COOR_REFSET_VERIFY ( COOR_RSLGTILE )
      ELSE
C       EM Et, HD veto, or Tot Et syntax are identical; use any one
        CALL L1COOR_REFSET_VERIFY ( COOR_RSEMET )
      ENDIF
C
      IF (PARSE_STATUS .NE. PARSE_SUCCESS) THEN
        BAD_LINE = .TRUE.
        GOTO 999
      ENDIF
C
  999 CONTINUE
      IF (BAD_LINE .EQV. .TRUE.) THEN
        SEND_LINE = .FALSE.
      ENDIF
      IF ( RS_TYPE .EQ. 'LARGE_TILE' ) GOTO 20
      RETURN
C#######################################################################
      ENTRY L1UTIL_TRANSLATE_LTRS_LINE(LINE, SEND_LINE, BAD_LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : same as L1UTIL_TRANSLATE_RS_LINE, 
C-                         but for Large Tile Reference Sets. 
C-                         The final syntax checking is slightly different for
C-                         Large Tiles, otherwise everything else is the same.
C-                         
C-   Inputs  : LINE      The input line from the reference set definition file.
C-                       Also, the translated COOR message is returned in this
C-                       string.
C-                       
C-   Outputs : SEND_LINE A flag indicating whether the resulting message should
C-                       be sent. 
C-                       .TRUE.  indicates the message should be sent. 
C-                       .FALSE. indicates the input line was a comment, and
C-                               should not be sent.
C-                               
C-             BAD_LINE  A flag indicated whether the syntax of the resulting 
C-                       message is valid:
C-                       .TRUE.  indicates a syntax error or that the resulting
C-                               message couldn't fit in input string.
C-                       .FALSE. indicates that the syntax of the message was 
C-                               correct and that the translated message can be
C-                               sent to TCC.
C-                               
C-   Controls: none
C-
C-   Created   6-JUL-1993   Philippe Laurens - MSU L1 Trigger
C-
C----------------------------------------------------------------------
      RS_TYPE = 'LARGE_TILE'
C     Now catch up with the main code body
      GOTO 10
C     Returning from the main code body
  20  CONTINUE
      RETURN 
C
C&ELSE
C&      CHARACTER*(*) LINE
C&      LOGICAL SEND_LINE, BAD_LINE
C&      RETURN
C&ENDIF
      END
