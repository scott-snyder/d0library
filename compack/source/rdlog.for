      SUBROUTINE RDLOG(KEYWORD,OUTSTR,LENSTR,*)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read from COMPACK command log file.
C-
C-   Inputs  : KEYWORD  [C*]  Keyword (name of routine)
C-
C-   Outputs : OUTSTR   [C*]  Output strings
C-             LENSTR   [I]   Length of strings
C-
C-   Controls: *        Error return
C-
C-   Created  30-MAR-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) KEYWORD
      CHARACTER*(*) OUTSTR
      INTEGER LENSTR,LPROMPT
      CHARACTER*(*) PROMPT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      INTEGER I,J,K,II,JJ,KK
      CHARACTER*80  REMARK,KEY
      CHARACTER*132 RECORD
C----------------------------------------------------------------------
      RECORD = ' '
      READ(UNIT=INPLUN,FMT='(A)',END=100) RECORD
      I = INDEX(RECORD,'!-<')
      IF ( I .GT. 0 ) THEN
        CALL SWORDS(RECORD(1:I-1),J,LENSTR,K)
      ELSE
        CALL SWORDS(RECORD,J,LENSTR,K)
      ENDIF
      OUTSTR = RECORD(1:LENSTR)
C
C ****  Send to screen
C
      RECORD = ' '//RECORD
      CALL INTMSG(RECORD(1:72))
C
C ****  RETURN1 if keyword found
C
      IF ( KEYWORD(1:1) .NE. ' ' ) THEN
        KEY = KEYWORD(1:LEN(KEYWORD))
        CALL SWORDS(KEY,II,JJ,KK)
        CALL UPCASE(KEY,KEY)
        CALL UPCASE(RECORD,RECORD)
C
        KEY = ' .END '//KEY(II:JJ)
        IF ( RECORD(1:KK+6) .EQ. KEY(1:KK+6) ) THEN
          GOTO 100
        ENDIF
      ENDIF
      RETURN
C
      ENTRY RDLOG_BEGIN(KEYWORD,*)
      KEY = KEYWORD(1:LEN(KEYWORD))
      READ(UNIT=INPLUN,FMT='(A)',END=100) RECORD
      CALL SWORDS(KEY,II,JJ,KK)
      CALL SWORDS(RECORD,I,J,K)
      CALL UPCASE(KEY,KEY)
      CALL UPCASE(RECORD,RECORD)
C
      RECORD = ' '//RECORD
      CALL INTMSG(RECORD(1:J+1))
C
      KEY = ' .'//KEY(II:JJ)
      IF ( RECORD(1:KK+2) .NE. KEY(1:KK+2) ) THEN
        REMARK = ' %RDLOG_BEGIN-W-SYNTAXERR, Expected '//
     &      ' keyword'//KEY(1:KK+2)//' NOT found'
        CALL INTMSG(REMARK)
        GOTO 100
      ENDIF
      RETURN
C
      ENTRY RDLOG_END(KEYWORD,*)
      KEY = KEYWORD(1:LEN(KEYWORD))
      READ(UNIT=INPLUN,FMT='(A)',END=100) RECORD
      CALL SWORDS(KEY,II,JJ,KK)
      CALL SWORDS(RECORD,I,J,K)
      CALL UPCASE(KEY,KEY)
      CALL UPCASE(RECORD,RECORD)
C
      RECORD = ' '//RECORD
      CALL INTMSG(RECORD(1:J+1))

      KEY = ' .END '//KEY(II:JJ)
      IF ( RECORD(1:KK+6) .NE. KEY(1:KK+6) ) THEN
        REMARK = ' %RDLOG_END-W-SYNTAXERR, Expected '//
     &      ' keyword'//KEY(1:KK+6)//' NOT found'
        CALL INTMSG(REMARK)
        GOTO 100
      ENDIF
      RETURN
C
C ****  Check for logging mode
C
      ENTRY WTLOG_BEGIN(KEYWORD)
      I = LEN(KEYWORD)
      RECORD = '.'//KEYWORD(1:I)
      WRITE(COMUNI,'(A)') RECORD(1:I+1)
      RETURN
C
      ENTRY WTLOG_END(KEYWORD)
      I = LEN(KEYWORD)
      RECORD = '.END '//KEYWORD(1:I)
      WRITE(COMUNI,'(A)') RECORD(1:I+5)
      RETURN
C
      ENTRY WTLOG(OUTSTR,LENSTR,PROMPT,LPROMPT)
      RECORD = OUTSTR(1:LENSTR)//' !-< '//PROMPT(1:LPROMPT)
      WRITE(COMUNI,'(A)') RECORD(1:LENSTR+LPROMPT+5)
      RETURN
C
  100 CONTINUE
      RETURN1
C
  999 RETURN
      END
