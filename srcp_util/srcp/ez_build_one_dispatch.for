      SUBROUTINE EZ_BUILD_ONE_DISPATCH(LUN,RCP_BANK,ROUTINE,MENU,TITLE,
     &  DAY,AUTHOR,PREFIX,COMMAND,ACTION,NCOMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write shell for dispatch routine.
C-
C-   Inputs  : LUN      [I]     Output unit number
C-             RCP_BANK [C*]    RCP-bank containing templates
C-             ROUTINE  [C*]    Routine Name
C-             MENU     [C*]    Menu name
C-             TITLE    [C*]    Menu title
C-             DAY      [C*]    Creation date
C-             AUTHOR   [C*]    Author's name
C-             PREFIX   [C*]    Prefix to command routine names
C-             COMMAND(*)[C*]   List of commands
C-             ACTION(*) [C*]   List of action names
C-             NCOMMAND  [I]    Number of commands
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-JUN-1991   Harrison B. Prosper
C-   Updated   9-DEC-1991   Krzysztof L. Genser  
C-      record length changed 80-->132 
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      Fixed for UNIX.  Replace OPEN with D0OPEN.  Elimate concat. in arg.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
      CHARACTER*(*) ROUTINE
      CHARACTER*(*) RCP_BANK
      CHARACTER*(*) MENU
      CHARACTER*(*) TITLE
      CHARACTER*(*) DAY,AUTHOR
      CHARACTER*(*) PREFIX,COMMAND(*),ACTION(*)
      INTEGER NCOMMAND
C----------------------------------------------------------------------
      INTEGER I,J,K,L,II
      INTEGER LR,LC,LM,LT,LA,LP,LTAB
      INTEGER ISTART,IEND,IER,NLINES,NLINES_A,NLINES_D,ROW,COLUMN
C
      INTEGER MAXLINE
      PARAMETER( MAXLINE = 50 )
      INTEGER LLINE(MAXLINE)
      CHARACTER*132 TEMPLATE_A(MAXLINE),TEMPLATE_D(MAXLINE),
     &  LINE(MAXLINE)
      CHARACTER*132 FILENAME, ACTION_NAME, STRING, CTEMP
      LOGICAL OK
C
      CHARACTER*(*) BLANK
      PARAMETER( BLANK   = ''' ''' )
      CHARACTER*(*) TAB
      PARAMETER( TAB     = '     ' )
      CHARACTER*(*) EXIT
      PARAMETER( EXIT    = '''EXIT''' )
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Read templates
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LTAB  = LEN(TAB)
C
        CALL EZPICK(RCP_BANK(1:LEN(RCP_BANK)))
        CALL EZ_GET_CHARS('DISPATCH_TEMPLATE',NLINES_D,TEMPLATE_D,IER)
        IF ( IER .NE. 0 ) THEN
          STOP 'No DISPATCH_TEMPLATE'
        ENDIF
C
        CALL EZ_GET_CHARS('ACTION_TEMPLATE',NLINES_A,TEMPLATE_A,IER)
        IF ( IER .NE. 0 ) THEN
          STOP 'No ACTION_TEMPLATE'
        ENDIF
        CALL EZRSET
      ENDIF
C
      CALL WORD(ROUTINE(1:LEN(ROUTINE)),I,J,LR)
      CALL WORD(MENU(1:LEN(MENU)),I,J,LM)
      CALL SWORDS(TITLE(1:LEN(TITLE)),I,J,LT)
      CALL SWORDS(AUTHOR(1:LEN(AUTHOR)),I,J,LA)
      CALL SWORDS(PREFIX(1:LEN(PREFIX)),I,J,LP)
C
      CTEMP = ' Writing '//ROUTINE(1:LR)
      CALL INTMSG(CTEMP(1:LR+9))
C
C ****  Replace placeholders with tokens
C
      IF ( NCOMMAND .GT. 0 ) THEN
        NLINES = NLINES_D
        DO I =  1,NLINES
          LINE(I) = TEMPLATE_D(I)
        ENDDO
      ELSE
        NLINES = NLINES_A
        DO I =  1,NLINES
          LINE(I) = TEMPLATE_A(I)
        ENDDO
      ENDIF
C
      CALL SWAP_TOKEN('%ROUTINE',ROUTINE(1:LR),NLINES,LINE,LLINE)
      IF ( NCOMMAND .GT. 0 ) THEN
        CTEMP = ''''//MENU(1:LM)//''''
        CALL SWAP_TOKEN('%MENU',CTEMP(1:LM+2),NLINES,LINE,LLINE)
        CTEMP = ''''//TITLE(1:LT)//''''
        CALL SWAP_TOKEN('%TITLE',CTEMP(1:LT+2),NLINES,LINE,LLINE)
      ENDIF
      CALL SWAP_TOKEN('%DATE',DAY(1:11),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN('%AUTHOR',AUTHOR(1:LA),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN('%BLANK',BLANK,NLINES,LINE,LLINE)
      CALL SWAP_TOKEN('%EXIT',EXIT,NLINES,LINE,LLINE)
C
C ****  Open output file
C
      FILENAME = ROUTINE(1:LR)//'.FOR'
C
      CALL D0OPEN(LUN,FILENAME,'OFL',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
C
      IF ( NCOMMAND .GT. 0 ) THEN
C
C ****  Command with a MENU
C
        CALL FIND_TOKEN('CALL ',NLINES,LINE,ROW,COLUMN)
        DO II =  1, ROW
          WRITE(UNIT=LUN,FMT='(A)') LINE(II)(1:LLINE(II))
        ENDDO

        DO II = 1, NCOMMAND
          CALL SWORDS(COMMAND(II),I,J,L)
          CALL UPCASE(COMMAND(II)(I:J),COMMAND(II)(I:J))
C
          IF ( II .EQ. 1 ) THEN
            STRING = TAB//'   IF     ( COMMAND .EQ. '''//
     &      COMMAND(II)(I:J)//''''
          ELSE
            STRING = TAB//'   ELSEIF ( COMMAND .EQ. '''//
     &      COMMAND(II)(I:J)//''''
          ENDIF
C
          CALL SWORDS(STRING,I,J,L)
          K = J/LTAB
          J = LTAB*(K+1)
          STRING = STRING(1:J)//' ) THEN'
          J = J + 7
          WRITE(UNIT=LUN,FMT='(A)') STRING(1:J)
C
C ****  Create a call to action routine
C
          ACTION_NAME = ACTION(II)
          CALL WORD(ACTION_NAME,I,J,L)
C
          STRING = TAB//'     CALL '//PREFIX(1:LP)//ACTION_NAME(1:L)
          CALL SWORDS(STRING,I,J,L)
          WRITE(UNIT=LUN,FMT='(A)') STRING(1:J)
        ENDDO
C
      ELSE
C
C ****  Command without a MENU
C
        ROW = NLINES - 3
      ENDIF
C
      DO I =  ROW+1, NLINES
        WRITE(UNIT=LUN,FMT='(A)') LINE(I)(1:LLINE(I))
      ENDDO
C
C ****  Close file
C
      CLOSE(UNIT=LUN)
  999 RETURN
      END
