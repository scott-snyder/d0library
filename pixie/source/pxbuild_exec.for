      SUBROUTINE PXBUILD_EXEC(VERSION,MENU,ROUTINE,MENU_INDX,INDX,TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write EXEC interface routine.
C-
C-   Inputs  : VERSION  [C*]    PXBUILD version number
C-             MENU     [C*]    Menu/Package Name
C-             ROUTINE  [C*]    Routine Name
C-             MENU_INDX[I ]    Index representing the menu that the EXEC
C-                              routine corresponds to
C-             INDX     [I ]    Index representing the submenu that the EXEC
C-                              routine corresponds to
C-             TITLE    [C*]    Menu Title
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-   Updated  19-SEP-1990   Harrison B. Prosper , Lupe Howell
C-   Updated  26-SEP-1990   Harrison B. Prosper
C-   Updated  30-APR-1991   Lupe Howell  Combined Views 
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) VERSION
      CHARACTER*(*) MENU
      CHARACTER*(*) ROUTINE
      CHARACTER*(*) TITLE
      INTEGER MENU_INDX,INDX
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      INTEGER I,J,K,L,N,NLINES,IER,ROW,COLUMN,II,JJ,III,JJJ
      INTEGER PFNUM,LR,LT,LM,LV,ISTART,IEND,TL
C
      LOGICAL EZERROR,ACTIONS
      CHARACTER*23 DAY
      CHARACTER*80 TEMPLATE(MAXLINE),STRING,TEMP_STRG
      CHARACTER*32 SET_ROUTINE,RESET_ROUTINE
C
      CHARACTER*(*) BLANK
      PARAMETER( BLANK   = ''' ''' )
      CHARACTER*(*) TAB
      PARAMETER( TAB     = '     ' )
      CHARACTER*(*) EXIT
      PARAMETER( EXIT    = '''EXIT''' )
C----------------------------------------------------------------------
      LOGICAL FIRST,ONCE
      DATA FIRST/.TRUE./
      DATA ONCE/.FALSE./
      SAVE FIRST,ONCE

C----------------------------------------------------------------------
C
C ****  Read template
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('PXBUILD_RCP')
        IF ( EZERROR(IER) ) THEN
          GOTO 999
        ELSE
          CALL EZ_GET_CHARS('EXEC_TEMPLATE',NLINES,TEMPLATE,IER)
          CALL EZRSET
        ENDIF
      ENDIF
C
      CALL WORD(VERSION,I,J,LV)
      CALL WORD(ROUTINE,I,J,LR)
      CALL WORD(MENU,I,J,LM)
      CALL SWORDS(TITLE,I,J,LT)
C
C ****  Get date
C
      CALL LIB$DATE_TIME(DAY)
C
C **********************************************
C ****  Do INTERFACE ROUTINE
C **********************************************
C
C ****  Replace placeholders with tokens
C
      DO I =  1,NLINES
        LINE(I) = TEMPLATE(I)
      ENDDO
C
      CALL SWAP_TOKEN
     &  ('%ROUTINE',ROUTINE(1:LR),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN
     &  ('%PACKAGE',MENU(1:LM),NLINES,LINE,LLINE)
      TEMP_STRG = ''''//MENU(1:LM)//''''
      CALL WORD(TEMP_STRG,I,J,TL)
      CALL SWAP_TOKEN
     &  ('%MENU',TEMP_STRG(1:TL),NLINES,LINE,LLINE)
      TEMP_STRG = ''''//TITLE(1:LT)//''''
      CALL WORD(TEMP_STRG,I,J,TL)
      CALL SWAP_TOKEN
     &  ('%TITLE',TEMP_STRG(1:TL),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN
     &  ('%DATE',DAY(1:11),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN
     &  ('%VERSION',VERSION,NLINES,LINE,LLINE)
      CALL SWAP_TOKEN
     &  ('%BLANK',BLANK,NLINES,LINE,LLINE)
      CALL SWAP_TOKEN
     &  ('%EXIT',EXIT,NLINES,LINE,LLINE)
C
C ****  Open output file
C
      TEMP_STRG = ROUTINE(1:LR)//'.FOR'
      CALL PXOPEN(LUN,TEMP_STRG,'OL',IER)
      IF (IER .NE. 0 ) GOTO 999
CC?      OPEN (UNIT=LUN,FILE=ROUTINE(1:LR)//'.FOR',STATUS='NEW',
CC?     &  CARRIAGECONTROL='LIST')
C
C ****  Write DO WHILE
C
      CALL FIND_TOKEN('PUMEN',NLINES,LINE,IEND,COLUMN)
      DO I =  ROW+1,IEND
        WRITE(UNIT=LUN,FMT='(A)') LINE(I)(1:LLINE(I))
      ENDDO

      ACTIONS = .FALSE.
      DO III = 1 , ACTION_COUNT(MENU_INDX,INDX)
        CALL SWORDS(ACTION_COMMAND(MENU_INDX,INDX,III),I,J,L)
C
        IF( ACTION_COMMAND(MENU_INDX,INDX,III)(L:L) .NE. '%' ) THEN
          ACTIONS = .TRUE.
          IF ( .NOT. ONCE ) THEN
            WRITE(UNIT=LUN,FMT='(A)')
     &    TAB//'   IF     ( COMMAND .EQ. '''//
     &    ACTION_COMMAND(MENU_INDX,INDX,III)(I:J)//''' ) THEN'
            ONCE = .TRUE.
          ELSE
            WRITE(UNIT=LUN,FMT='(A)')
     &    TAB//'   ELSEIF ( COMMAND .EQ. '''//
     &    ACTION_COMMAND(MENU_INDX,INDX,III)(I:J)//''' ) THEN'
          ENDIF
C
          CALL WORD(ACTION_NAME(MENU_INDX,INDX,III),I,J,L)
          WRITE(UNIT=LUN,FMT='(A)')
     & TAB//'     CALL '//ACTION_NAME(MENU_INDX,INDX,III)(1:L)
        ENDIF
      ENDDO
C
C ****  If there were no actions skip ENDIF in the template 
C
      IF ( .NOT. ACTIONS ) THEN
        IEND = IEND + 1
      ENDIF

      DO I =  IEND+1,NLINES
        WRITE(UNIT=LUN,FMT='(A)') LINE(I)(1:LLINE(I))
      ENDDO
C
C ****  Close file
C
      CLOSE(UNIT=LUN)
      CALL INTMSG(' Exec Routine Done')
  999 RETURN
      END
