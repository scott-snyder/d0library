      SUBROUTINE EZ_BUILD_ALL_DISPATCH(TEMPLATE_RCP,MENU_RCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create dispatch routines from the menus
C-   defined in the specified RCP-bank, using the specified templates.
C-
C-   Inputs  : TEMPLATE_RCP     [C*]    Bank containing templates
C-             MENU_RCP         [C*]    Bank containing menu definition
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-JUN-1991   Harrison B. Prosper
C-   Updated   3-JUL-1991   Harrison B. Prosper
C-      Change calling sequence to ez_get_next_button
C-   Updated  12-Feb-1991   Herbert Greenlee
C-      Use offline_util routines to get date/time
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEMPLATE_RCP, MENU_RCP
C----------------------------------------------------------------------
      INTEGER NUMBER,NUMBER_MENUS,LP,LR,LA,I,J,IER
      INTEGER LHELP,LLHELP,LITEM,LCOMMAND,LACTION,CONTROL,NCOMMAND
      INTEGER OFFTIM, D0TLOCT
C
      INTEGER MAXMENU
      PARAMETER( MAXMENU = 25 )
      INTEGER LUN
      PARAMETER( LUN     = 20 )
C
      CHARACTER*23 DAY
      CHARACTER*26 D0_DAY
      CHARACTER*32 MENU(MAXMENU),ROUTINE
      CHARACTER*32 ACTION(MAXMENU),COMMAND(MAXMENU),PREFIX
      CHARACTER*40 ITEM,KOMMAND,AKTION,RECORD(MAXMENU*2),AUTHOR
      CHARACTER*132 HELP,TITLE(MAXMENU)
C
      LOGICAL EZERROR,SWITCH,LAST
      LOGICAL ADD_MENU_ITEM,GET_TITLE
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Get date
C
      CALL OFTSTR(D0TLOCT(OFFTIM()),D0_DAY)
      DAY = D0_DAY
C
C ****  Select MENU RCP bank
C
      CALL EZPICK (MENU_RCP(1:LEN(MENU_RCP)))
      IF ( EZERROR(IER) ) THEN
        GOTO 999
      ENDIF
C
      CALL EZ_GET_MENUS_TITLES(MENU,TITLE,NUMBER_MENUS,IER)
C
      CALL EZGETS('ACTION_PREFIX',1,PREFIX,LP,IER)
      PREFIX = PREFIX(1:LP)
C
      CALL TRNLNM('AUTHOR',AUTHOR,LA)
      AUTHOR = AUTHOR(1:LA)
C
C ****  Loop over MENUs
C
      DO I =  1, NUMBER_MENUS
C
        NCOMMAND= 0
        CONTROL = 0
        LAST    = .FALSE.
        DO WHILE ( .NOT. LAST )
          CALL EZ_GET_NEXT_BUTTON(MENU(I),
     &                            ADD_MENU_ITEM,
     &                            SWITCH,
     &                            ITEM,LITEM,
     &                            KOMMAND,LCOMMAND,
     &                            AKTION,LACTION,
     &                            HELP,LHELP,
     &                            LAST,
     &                            CONTROL)
          IF ( ADD_MENU_ITEM ) THEN
            IF ( KOMMAND(1:LCOMMAND) .NE. 'MENCTR' ) THEN
              NCOMMAND = NCOMMAND + 1
              COMMAND(NCOMMAND) = KOMMAND(1:LCOMMAND)
C
              IF ( LACTION .GT. 0 ) THEN
                ACTION(NCOMMAND) = AKTION(1:LACTION)
              ELSE
C
C ****  Make sensible action routine name
C
                AKTION = KOMMAND(1:LCOMMAND)
                CALL SWAP_TOKEN(' ','_',1,AKTION,LR)
                CALL SWAP_TOKEN('%','_',1,AKTION,LR)
                CALL SWAP_TOKEN('$','_',1,AKTION,LR)
                CALL SWAP_TOKEN('/','_',1,AKTION,LR)
                CALL SWAP_TOKEN('&','_',1,AKTION,LR)
                CALL SWAP_TOKEN('+','_',1,AKTION,LR)
                CALL SWAP_TOKEN('-','_',1,AKTION,LR)
                ACTION(NCOMMAND) = AKTION(1:LR)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
C ****  Make sensible dispatch routine name
C
        ROUTINE = MENU(I)
        CALL SWAP_TOKEN(' ','_',1,ROUTINE,LR)
        CALL SWAP_TOKEN('%','_',1,ROUTINE,LR)
        CALL SWAP_TOKEN('$','_',1,ROUTINE,LR)
        CALL SWAP_TOKEN('/','_',1,ROUTINE,LR)
        CALL SWAP_TOKEN('&','_',1,ROUTINE,LR)
        CALL SWAP_TOKEN('+','_',1,ROUTINE,LR)
        CALL SWAP_TOKEN('-','_',1,ROUTINE,LR)
        ROUTINE = PREFIX(1:LP)//ROUTINE(1:LR)
C
        CALL EZ_BUILD_ONE_DISPATCH
     &    (LUN,
     &     TEMPLATE_RCP(1:LEN(TEMPLATE_RCP)),
     &     ROUTINE,
     &     MENU(I),
     &     TITLE(I),
     &     DAY,AUTHOR,PREFIX,COMMAND,ACTION,NCOMMAND)
C
      ENDDO
C
  900 CONTINUE
      CALL EZRSET
C
  999 RETURN
      END
