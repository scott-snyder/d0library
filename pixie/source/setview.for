      SUBROUTINE SETVIEW( PACKAGE_NAME, COMMAND, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set view for the requested package and
C-   command. If the command is a combined-view command then initialize
C-   SETVIEW so that it will process subsequent commands as part of a
C-   combined-view sequence of commands. If the command is END% then
C-   set IER = 1 and disable the command fifo. If the RESET_VIEWS
C-   flag is active turn off that flag (including the combined_mode
C-   flag) and reset the view parameters.
C-
C-   Inputs  : PACKAGE_NAME     [C*]    Name of package
C-             COMMAND          [C*]    Name of command (screen)
C-
C-   Output  : IER      [I]     0 --- OK
C-                              1 --- END% or combined-view command
C-                              2 --- Screen NOT found
C-                              3 --- Unable to modify parameters
C-
C-
C-   Created   1-MAY-1990   Lupe Howell
C-   Updated  24-SEP-1990   Harrison B. Prosper
C-      Added COMMAND argument. Added calls to process combined-views
C-   Updated  19-OCT-1990   Lupe Howell, Harrison B. Prosper
C-      Now handles zoom
C-   Updated  28-NOV-1990   Harrison B. Prosper
C-      Allow for superimposing views
C-   Updated   3-DEC-1990   Harrison B. Prosper
C-   Modified 18-DEC-1990   Nobu. Oshima( Retained to Temporary segment. )
C-   Updated  28-JAN-1991   Lupe Howell  Clean up unnecessary calls and
C-       check making sure we the right RCP file is activated.
C-   Updated   6-MAY-1991   Harrison B. Prosper
C-      Tidy up a little
C-   Updated   8-MAY-1991   Harrison B. Prosper
C-      Update WINDOWYMIN, WINDOWYMAX
C-   Updated  15-MAY-1991   Harrison B. Prosper
C-      Changed arguments to PU_GOTO_SCREEN
C-   Updated  18-MAY-1991   Harrison B. Prosper
C-      Move DI3000 code into PU_SETUP_VIEWPORT
C-   Updated  20-JUN-1991   Nobuaki Oshima, Harrison B. Prosper
C-      Add code to handle picking
C-   Updated  21-JUN-1991   Harrison B. Prosper  
C-      Use remark instead of command as title 
C-   Modified 08-JUL-1991   Nobuaki Oshima
C-      Skip PUHEAD when 'PICKING' is .TRUE., too.
C-   Updated  30-MAR-1992   Lupe Howell  Modify for Unix 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE_NAME
      CHARACTER*(*) COMMAND
      LOGICAL IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      INTEGER I, J, K, L, II, JJ, LL
      INTEGER ISCREEN, OLD_ISCREEN
      INTEGER IDX,LCOMM,LPACK,LCTITLE
C
      LOGICAL FLGVAL,FLAGRESET,ZOOMING,ROTATING,COMBINED_MODE
      LOGICAL SUPERIMPOSE, PICKING, WITHIN, PICKABLE
      REAL    XP(2), VPORTXMIN, VPORTXMAX, VPORTYMIN, VPORTYMAX
C
      CHARACTER*32 OLD_COMMAND, OLD_PACKAGE_NAME
      CHARACTER*32 BANK_NAME, CURRENT_BANK
      CHARACTER*80 CTITLE,STRING
C----------------------------------------------------------------------
      SAVE OLD_PACKAGE_NAME,OLD_COMMAND,OLD_ISCREEN
      DATA OLD_PACKAGE_NAME  /'                    '/
      DATA OLD_COMMAND  /'                    '/
C----------------------------------------------------------------------
      CALL SWORDS(COMMAND(1:LEN(COMMAND)),I,LCOMM,J)
      CALL WORD(PACKAGE_NAME(1:LEN(PACKAGE_NAME)),I,J,LPACK)
C
C ****  Check for bank mis-match
C
      CALL EZTELL(BANK_NAME,L)
      CURRENT_BANK = 'PX_'//PACKAGE_NAME(1:LPACK)//'_RCP'
      IF ( BANK_NAME(1:L) .NE. CURRENT_BANK(1:(LPACK+7)) ) THEN
        CALL EZPICK(CURRENT_BANK)
        FLAGRESET = .TRUE.
      ENDIF
C
C ****************************************
C ****  Check for special commands:
C ****          a) Combined-View-Command
C ****             (identified by % at end)
C ****          b) END% command
C ****************************************
C
C
      COMBINED_MODE = FLGVAL('COMBINED_MODE')
      SUPERIMPOSE   = FLGVAL('SUPERIMPOSE')
      ZOOMING       = FLGVAL('ZOOMING')
      ROTATING      = FLGVAL('ROTATING')
      PICKING       = FLGVAL('PICKING')
C
      IF ( COMMAND(LCOMM:LCOMM) .EQ. '%' ) THEN
C
        IER = 1                         ! Combined mode
C
        IF ( COMMAND(1:LCOMM) .EQ. 'END%' ) THEN
          CALL PX_DISABLE_COMMAND_FIFO  ! Turn off fifo
          CALL FLGSET('COMBINED_MODE',.FALSE.)
        ELSE
C
C ****  Display Combined View Title and initialize views
C
          CALL EZGETS(COMMAND(1:LCOMM),2,CTITLE,LCTITLE,IER)
          IF ( .NOT. PICKING ) THEN
            CALL PUHEAD(CTITLE(1:LCTITLE))
          ENDIF
C
          IF( ( .NOT. ZOOMING   ) .AND.
     &        ( .NOT. PICKING   ) .AND.
     &        ( .NOT. ROTATING  ) )THEN
C
            CALL PU_INITIALIZE_VIEWS
     &        (PACKAGE_NAME(1:LPACK),COMMAND(1:LCOMM))
          ENDIF
C
          CALL FLGSET('COMBINED_MODE',.TRUE.)
          CALL PX_COMBINE_VIEWS
     &      (PACKAGE_NAME(1:LPACK),COMMAND(1:LCOMM))
        ENDIF
C
        GOTO 999
C
C ****  If we are in single view mode we should ALWAYS initialize
C ****  and save the WINDOW parameters
C
      ELSE
C
        IF ( ( .NOT. COMBINED_MODE ) .AND.
     &       ( .NOT. PICKING       ) .AND.
     &       ( .NOT. ZOOMING       ) .AND.
     &       ( .NOT. ROTATING      )) THEN
C
          CALL PU_INITIALIZE_VIEWS
     &      (PACKAGE_NAME(1:LPACK),COMMAND(1:LCOMM))
        ENDIF
      ENDIF
C
C ****  Get Screen Number for current package
C
      CALL PU_GET_SCREEN_NUMBER(COMMAND,ISCREEN,NSCREEN,IER)
      IF ( IER .NE. 0 ) THEN
        STRING = 'Error accessing view '//COMMAND
        CALL ERRMSG('BAD_VIEW','SETVIEW',
     &    STRING,'W')
        GOTO 999
      ENDIF
      CALL PU_GOTO_SCREEN(ISCREEN,IDX)
C
C ****************************************
C ****  We've found the right screen
C ****************************************
C
C ****  If we are in combined-view mode modify the parameters for
C ****  the current command, otherwise simply save the original
C ****  WINDOW parameters. However, do nothing if we are in
C ****  ZOOMING or ROTATING mode.
C
      IF ( ( .NOT. ZOOMING      ) .AND.
     &     ( .NOT. PICKING      ) .AND.
     &     ( .NOT. ROTATING     )) THEN
C
        CALL PU_MODIFY_VIEW
     &    (PACKAGE_NAME(1:LPACK),COMMAND(1:LCOMM),IER)
C
        IF ( IER .NE. 0 ) THEN
          STRING = 'Problem modifying parameter '//COMMAND(1:LCOMM)
          CALL ERRMSG('PIXIE_ERROR','SETVIEW',
     &          STRING,'W')
          IER = 3
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Check whether to clear screen and write title
C
      IF ( ( .NOT. COMBINED_MODE ) .AND.
     &     ( .NOT. PICKING       ) .AND.
     &     ( .NOT. SUPERIMPOSE   ) ) THEN
C
        CALL PU_GET_SCREEN_TITLE(IDX,CTITLE,LCTITLE,IER)
        CALL PUHEAD( CTITLE(1:LCTITLE) )
      ENDIF
C
C ****  If in picking mode we need to check PICKABLE flag
C ****  and also we should check if selected point lies
C ****  within the current viewport
C
      IF ( PICKING ) THEN
C
        CALL PU_GET_SCREEN_PARAM(IDX,'PICKABLE',PICKABLE,IER)
        IF ( PICKABLE ) THEN
C
C ****  Get selected point in PIXIE virtual coordinates
C
          CALL PU_GET_PICKP(XP)
C
C ****  Check if point is within viewport boundary
C
          WITHIN = .FALSE.
          CALL PU_GET_SCREEN_PARAM
     &            (IDX,'VPORTXMIN',VPORTXMIN,IER)
          IF ( XP(1) .GE. VPORTXMIN ) THEN
            CALL PU_GET_SCREEN_PARAM
     &              (IDX,'VPORTXMAX',VPORTXMAX,IER)
            IF ( XP(1) .LE. VPORTXMAX ) THEN
              CALL PU_GET_SCREEN_PARAM
     &                (IDX,'VPORTYMIN',VPORTYMIN,IER)
              IF ( XP(2) .GE. VPORTYMIN ) THEN
                CALL PU_GET_SCREEN_PARAM
     &                  (IDX,'VPORTYMAX',VPORTYMAX,IER)
                IF ( XP(2) .LE. VPORTYMAX ) THEN
                  WITHIN = .TRUE.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C ****  If point is NOT within current viewport then do NOT
C ****  call the action routine.
C
          IF ( .NOT. WITHIN ) THEN
            COMMAND = '$'//COMMAND
          ENDIF
        ELSE
          COMMAND = '$'//COMMAND
        ENDIF
      ENDIF
C
C ****  Setup viewport and its world coordinates
C
      CALL PU_SETUP_VIEWPORT(IDX,IER)
C
C ****  Note screen index etc.
C
  900 CONTINUE
      IF ( FLAGRESET ) THEN
        CALL EZRSET
        FLAGRESET = .FALSE.
      ENDIF
      OLD_ISCREEN = ISCREEN
C
  999 CONTINUE
      OLD_PACKAGE_NAME = PACKAGE_NAME
      OLD_COMMAND      = COMMAND
      RETURN
      END
