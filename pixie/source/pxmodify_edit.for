      SUBROUTINE PXMODIFY_EDIT(RCPFILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the screen available in the give RCPFILE
C-   After a screen is selected a submenu allowing the modifications to the
C-   screen selected will be displayed
C-
C-   Inputs  : RCPFILE [C*]: Name of the RCP file to be use to select the
C-                           screens
C-   Outputs : None
C-
C-   Created  16-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  The sunmenu was updated 
C-   Updated  27-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
C
      CHARACTER*40 SCREEN_NAME(40),SCREEN,MENUNAME,TITLE
      CHARACTER*32 COMMAND
      CHARACTER*1 CPORT
      INTEGER TOTAL_SCREEN,OUTNUM,I,J,L,IER,VIEWPORT,NPORT
      LOGICAL EZERROR,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Create the Graphics window
C
      IF ( FIRST ) THEN
        CALL PXBUILD_DI3INIT
        FIRST = .FALSE.
      ENDIF
C
C ****  Pick the requested RCP file
C
      CALL EZPICK(RCPFILE)
      IF ( .NOT. EZERROR(IER) ) THEN
C
C ****  Display the screens and get input form the user.
C
        CALL PUGET_SCREENS(SCREEN_NAME,TOTAL_SCREEN,OUTNUM)
C
        IF ( OUTNUM .NE. 0 ) THEN
          COMMAND = ' '
          VIEWPORT = 1
          CALL SWORDS(SCREEN_NAME(OUTNUM),I,J,L)
          MENUNAME = 'EDIT'
          SCREEN = SCREEN_NAME(OUTNUM)(I:J)
C
C ****  Draw the current viewport of the chosen screen
C
          CALL JCLEAR
          CALL JFRAME
          CALL PU_DRAW_SCREEN(SCREEN,RCPFILE,NPORT)
C
          DO WHILE ( COMMAND .NE. 'EXIT' )
C
            CALL SWORDS(SCREEN,I,J,L)
            TITLE = '       EDIT VIEW '//SCREEN(I:J)
            CALL MENUDO(TITLE,MENUNAME,COMMAND)
            IF ( COMMAND .EQ. 'MODIFY VIEW' ) THEN
              CALL PXMODIFY_VIEW(RCPFILE,SCREEN,NPORT)
            ELSEIF ( COMMAND .EQ. 'DELETE VIEW' ) THEN
              CALL PXMODIFY_DELETE_VIEW(RCPFILE,SCREEN,NPORT)
            ELSEIF ( COMMAND .EQ. 'CHANGE WINDOW' ) THEN
              CALL PXMODIFY_WINDOW(RCPFILE,SCREEN,NPORT)
            ELSEIF ( COMMAND .EQ. 'CENTER' ) THEN
              CALL PXMODIFY_CENTER(RCPFILE,SCREEN,NPORT)
            ELSEIF ( COMMAND .EQ. 'CHANGE ACTION' ) THEN
              CALL PXMODIFY_CHANGE_ACTION(RCPFILE,SCREEN,NPORT)
            ELSEIF ( COMMAND .EQ. 'MODIFY 3-D' ) THEN
              CALL PXMODIFY_3D(RCPFILE,SCREEN,NPORT)
            ELSEIF ( COMMAND .EQ. 'CHANGE NAME' ) THEN
              CALL PXMODIFY_NAME(RCPFILE,SCREEN)
            ENDIF
          ENDDO
        ENDIF
        CALL EZRSET
      ELSE
        SCREEN = ' RCP file was not defined: '//RCPFILE
        CALL INTMSG(SCREEN)
      ENDIF
  999 RETURN
      END
