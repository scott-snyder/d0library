      SUBROUTINE PXMODIFY_CHANGE_ACTION(RCPFILE,SCREEN,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Changes the action routine of a given screen and
C-   viewport.
C-
C-   Inputs  : RCPFILE [C*]: RCP file name
C-             SCREEN  [C*]: Screen name
C-             VIEWPORT [I]: Viewport that the change is going to be
C-             NPORT    [I]: nober of ports in this screen
C-
C-   Outputs : None
C-
C-   Created  17-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up
C-   Updated   1-NOV-1991   Lupe Howell  Clear the CURRENT_ACTION if no input
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
C
      INTEGER I,II,J,JJ,K,L,IER,VIEWPORT,TOTPORTS
      INTEGER IDX(MAXCVIEW),UNIQUE_PORTS,TOTAL_ACTIONS,TEMP_VIEWPORT
      INTEGER KK,CURL,ISCRE,IVAL,ITYPE,TOTSCRE
C
      REAL    XMIN(MAXCVIEW),XMAX(MAXCVIEW)
      REAL    YMIN(MAXCVIEW),YMAX(MAXCVIEW)
C
      CHARACTER*80 STRING,OUTSTR,VIEW_NAME,ACTION_PACK
      CHARACTER*32 NEW_NAME,CURRENT_PACKAGE
      CHARACTER*32 CURRENT_ACTION(MAXCVIEW)
      CHARACTER*32 PACKAGES(MAXCVIEW),ACTIONS(MAXCVIEW)
      CHARACTER*1  CVIEW
C
      LOGICAL EZERROR,COMBINED
C----------------------------------------------------------------------
C
C ****  Determine if the command entered is a combined view
C
      CALL WORD(SCREEN,I,J,L)
      IF ( SCREEN(L:L) .EQ. '%' ) THEN
        COMBINED = .TRUE.
      ELSE
        COMBINED = .FALSE.
      ENDIF
C
C ****  Select port that is going to be modify if the
C ****  number of viewports is greather than 1
C
      IF ( NPORT .GT. 1 ) THEN
        CALL PU_SELECT_PORT(NPORT,VIEWPORT)
      ELSE
        VIEWPORT = NPORT
      ENDIF
C
C ****  Get the number of unique and total viewports
C ****  to determine if the combined views are separate or
C ****  together
C
      I = INDEX(RCPFILE,'PX_')
      J = INDEX(RCPFILE,'_RCP')
      CURRENT_PACKAGE = RCPFILE(I+3:J-1)
      CALL PU_GET_UNIQUE_PORTS(CURRENT_PACKAGE,SCREEN,
     &   XMIN,XMAX,YMIN,YMAX,PACKAGES,
     &   ACTIONS,IDX,UNIQUE_PORTS,TOTPORTS)
C
C ****  Determine if the view has multiple viewports or only one
c ****  If the number of unique ports EQUAL to the number 
c ****  of ports the view the total number of actions to be 
c ****  modifified is 1.  
C
      IF ( NPORT .EQ. TOTPORTS ) THEN
        TOTAL_ACTIONS = 1
C
C **** If the number of unique is NOT EQUAL to the total of ports 
c **** in the view the total nubemr of actions to be modifyied
C **** is equat to the total ports 
C       
      ELSE
        TOTAL_ACTIONS = TOTPORTS
      ENDIF
C
C ****  Write heading 
C
      CALL OUTMSG('1')
      CALL OUTMSG('  Give the name of the routine you want to call')
      CALL OUTMSG('  to fill this viewport.')
      CALL OUTMSG( '  <CR> keeps the default if non empty')
      CALL OUTMSG( '       ends the list if no default value')
      CALL OUTMSG( '  "-"  suppress the default value from the list')
      TEMP_VIEWPORT = VIEWPORT
      DO KK = 1, TOTAL_ACTIONS
C
C ****  If a combined view, using the viewport number, get the 
C ****  current action routine name 
C
        IF ( COMBINED ) THEN
          CALL PXGET_COMB_ACTION(RCPFILE,SCREEN,TEMP_VIEWPORT,
     &      VIEW_NAME,CURRENT_ACTION(KK),ACTION_PACK,IER)
        ELSE
C
C ****  If not a combined view get the current action routine from the 
C ****  common block.
C
          CALL EZPICK(RCPFILE)
          CALL PU_GET_SCREEN_NUMBER(SCREEN,ISCRE,TOTSCRE,IER)
          CALL EZ_GET_ELEMENT
     &          ('PXSCREEN','ACTION',1,ISCRE,IVAL,ITYPE,IER)
          CALL EZ_GET_ELEMENT_CHARACTER(CURRENT_ACTION(1))
          CALL EZRSET
        ENDIF
C
C ****  Display old action name 
C
        IF( IER .NE. 0 ) THEN
          CURRENT_ACTION(KK) = ' '
          JJ = 1
        ELSE
          CALL WORD(CURRENT_ACTION(KK),II,CURL,L)
        ENDIF
        CALL PXGTVAL(KK,1,CVIEW)
        CALL OUTMSG(' Action for View Number '//CVIEW)
C
C ****  Get new action routine name
C
        CALL GETPAR(1,'Enter Action Routine for view '//CVIEW//
     &    ' ['//CURRENT_ACTION(KK)(1:CURL)//']:','U',STRING)
        CALL SWORDS(STRING,II,JJ,L)
        IF ( L .NE. 0 ) THEN
          CALL UPCASE (STRING,OUTSTR)
          CALL SWORDS(OUTSTR,II,JJ,L)
C
C ****  If input was '-' delete the action
C
          IF ( OUTSTR .EQ. '-' ) THEN
            CALL PX_DELETE_ACTIONS(RCPFILE,SCREEN(1:L),
     &          CURRENT_ACTION(KK),TEMP_VIEWPORT,COMBINED,IER)
          ELSE
C
C ****  If a Routine name was entered modify the
C ****  corresponding comon block variable and
C ****  the the *ACTION_RCP array name
C
            NEW_NAME = STRING(II:JJ)
            CALL PX_CHANGE_ACTION(RCPFILE,SCREEN,TEMP_VIEWPORT,
     &         NEW_NAME,COMBINED,IER)
          ENDIF
        ENDIF
        TEMP_VIEWPORT = TEMP_VIEWPORT + 1
      ENDDO
C
C ****  Draw the view with the new action routines names 
C
      CALL JCLEAR
      CALL JFRAME
      CALL PU_DRAW_SCREEN(SCREEN,RCPFILE,NPORT)
  999 RETURN
      END
