      SUBROUTINE PXMODIFY_NAME(RCPFILE,SCREEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Changes the NAME of the screen requested
C-   If the screen requested is a combined view ONLY the menu names
C-   will be changed.
C-
C-   Inputs  : RCPFILE [C*]: RCP file name
C-             SCREEN  [C*]: Screen Name
C-
C-   Outputs : SCREEN  [C*]: Return the new screen name
C-
C-   Created  20-MAY-1991   Lupe Howell
C-   Updated   7-JUN-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  The combined array spacial case
C-   Updated   1-NOV-1991   Lupe Howell   Tidy up
C-   Updated  31-DEC-1991   Lupe Howell  Getting rid on *ACTIONS_RCP
C-   Updated  27-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER INAME(10)
      REAL    RVAL(10)
      EQUIVALENCE ( RVAL, INAME )
C
      INTEGER MAXROUTINES
      PARAMETER( MAXROUTINES = 200 )
C
      INTEGER IDX,I,K,J,OLEN,IER,NSCREEN,ISCREEN,WTYPE
      INTEGER IMENU,ISUBM,ISCRE,LENGTH,IVAL,REMREC(MAXROUTINES)
      INTEGER ITYP,SLEN,ARRAY_LENGTHMENU_LENGTH,PARAM_SIZE
      INTEGER BLANK,NMENUS,ARRAY_LENGTH,MENU_LENGTH,TOTREM
      INTEGER MENUPOS,REMTOT
C
      CHARACTER*40 ARRAY,OLD_NAME,NEW_NAME,NEW_ARRAY
      CHARACTER*40 NEW_MENU_NAME,PARAM_NAME
      CHARACTER*80 CVAL,RECORD_OUT,MESS
      CHARACTER*80 NAME1(MAXROUTINES),NAME2(MAXROUTINES)
      CHARACTER*80 ROUTINE(MAXROUTINES)
      CHARACTER*2 CNUM
C
      LOGICAL COMBINED,FOUND
C----------------------------------------------------------------------
      CALL SWORDS(SCREEN,I,J,SLEN)
C
C ****  Set up parameters acording to combined view or not
C
      IF ( SCREEN(SLEN:SLEN) .EQ. '%' ) THEN
        ARRAY = SCREEN(1:SLEN)
        COMBINED = .TRUE.
        ISCREEN = 1
        OLEN = SLEN - 1
        OLD_NAME = SCREEN(1:OLEN)
        PARAM_NAME = '%TITLE'
        PARAM_SIZE = 6
      ELSE
        ARRAY = 'PXSCREEN'
        CALL PU_GET_SCREEN_NUMBER(SCREEN,ISCREEN,NSCREEN,
     &          IER)
        CALL PU_GOTO_SCREEN(ISCREEN,IDX)
        COMBINED = .FALSE.
        CALL EZ_GET_ELEMENT(ARRAY,'NAME',1,ISCREEN,INAME,WTYPE,IER)
        OLEN = WTYPE - 10
        CALL DHTOC(OLEN,INAME,OLD_NAME)
        PARAM_NAME = 'NAME'
        PARAM_SIZE = 4
      ENDIF
C
C ****  Get old NAME and the new name
C
      CALL OUTMSG('1')
      MESS = 'Enter the new screen name ['//OLD_NAME(1:OLEN)//']:'
      CALL SWORDS(MESS,I,J,K)
      CALL GETPAR(1,MESS(I:J),'U',NEW_NAME)
      CALL SWORDS(NEW_NAME,I,J,LENGTH)
      IF ( LENGTH .GT. 0 ) THEN
        CALL DCTOH(LENGTH,NEW_NAME,INAME)
C
C ****  Get the menu, submenu and action indexes
C
        CALL PXBUILD_INDEX(RCPFILE,SCREEN,IMENU,ISUBM,ISCRE,IER)
C
C ****  Search for the menu item to be modified to use its position
C ****  in the array to modify it
C
        CALL PX_GET_ALL_MENUS(ACTION_SUBMENU(IMENU,ISUBM),
     &              NAME1,NAME2,ROUTINE,REMREC,NMENUS,IER)
        I = 0
        FOUND = .FALSE.
        DO WHILE ( (I .LT. NMENUS) .AND. (.NOT. FOUND)  )
          I = I + 1
          IF ( OLD_NAME(1:OLEN) .EQ. NAME2(I)(1:OLEN) ) THEN
            FOUND = .TRUE.
          ENDIF
        ENDDO
        IF ( FOUND ) THEN
C
C ****  If the menu found add the total remarks and the
C ****  logical values to determine the position of the element
C
          REMTOT = 0
          DO J = 1, (I-1)
            REMTOT = REMREC(J) + REMTOT
          ENDDO
          I = (4*(I-1)) + REMTOT + 3
          MENUPOS = I + 1
        ENDIF
C
C ****  Setting new NAME or %TITLE in PX_XXX_RCP bank
C
        ITYP = LENGTH + 10
        CALL EZ_CVT_ELEMENT
     &    (PARAM_NAME,PARAM_SIZE,IVAL,NEW_NAME,LENGTH,
     &    ' ',1,ITYP,RECORD_OUT)
        CALL EZ_MODIFY_ELEMENT(ARRAY,PARAM_NAME,ISCREEN,RECORD_OUT,IER)
C
C ****  If a combined view, set a % at the end of the new name
C ****  and an undescore between words
C
        IF ( COMBINED ) THEN
          NEW_ARRAY = NEW_NAME(1:LENGTH)
          ARRAY_LENGTH = LENGTH
          BLANK = INDEX(NEW_ARRAY(1:ARRAY_LENGTH),' ')
          DO  WHILE ( BLANK .NE. 0 )
            NEW_ARRAY = NEW_ARRAY(1:BLANK-1)//'_'//
     &        NEW_ARRAY(BLANK+1:ARRAY_LENGTH)
            BLANK = INDEX(NEW_ARRAY(1:ARRAY_LENGTH),' ')
          ENDDO
          IF ( NEW_ARRAY(ARRAY_LENGTH:ARRAY_LENGTH) .NE. '%' ) THEN
            NEW_ARRAY = NEW_ARRAY(1:LENGTH)//'%'
            ARRAY_LENGTH = ARRAY_LENGTH + 1
          ENDIF
          NEW_MENU_NAME = NEW_ARRAY(1:ARRAY_LENGTH)
          MENU_LENGTH = ARRAY_LENGTH
C
C ****  Rename the combined array name in PX_XXX_RCP bank
C ****  and the action name in the common blocks
C
          CALL EZ_RENAME_PARAM(SCREEN(1:SLEN),
     &      NEW_ARRAY(1:ARRAY_LENGTH),IER)
          ACTION_NAME(IMENU,ISUBM,ISCRE) = NEW_ARRAY(1:ARRAY_LENGTH)
        ELSE
C
C ****  If NOT a combined the menu_name should be the same as the
C ****  new name entered
C
          NEW_MENU_NAME = NEW_NAME(1:LENGTH)
          MENU_LENGTH = LENGTH
        ENDIF
C
C ****  Setting the new NAME in the menu items .i.e.
C ****  action and item common blocks
C
        IF ( IER .EQ. 0 ) THEN
          ACTION_COMMAND(IMENU,ISUBM,ISCRE) =
     &      NEW_MENU_NAME(1:MENU_LENGTH)
          ACTION_ITEM(IMENU,ISUBM,ISCRE) = NEW_NAME(1:LENGTH)
C
C ****  Setting the new name in the menu array in PX_XXX_RCP bank
C
          WRITE(CNUM,FMT='(I2)') MENUPOS
          CALL EZ_MODIFY_PARAM(ACTION_SUBMENU(IMENU,ISUBM),
     &      CNUM,IVAL,NEW_MENU_NAME(1:MENU_LENGTH),IER)
          WRITE(CNUM,FMT='(I2)') (MENUPOS-1)
          CALL EZ_MODIFY_PARAM(ACTION_SUBMENU(IMENU,ISUBM),
     &      CNUM,IVAL,NEW_NAME(1:LENGTH),IER)
C
C ****  Return the new screen name
C
          SCREEN = NEW_MENU_NAME(1:MENU_LENGTH)
C
C ****  Redrawn the view if it is NOT a combined view
C
          IF ( .NOT. COMBINED ) THEN
            CALL JCLEAR
            CALL JFRAME
            CALL PU_DRAW_SCREEN(SCREEN,RCPFILE,1)
          ENDIF
        ELSE
          MESS = ' Could not find index for '//SCREEN 
          CALL ERRMSG('NO INDEX','PXMODIFY_NAME',MESS,'W')
        ENDIF
      ENDIF
  999 RETURN
      END
