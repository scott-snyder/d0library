      SUBROUTINE PX_CHANGE_ACTION(RCPFILE,SCREEN,VIEWPORT,
     &  NEW_NAME,COMBINED,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change the Action name(s) in a given Screen.
C-   array.  If a combined view the correct %ACTION is replace and the
C-   %PACKAGE is aloud to be changed.  
C-
C-   Inputs  : RCPFILE    [C*]: RCP file name
C-             SCREEN     [C*]: Name of the screen tha is modify
C-             VIEWPORT   [I ]: Viewport number to be modify
C-             NEW_NAME   [C*]: New Action name
C-             COMBINED   [L ]: Combied view flag .TRUE. combined
C-                                                .FALSE. NOT combined
C-
C-   Outputs : IER        [I ]: 0 If OK
C-
C-   Created   6-JUN-1991   Lupe Howell
C-   Updated   1-NOV-1991   Lupe Howell  Fixing prompt message
C-   Updated  18-DEC-1991   Lupe Howell  Update and Entry PXGET_COMB_ACTION
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
      INTEGER VIEWPORT,IER
      CHARACTER*(*) NEW_NAME
      LOGICAL COMBINED
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
C
      INTEGER INTVAL
      REAL    REALVAL
      LOGICAL LOGVAL
      EQUIVALENCE(INTVAL,REALVAL,LOGVAL)
C
      INTEGER I,J,K,AL,SCREENLEN
      INTEGER IVAL,IACTION,IMENU,ISUBM,IDX,ITYPE,VLEN
      INTEGER ISCRE,BLEN,BEGVIEW,ENDVIEW,PLEN,TOTSCRE
C
      CHARACTER*80 NEW_PACKNAME,NEW_VIEW,STRING
      CHARACTER*132 BUFFER
C
      LOGICAL EZERROR
C
      CHARACTER*(*) COMBINED_ARRAY
      CHARACTER*(*) VIEW_NAME
      CHARACTER*(*) ACTIONNAME
      CHARACTER*(*) ACTION_PACK

      CHARACTER*80 BANK_NAME,FILE_NAME,ACTION_PACKAGE
      INTEGER VIEWNUM
C
      SAVE BANK_NAME
C----------------------------------------------------------------------
      CALL WORD(SCREEN,I,J,SCREENLEN)
C
C ****  Get the corresponding menu, submenu and action index
C
      IF ( IER .NE. 0 ) THEN
        STRING = ' Could not find index for screen '//SCREEN
        CALL INTMSG(STRING)
        GOTO 999
      ENDIF
      CALL WORD(NEW_NAME,I,J,AL)
      NEW_PACKNAME = ' '
      NEW_VIEW = ' '
      I = 1
C
C ****  Pick the RCP file
C
      CALL EZPICK(RCPFILE)
      IF ( EZERROR(IER) ) THEN
        STRING = ' Could not find file '//RCPFILE//
     &           ' in PX_CHANGE_COMBINED_ACTION'
        CALL INTMSG(STRING)
        GOTO 999
      ENDIF
C
C ****  Set the new action
C
      IF ( COMBINED ) THEN
C
C ****  Check if the new action name is defined
C ****  in the defined bank.  If it is get the view name
C ****  that corresponds to it to modify the combined
C ****  array with it.
C
        CALL WORD(BANK_NAME,I,J,BLEN)
        CALL WORD(ACTION_PACKAGE,I,J,PLEN)
        IDX = 0
        CALL EZPICK(BANK_NAME(1:BLEN)) ! Pick BANK_NAME
        IF ( EZERROR(IER) ) THEN
          STRING = ' The bank name for this view was not found '
     &             //BANK_NAME
          CALL INTMSG(STRING)
          GOTO 20
        ENDIF
        I = 0
        CALL PU_GOTO_ACTION(I,NEW_NAME,IDX)
        IF ( IDX .NE. 0 ) THEN
          CALL EZ_GET_ELEMENT('PXSCREEN','NAME',IDX,1,IVAL,ITYPE,IER)
          CALL EZ_GET_ELEMENT_CHARACTER(NEW_VIEW)
        ENDIF
        CALL EZRSET  ! Reset BANK_NAME
C
C ****  If the new action is not defined in the view package
C ****  OR the old action was not found get the name of the
C ****  package (if different) from the user
C
   20   IF (  IDX .EQ. 0 ) THEN
          STRING = '$The action name requested is not defined in the'//
     &      ' current package.'
          CALL OUTMSG(STRING)
          STRING = ' Please enter Package name if different ['//
     &             ACTION_PACKAGE(1:PLEN)//']:'
          CALL GETPAR(1,STRING,'U',NEW_PACKNAME)
C
C ****  If a new package is entered search in that package
C ****  for the new action name to find the name of its
C ****  corresponding view.
C
          CALL WORD(NEW_PACKNAME,I,J,PLEN)
          IF ( PLEN .GT. 0 ) THEN
            BANK_NAME = 'PX_'//NEW_PACKNAME(1:PLEN)//'_RCP'
            FILE_NAME =
     &          'D0$PIXIE:'//'PX_'//NEW_PACKNAME(1:PLEN)//'.RCP'
            CALL EZLOC(BANK_NAME,IER)  ! Checking if back exists
C
C ****  If bank has not been read read it
C
            IF ( IER .EQ. 0 ) THEN
              CALL INRCP(FILE_NAME,IER)  ! Read the package is different
              IF ( IER .NE. 0 ) THEN
                STRING = ' Package name '//FILE_NAME//' is illegal' 
                CALL INTMSG(STRING)
                CALL INTMSG(' Action name can not be found')
                GOTO 999
              ENDIF
            ENDIF
C
C ****  Search for the action name in this package
C
            I = 0
            CALL PU_GOTO_ACTION(I,NEW_NAME,IDX)
            IF ( IDX .NE. 0 ) THEN
              CALL EZ_GET_ELEMENT
     &            ('PXSCREEN','NAME',IDX,1,IVAL,ITYPE,IER)
              CALL EZ_GET_ELEMENT_CHARACTER(NEW_VIEW)
            ENDIF
          ENDIF
C
C ****  If the view of the new action had not been found get
C ****  the name form the user
C
          IF ( NEW_VIEW .EQ. ' ' ) THEN
            CALL OUTMSG(
     &        '$Please enter the name of the view corresponding')
            CALL GETPAR(1,
     &        ' to the requested action>','U',NEW_VIEW)
          ENDIF
        ENDIF
        CALL SWORDS(NEW_VIEW,BEGVIEW,ENDVIEW,I)
C
C ****  Set the name of the package if it is new
C
        CALL EZPICK(RCPFILE)
        IF ( NEW_PACKNAME .NE. ' ' ) THEN
          ITYPE = PLEN + VTCHAR
          CALL EZ_CVT_ELEMENT
     &      ('%PACKAGE',8,IVAL,NEW_PACKNAME(1:PLEN),PLEN,
     &      'Package name',12,ITYPE,BUFFER)
          CALL EZ_MODIFY_ELEMENT
     &      (SCREEN,'%PACKAGE',VIEWPORT,BUFFER,IER)
        ENDIF
C
C ****  Set the view name
C
        VLEN = ENDVIEW - BEGVIEW + 1
        ITYPE = VLEN + VTCHAR
        CALL EZ_CVT_ELEMENT
     &    ('%ACTION',7,IVAL,NEW_VIEW(BEGVIEW:ENDVIEW),VLEN,
     &    'Action name',11,ITYPE,BUFFER)
        CALL EZ_MODIFY_ELEMENT
     &    (SCREEN,'%ACTION',VIEWPORT,BUFFER,IER)
        CALL EZRSET
C
C ****  If NOT COMBINED view set the actions in the screen array
C
      ELSE
        CALL PU_GET_SCREEN_NUMBER(SCREEN,ISCRE,TOTSCRE,IER)
        ITYPE = AL + VTCHAR
        CALL EZ_CVT_ELEMENT
     &    ('ACTION',6,IVAL,NEW_NAME(1:AL),AL,
     &    'Action name',11,ITYPE,BUFFER)
        CALL EZ_MODIFY_ELEMENT
     &    ('PXSCREEN','ACTION',ISCRE,BUFFER,IER)
C
C ****  Updating the common blocks with the new action name
C
        CALL PXBUILD_INDEX(RCPFILE,SCREEN,IMENU,ISUBM,IACTION,IER)
        ACTION_NAME(IMENU,ISUBM,IACTION) = NEW_NAME(1:AL)
      ENDIF
      CALL EZRSET
      RETURN
C
C#######################################################################
      ENTRY PXGET_COMB_ACTION(RCPFILE,COMBINED_ARRAY,VIEWNUM,
     &    VIEW_NAME,ACTIONNAME,ACTION_PACK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the name of the action of a given view
C-   port number in a combined array
C-
C-   Inputs  : RCPFILE       [C*]: Name of the bank where the combined arary is
C-             COMBINED_ARRAY[C*]: Name of the combined array
C-             VIEWNUM        [I]: Number of the viewport
C-
C-   Outputs : VIEW_NAME     [C*]: Name of the view corresponds to the
C-                                 requested action
C-             ACTIONNAME    [C*]: Name of the action
C-             ACTION_PACK   [C*]: Name of the package that the action belongs
C-             IER           [ I]: If 0 Ok
C-
C-   Created  18-DEC-1991   Lupe Howell
C-
C----------------------------------------------------------------------
C
C ****  Get the package name
C ****  If the package is different than the current one
C ****  read it
C
      CALL EZPICK(RCPFILE)
      CALL EZ_GET_ELEMENT  !  Get the name of the package
     &    (COMBINED_ARRAY,'%PACKAGE',1,VIEWNUM,IVAL,ITYPE,IER)
      CALL EZ_GET_ELEMENT_CHARACTER(ACTION_PACK)
      CALL WORD(ACTION_PACK,I,J,PLEN)
      ACTION_PACKAGE = ACTION_PACK(1:PLEN)
      BANK_NAME = 'PX_'//ACTION_PACK(1:PLEN)//'_RCP'
      FILE_NAME ='D0$PIXIE:'//'PX_'//ACTION_PACK(1:PLEN)//'.RCP'
      BLEN = PLEN + 13
      CALL EZRSET ! Reset RCPFILE
C
C ****  Select package name if it is different from the
C ****  current package.
C
      IF ( BANK_NAME(1:BLEN) .NE. RCPFILE(1:BLEN) ) THEN
        CALL EZLOC(BANK_NAME,IER)
C
C ****  If bank has not been read read it
C
        IF ( IER .EQ. 0 ) THEN
          CALL INRCP(FILE_NAME,IER)  ! Read the package is different
          IF ( IER .NE. 0 ) THEN
            STRING = ' Package name '//FILE_NAME//' is illegal'
            CALL INTMSG(STRING)
            CALL INTMSG(' Action name can not be found')
            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C ****  Get the view name
C
      CALL EZPICK(RCPFILE)
      CALL EZ_GET_ELEMENT
     &    (COMBINED_ARRAY,'%ACTION',1,VIEWNUM,IVAL,ITYPE,IER)
      CALL EZ_GET_ELEMENT_CHARACTER(VIEW_NAME)
      CALL EZRSET  ! Reset RCPFILE
C
C ****  Search for the action name
C
      CALL EZPICK(BANK_NAME)  ! Pick the bank name of the view
      CALL PU_GET_SCREEN_NUMBER(VIEW_NAME,ISCRE,TOTSCRE,IER)
      CALL EZ_GET_ELEMENT('PXSCREEN','ACTION',1,ISCRE,IVAL,ITYPE,IER)
      CALL EZ_GET_ELEMENT_CHARACTER(ACTIONNAME)
      CALL EZRSET  ! Reset BANK_NAME
  999 RETURN
      END
