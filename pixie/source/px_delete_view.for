      SUBROUTINE PX_DELETE_VIEW(RCPFILE,VIEWNAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Deletes a view in a RCP file of the name PX_*_RCP
C-
C-   Inputs  : RCPFILE [C*]: Name of the RCP file
C-             VIEWNAME[C*]: Name of the view to delete This is name is
C-                           the command view name
C-
C-   Outputs : IER      [I]: 0 if okay
C-
C-   Created   6-DEC-1991   Lupe Howell
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) VIEWNAME
      INTEGER IER
C
      INTEGER MAXMENU
      PARAMETER( MAXMENU =10  )
C
      INTEGER MAXROUTINES
      PARAMETER( MAXROUTINES = 200 )
C
      INTEGER I,J,K,LNAM,NMENUS,TOTMEN,REMREC(MAXROUTINES),MENU_INDEX
      INTEGER ISCRE,TOTSCRE,NREC,NSCRE
      LOGICAL EZERROR,ACTIVE,FOUND
      CHARACTER*132 MENUS(MAXROUTINES),TEMP
      CHARACTER*132 MENU1(MAXROUTINES),MENU2(MAXROUTINES)
      CHARACTER*80 ACTION(MAXROUTINES),ARRAY_NAME,MENU1_NAME,MENU2_NAME
      CHARACTER*10 CVAL,REM
C&IF LINUX
C&      character*132 xrcpfile
C&ENDIF
C----------------------------------------------------------------------
      CALL WORD(VIEWNAME,I,J,LNAM)
      CALL EZPICK(RCPFILE)
      IF ( EZERROR(IER) ) THEN
        CALL WORD(I,J,K)
C&IF LINUX
C&        xrcpfile = rcpfile
C&        CALL INTMSG(
C&     &    ' Could NOT find bank '//xRCPFILE(1:K)//' to delete view')
C&ELSE
        CALL INTMSG(
     &    ' Could NOT find bank '//RCPFILE(1:K)//' to delete view')
C&ENDIF
        GOTO 999
      ENDIF
C
C ****  Searching for the menu requested 
C
      CALL EZFETCH('COMPACK_MENUS',MAXROUTINES,NMENUS,MENUS,IER)
      IF ( IER .EQ. 0 ) THEN
        IF ( NMENUS .EQ. 1 ) THEN
          CALL WORD(MENUS(1),K,J,I)
          TEMP = MENUS(1)(J+1:132)
          CALL WORD(TEMP,K,J,I)
          MENUS(1) = TEMP(K+1:J-1)
        ENDIF
        I = 0
        ACTIVE = .TRUE.
        FOUND = .FALSE.
        DO WHILE ( ACTIVE )
          I = I + 1
          CALL PX_GET_ALL_MENUS
     &      (MENUS(I),MENU1,MENU2,ACTION,REMREC,TOTMEN,IER)
          J = 0
          DO WHILE ( (.NOT. FOUND) .AND. (J .LT. TOTMEN ) ) 
            J = J + 1
            IF ( MENU2(J)(1:LNAM) .EQ. VIEWNAME(1:LNAM) ) THEN
              FOUND = .TRUE.
              ARRAY_NAME = MENUS(I)
              MENU1_NAME = MENU1(J)
              MENU2_NAME = MENU2(J)
              NREC = REMREC(J)
            ENDIF
          ENDDO
          ACTIVE = ( .NOT. FOUND ) .AND. ( I .LT. NMENUS )
        ENDDO
C
C ****  Delete the menu requested in the menu array
C
        IF ( FOUND ) THEN
          CALL PX_GET_MENU_INDEX(J,MENU_INDEX)
          CALL EZ_REMOVE_PARAM
     &      (ARRAY_NAME,'INDEX',MENU_INDEX,(NREC+4),IER)
C
C ****  If the view to be deleted is a COMBINED view
C ****  delete the combined array
C
          IF ( VIEWNAME(LNAM:LNAM) .EQ. '%') THEN
            CALL EZDELETE(MENU2_NAME,IER)
          ELSE
C
C ****  If the view is not combined delete the view
C ****  from the PXSCREEN array and decrease the number in NSCREEN
C
            CALL PU_GET_SCREEN_NUMBER(MENU2_NAME,ISCRE,TOTSCRE,IER)
            CALL EZ_REMOVE_ELEMENT('PXSCREEN','NAME',ISCRE,22,IER)
            CALL EZ_GET_ARRAY('PXSCREEN','NSCREEN',I,NSCRE,CVAL,
     &        J,REM,IER)     
            CALL EZ_SET_ARRAY('PXSCREEN','NSCREEN',(NSCRE-1),IER)
          ENDIF
        ENDIF
      ELSE
C&IF LINUX
C&        xrcpfile = rcpfile
C&        CALL INTMSG(' No COMPACK_MENUS found in '//
C&     &              xRCPFILE(1:len(rcpfile)))
C&ELSE
        CALL INTMSG(' No COMPACK_MENUS found in '//RCPFILE)
C&ENDIF
      ENDIF
      CALL EZRSET
  999 RETURN
      END
