      SUBROUTINE PU_SETUP_TOP_MENU(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : From the list of PIXIE user packages, each
C-   associated with an RCP bank having the name
C-   
C-      'PX_' + package-name + '_RCP',
C-      
C-   create the top menu PIXIE with one menu item for each package.
C-   If only one package has been linked in force a descent to that
C-   package's menu. If an RCP file contains the switch ACTIVE then
C-   add the menu item only if the switch ACTIVE is TRUE.   
C-
C-   Inputs  : None
C-   Outputs : OK       [L]     TRUE if successful
C-   Controls: None
C-
C-   Created   9-SEP-1990   Harrison B. Prosper
C-   Updated  14-JAN-1992   Lupe Howell  Display the menu names form 
C-                          'DISPLAYED_ITEM_NAME' if found
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
C
      INCLUDE 'D0$INC:PXCOMK.INC'
C
      INTEGER I,J,IER
      LOGICAL ACTIVE,NAME
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM = 'PIXIE' )
C----------------------------------------------------------------------
      CHARACTER*40 MENU_PACKAGE(MAXPAK)
      LOGICAL FIRST,OKAY
      DATA FIRST/.TRUE./
      SAVE FIRST,OKAY
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        OKAY  = .TRUE.
C
C ****  Now setup top menu
C
        CALL MENNEW(MENNAM)
C
C ****  Get list of PIXIE packages
C
        PACKAGE(1) = 'PX_*'             ! Get all 'PX_*' RCP files 
        CALL EZDIR(PACKAGE,NPACKAGE)
        IF ( NPACKAGE .GT. 0 ) THEN
          DO I = 1, NPACKAGE
C
C ****  Check ACTIVE switch and the DISPLAYED_ITEM_NAME parameter
C
            CALL EZPICK(PACKAGE(I))
            CALL EZGET('ACTIVE',ACTIVE,IER)
            IF ( IER .NE. 0 ) THEN
              ACTIVE = .TRUE.
            ENDIF
            MENU_PACKAGE(I) = ' '
            CALL EZGETC('DISPLAYED_ITEM_NAME',1,40,MENU_PACKAGE(I),IER)
            IF ( IER .NE. 0 ) THEN
              NAME = .FALSE.
            ELSE
              NAME = .TRUE.
            ENDIF
            CALL EZRSET
C
C ****  Get package name from name of RCP bank
C ****  and set the menu name if DISPLAYED_ITEM_NAME not found
C
            PACKAGE(I) = PACKAGE(I)(4:)
            J = INDEX(PACKAGE(I),'_RCP')
            IF ( J .GT. 0 ) THEN
              PACKAGE(I) = PACKAGE(I)(1:J-1)
            ENDIF
            IF ( .NOT. NAME ) THEN   ! Use package name for menu if no menu name
              MENU_PACKAGE(I) = PACKAGE(I)
            ENDIF
C
C ****  Add menu item for each package if package is to be active
C
            IF ( ACTIVE ) THEN
              CALL MENADD(MENNAM,.TRUE.,MENU_PACKAGE(I),PACKAGE(I),
     &        'Display the menu for the package '//PACKAGE(I))
            ENDIF
          ENDDO
        ELSE
          CALL ERRMSG('PIXIE','PU_TOP_MENU',
     &      ' No RCP banks PX_* in memory','W')
          OKAY = .FALSE.
        ENDIF
      ENDIF
C
C ****  Create a status window
C
      CALL SETSTA(3)
      CALL SPLSTA
C
      OK = OKAY
  999 RETURN
      END
