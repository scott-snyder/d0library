      SUBROUTINE SYSMENU(MENNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up the PIXIE system command menu items.
C-    The menu items will be appended to the menu items of a PIXIE user
C-    package.
C-
C-   Inputs  : MENNAM [C*] : Menu name.
C-
C-   Outputs : None
C-
C-   Created  25-JUL-1990   Lupe Howell
C-   Updated   9-SEP-1990   Harrison B. Prosper
C-      Add Default Next Event item
C-   Updated  15-NOV-1990   Harrison B. Prosper
C-      Add Set Path and New Display
C-   Updated  28-NOV-1990   Harrison B. Prosper
C-      Add Superimpose option and COMPACK menu
C-   Updated  20-FEB-1991   Harrison B. Prosper
C-      Add OTHER OPTIONS
C-   Updated  12-APR-1991   Lupe Howell
C-      Add Start and Stop buttons
C-   Updated  13-MAY-1991   Harrison B. Prosper
C-      Add PICK; change some names
C-   Updated  28-MAY-1991   Harrison B. Prosper
C-      Add ROTATE
C-   Updated  12-JUN-1991   Harrison B. Prosper
C-      Go back to name NAME EVENT
C-   Updated  25-SEP-1991   Lupe Howell, Harrioson B. Prosper
C-      Other Option menu deleted and Start Sequence added to main
C-   Updated   2-JAN-1992   Lupe Howell  The help in Start sequence updated
C-   Updated   5-JUN-1992   Nobuaki Oshima - Add 'Dump Bank' menu.
C-   Updated  15-DEC-1992   Vipin Bhatnagar - Add 'Modify Scan' menu.
C-   Updated  25-FEB-1993   Nobuaki Oshima 
C-      Checking 'DO_SCAN' parameter for 'Modify Scan' menu.
C-   Updated  15-APR-1993   Nobuaki Oshima - Added 'Set path to MDST.'
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      CHARACTER*(*) MENNAM
      LOGICAL FIRST,EZERROR,LSCAN
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Define menu for SET PATH
C
        CALL MENNEW('SETPATH')
        CALL MENADD('SETPATH',.FALSE.,
     &    'RECO',
     &    'RECO',
     &    '   Set path to RECO.'//
     &    ' '   )
        CALL MENADD('SETPATH',.FALSE.,
     &    'GEAN',
     &    'GEAN',
     &    '   Set path to GEAN.'//
     &    ' '   )
        CALL MENADD('SETPATH',.FALSE.,
     &    'FILT',
     &    'FILT',
     &    '   Set path to FILT.'//
     &    ' '   )
        CALL MENADD('SETPATH',.FALSE.,
     &    'MDST',
     &    'MDST',
     &    '   Set path to MDST.'//
     &    ' '   )
        CALL MENADD('SETPATH',.FALSE.,
     &    'Default',
     &    'DEFAULT',
     &    '   Set to default path.'//
     &    ' '   )
        CALL MENADD('SETPATH',.FALSE.,
     &    'Show',
     &    'SHOW',
     &    '   Show the currently set path.'//
     &    ' '   )
        CALL MENNEW('STOP_SEQUENCE')
        CALL MENADD('STOP_SEQUENCE',.FALSE.,
     &    'Stop Sequential Display',
     &    'STOP SEQUENTIAL DISPLAY',
     &    '  Stops the sequential Display.'//
     &    ' '  )
      ENDIF
C
      CALL MENADD(MENNAM,.FALSE.,
     &  'NEXT Event',
     &  'NEXT EVENT',
     &  '   Go to next event.                              '//
     &  'If the display is in automatic display mode then t'//
     &  'he same view will be displayed after the next even'//
     &  't has been read in.                               '//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'GO TO Event',
     &  'GO TO EVENT',
     &  '   Go to a particular event number. '//
     &  'If the display is in automatic display mode then t'//
     &  'he same view will be displayed after the correct even'//
     &  't has been read in.                               '//
     &  ' '   )
C
C----Checking DO_SCAN flag here for activating 'Modify Scan' menu
C
      CALL EZPICK('PX_SYSTEM_RCP')
      IF ( .NOT.EZERROR(IER) ) THEN
        CALL PUGETV('DO_SCAN', LSCAN)
        IF ( LSCAN ) THEN
          CALL MENADD(MENNAM,.FALSE.,
     &      'Modify Scan',
     &      'MODIFY SCAN',
     &      '   Opens a Scanning Sheet and update it.'//
     &      ' '   )
        ENDIF
        CALL EZRSET
      ENDIF
      CALL MENADD(MENNAM,.FALSE.,
     &  'ZOOM',
     &  'ZOOM A VIEW',
     &  '   Apply a zoom to the current display. The area to be'//
     &  ' zoomed is defined by supplying two points with the mouse.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'ROTATE',
     &  'ROTATE',
     &  '   Apply a rotation to the current display, if appropriate.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'PICK',
     &  'PICK',
     &  '   Pick a point in the current display, and, if appropriate,'//
     &  ' execute a PICK action associated with the display.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'HARDCOPY',
     &  'HARDCOPY',
     &  '   Produce a hardcopy of the current display.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'MODIFY',
     &  'MODIFY PARAMETERS',
     &  '   A sub menu is shown offering a list of parameters '//
     &  'to modify for the current package.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Change Display Mode',
     &  'CHANGE DISPLAY MODE',
     &  '   Toggle between Automatic display of the same display'//
     &  ' and Manual selection of a display.                '//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Superimpose(On/Off)',
     &  'SUPERIMPOSE',
     &  '   Toggle between Superimposing and NOT superimposing '//
     &  'subsequent views.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Set Path',
     &  'SET PATH',
     &  '   Set the ZEBRA data path in /ZEBCOM/. '//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Dump Bank',
     &  'DUMP BANK',
     &  '   Dump ZEBRA Banks by DBANK utility. '//
     &  ' '   )

      CALL MENADD(MENNAM,.FALSE.,
     &  'Dump Event',
     &  'DUMP EVENT',
     &  '   Set the flag DUMP_THIS_EVENT to TRUE.'//
     &  '   The action performed is framework-dependent.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Write Event',
     &  'WRITE EVENT',
     &  '   Set the flag WRITE_THIS_EVENT to TRUE.'//
     &  '   The action performed is framework-dependent.'//
     &  ' '   )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Start Sequential Display',
     &  'START SEQUENTIAL DISPLAY',
     &  '   Start the automatic display of a view(s) with a delay'//
     &  'interval between  each  view and skipping events between'//
     &  'displays.  A list of the available sequence display '//
     &  'arrays names will be displayed for selection.   If  there'//
     &  'is  only  one  sequence display array in the current package'//
     &  'it will be automatically selected to start the sequence.'//
     &  ' ' )
      CALL MENADD(MENNAM,.FALSE.,
     &  'Add/Remove Views',
     &  'REMOVE',
     &
     &
     &'  Displays the actions available in the current combined view'//
     &  ' and lets the user set the actions he/she wants to be '//
     &  'displayed'//
     &  ' ')
      CALL MENADD(MENNAM,.FALSE.,
     &  'COMPACK System Menu',
     &  'MENCTR',
     &  '   Display COMPACK system menu.'//
     &  ' '   )
C
  999 RETURN
      END
