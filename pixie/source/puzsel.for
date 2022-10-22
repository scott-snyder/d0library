      SUBROUTINE PUZSEL( FILNAM, LON, ISEL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the Zebra and local PIXIE RCP files for the
C-                         user to select one and returns its name and 
C-                         length ( of the name ).
C-
C-   Inputs  : none
C-   Outputs : FILNAM [C*] : full file specification of the selected file
C-             LON    [I]  : Length of the file name. 0 means no name.
C-
C-   Created 30-OCT-1991   Lupe Howell   Based on the old PUZSEL 
C-                                       by Oliver Calloy
C-   Updated  16-DEC-1991   Lupe Howell  In "NEITHER' option the file entered
C-            returned Modify call to PX_DISPLAY_ITEMS
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-   Updated   2-NOV-1992   Lupe Howell  Add call to DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INTEGER LON
C----------------------------------------------------------------------
      INTEGER ISEL,LSEL,NFND1,NFND2,I,II,JJ,CDIRL,IFILE,ICOM
      REAL    RTCODE, SYS$SETDDIR      
      CHARACTER*32 NAMFND1(39),NAMFND2(39),MENLIS1(39),MENLIS2(39)
      character*32 rem1(39), rem2(39)
      CHARACTER*80 CURDIR,TOP
      CHARACTER*132 STRING
C
      INTEGER LDIR
      PARAMETER( LDIR = 9 )
      CHARACTER*(LDIR) DIRNAM
      DATA DIRNAM / 'D0$PIXIE:' /
C
      INTEGER LEXT
      PARAMETER( LEXT = 4 )
      CHARACTER*(LEXT) EXTNAM
      DATA EXTNAM / '.RCP' /
C
      CHARACTER*60 MENULIST(3),MENU_REM(3)
      DATA MENULIST /
     &     'FILES AVAILABLE IN LIBRARY',
     &     'USER DEFAULT AREA FILES',
     &     'NO FILE FROM NEITHER LIST'/
      DATA MENU_REM/
     &     ' Choose between files available in PIXIE library',
     &     ' Choose between files in you current area',
     &     ' Write the area and file name to pick'/
C----------------------------------------------------------------------
C
C ****  Getting a list of all PX_*.RCP files in D0$PIXIE and
C ****  in the current directory
C
      STRING = DIRNAM//'PX_*'//EXTNAM
      CALL WORD(STRING,I,II,JJ)
      CALL PXFILLST(STRING(1:JJ), 39, NFND1, NAMFND1 )
      RTCODE = SYS$SETDDIR('0',CDIRL,CURDIR)
      STRING = CURDIR//'PX_*'//EXTNAM
      CALL WORD(STRING,I,II,JJ)
      CALL PXFILLST(STRING(1:JJ), 39, NFND2, NAMFND2 )

      IF( ( NFND1 .EQ. 0 ).AND.( NFND2 .EQ. 0 ) ) GOTO 999
C
C ****  Filling menu display array 
C
      DO 10 I = 1 , NFND1
        MENLIS1(I) = NAMFND1(I)
        rem1(i) = ''
   10 CONTINUE
      DO 15 I = 1 , NFND2
        MENLIS2(I) = NAMFND2(I)
        rem2(i) = ''
   15 CONTINUE
C
C ****  Select the which RCP files to display
C ****  locals or pixie linrary
C
      LON = 0
      CALL DISPLAY_ITEMS(3,MENULIST,MENU_REM,'SELECT FILES',ICOM)
      IF( ICOM .NE. 0 ) THEN
C
C ****  Selecting from ligrary
C
        IF( MENULIST(ICOM) .EQ. 'FILES AVAILABLE IN LIBRARY' ) THEN
          CALL DISPLAY_ITEMS
     &      (NFND1,MENLIS1,rem1,'LIBRARY FILES',IFILE)
          IF( IFILE .NE. 0 ) THEN
            LSEL   = INDEX( NAMFND1(IFILE),' ') - 1
            FILNAM = DIRNAM//NAMFND1(IFILE)(1:LSEL)
            LON    = LSEL + LDIR
          ENDIF
C
C ****  Selecting form local area
C
        ELSEIF( MENULIST(ICOM) .EQ. 'USER DEFAULT AREA FILES' ) THEN
          TOP = 'FILES IN AREA '//CURDIR(1:CDIRL)
          CALL DISPLAY_ITEMS
     &      (NFND2,MENLIS2,rem2,TOP,IFILE)
          IF( IFILE .NE. 0 ) THEN
            LSEL   = INDEX( NAMFND2(IFILE),' ') - 1
            FILNAM = CURDIR(1:CDIRL)//NAMFND2(IFILE)(1:LSEL)
            LON    = LSEL + CDIRL
          ENDIF
C
C ****  No file from neither list
C
        ELSEIF( MENULIST(ICOM) .EQ. 'NO FILE FROM NEITHER LIST' ) THEN
          CALL GETPAR(1,'Enter the name of the file to be read :',
     &      'U',STRING)
          CALL WORD(STRING,JJ,II,LON)
          IF( LON .NE. 0 ) THEN
            I = INDEX(STRING,'.')
            IF( I .EQ. 0 ) THEN
              STRING = STRING(1:LON)//'.RCP'
              LON = LON + 4
            ENDIF
            FILNAM = STRING(1:LON)
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
