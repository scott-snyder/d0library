      SUBROUTINE MENUEX(ATITLE,MNUNAM,COMOUT)
C----------------------------------------------------------------------------
C-   Purpose and Methods : Set up for and control a menu level.
C-                         Called from MENUDO and MENCTR to avoid recursive
C-                         calling of routines.
C-
C-   Inputs  : ATITLE [C*]: Title for top of display
C-             MNUNAM [C*]: Name of menu level to use
C-
C-   Outputs : COMOUT [C*]: Unique command identifier to returned to dispatch
C-                          loop
C-
C-   Created XX-XXX-XXXX Mike Shupe
C-   Updated 07-FEB-1993 Lupe Howell Clean up implement help
C-
C---------------------------------------------------------------------------
      CHARACTER*(*) ATITLE,MNUNAM,COMOUT
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
C
      INTEGER I,ITASK
C-------------------------------------------------------------------------
C
C *** Search for the menu requested to see if it 
C *** has being declared
C
      IMENU=0
      DO 5000 I=1,NMENU
        IF(FNAME(I).NE.MNUNAM) GO TO 5000
        IMENU=I
        GOTO 5001
 5000 CONTINUE
C
C *** If no match found in the menu name exit
C
      TYPE 555,MNUNAM
  555 FORMAT(' MENUEX--NO MATCH WITH MENU NAME:',A40)
      COMOUT='EXIT'                            
      GO TO 999
C
 5001 CONTINUE
C
      NI=NITEMS(IMENU)
C
C *** Go make and do the menu
C
      MENULEV = IMENU
      ITASK=MAKEMENU(NI,ITEMNM(1,IMENU),FNAME(IMENU))
C
      IF ( ITASK .GT. 0 ) THEN
        COMOUT = ITEMDS(ITASK,IMENU)
      ELSEIF ( ITASK .EQ. 0 ) THEN
	COMOUT = 'EXIT'
      ENDIF
 999  RETURN
      END
