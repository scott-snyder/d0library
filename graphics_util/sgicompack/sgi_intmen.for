      SUBROUTINE INTMEN(ATITLE,MNUNAM,DSPNAM)
C------------------------------------------------------------------------
C-
C-   Purpose and Methods : Setup interrupt menu and return to main task.
C-      DSPNAM gets ignored.  Router does the job.
C-
C-   Inputs  : ATITLE [C*]: Title for top of display.
C-             MNUNAM [C*]: Name of menu level to use
C-             DSPNAM [C*]: Name of dispatch routine to use (EXTERNAL)
C-
C-   Outputs : None
C-
C-  Created  XX-XXX-XX   Mike Shupe
C-  Updated  09-DEC-1992 Lupe Howell Clean up and fix interrupt
C-
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) ATITLE
      CHARACTER*(*) MNUNAM
      EXTERNAL DSPNAM
     
C------------------------------------------------------------------------
      INCLUDE   'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
C
      INTEGER IMENU,I,NI,ITASK,MAKEMENI,CNT
      LOGICAL ACTIVE,INTERRUPT,INTR_ON
      DATA INTR_ON /.FALSE./
C
      SAVE INTR_ON
C------------------------------------------------------------------------
      IF ( INTR_ON ) THEN
	CALL ITASK_QUEUE(INTERRUPT)
	IF ( INTERRUPT ) THEN
          CALL DELE_DISP(-1)
          INTR_ON = .FALSE.
	  CALL DSPNAM
        ENDIF
        GOTO 999
      ENDIF
      IRNOW=0
      IMENU=0
C
C *** Searching for the index number of the name
C *** name of the menu level to use
C
      ACTIVE = .TRUE.
      I   = 1
      CNT = 0
      DO WHILE ( ACTIVE )
        IF( FNAME(I) .EQ. MNUNAM) THEN 
          IMENU=I
          ACTIVE = .FALSE. 
        ENDIF
        I = I + 1
        ACTIVE = ( I .LE. NMENU ) .AND. ( ACTIVE )
      ENDDO
      IF ( IMENU .EQ. 0 ) THEN
	CALL ERRMSG('SGI_COMPACK','INTMEN',
     &    'Menu not found in FNAME','W')
        GOTO 999
      ENDIF
C
C *** Searching for match with the router name
C
 5001 CONTINUE
      ACTIVE = .TRUE.
      I   = 1
      CNT = 0
      DO WHILE ( ACTIVE )
        IF ( DSNAME(I) .NE. ' ') THEN 
          CNT = CNT + 1
          IF( DSNAME(I) .EQ. MNUNAM ) THEN 
            IRNOW  = I
            ACTIVE = .FALSE.
	  ENDIF
        ENDIF
        I = I + 1 
        ACTIVE = ( I .LE. NMENMX ) .AND. ( ACTIVE )
 5020 ENDDO
C
C *** If the dispatch routine was not found add it 
C *** Displays an error message if out of range
C
      IF ( IRNOW .EQ. 0 ) THEN
        CNT = CNT + 1
	IF ( CNT .LE. NMENMX ) THEN
	  DSNAME(CNT) = MNUNAM
	  IRNOW = CNT
        ELSE
	  CALL ERRMSG('SGI_COMPACK','INTMEN',
     &      'Routing array DSNAME out of range','W')
          GOTO 999
	ENDIF
      ENDIF

C
C *** Go make and do the menu
C
      NI=NITEMS(IMENU)
      JMENU=IMENU                       
      ITASK=MAKEMENI(NI,ITEMNM(1,IMENU),FNAME(IMENU))
C
C *** Set Interrupt Flag
C
      ASTFLG  = .TRUE.
      INTR_ON = .TRUE.
 999  RETURN
      END
