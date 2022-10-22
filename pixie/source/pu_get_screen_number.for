      SUBROUTINE PU_GET_SCREEN_NUMBER
     &  (SCREEN_COMMAND,ISCREEN,NSCREEN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For given screen command return the
C-   screen number. This is just the ordinal value of the screen
C-   block within the current PXSCREEN array.
C-
C-   Inputs  : SCREEN_COMMAND   [C*]    Screen (View) Command
C-   Outputs : ISCREEN          [I]     Screen Number
C-             NSCREEN          [I]     Total number of screens (views)
C-                                      in current PXSCREENS array
C-             IER              [I]     0 -- OK
C-                              If IER = 2 the requested screen was not
C-                              found.
C-   Controls: None
C-
C-   Created   6-MAY-1991   Lupe Howell
C-   Updated  14-MAY-1991   Lupe Howell
C-   Updated  13-JUN-1991   Lupe Howell  Get all the screen names every
C-      time since even thou it might be the  same bank the screen name
C-      might it changed !
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) SCREEN_COMMAND
      INTEGER ISCREEN,IER
C
      INTEGER MAXNUM                    ! Should go into PIXIE.DEF
      PARAMETER( MAXNUM = 50 )
C
      INTEGER NSCREEN,I,J,K,L
      INTEGER SCRETYPE(MAXNUM),IVAL(MAXNUM),TOTAL_SCREENS
      CHARACTER*32 SCRENAME(MAXNUM),SCREREM(MAXNUM)
      CHARACTER*32 OLD_BANK,CURRENT_BANK,COMMAND
C
      SAVE OLD_BANK, SCRENAME,TOTAL_SCREENS
C----------------------------------------------------------------------
      ISCREEN = 0
C
C ****  Get ALL the screen names 
C
      CALL EZTELL(CURRENT_BANK,L)
C
C
C ****  Get number of screens in PXSCREEN array
C
      CALL EZ_GET_ARRAY('PXSCREEN','NSCREEN',
     &    1,TOTAL_SCREENS,SCRENAME,SCRETYPE,SCREREM,IER)

      IF ( IER .NE. 0 ) THEN
        CALL INTMSG
     &    (' PU_GET_SCREEN_NUMBER: Problem accessing PXSCREEN in ')
        CALL INTMSG(' RCP bank '//CURRENT_BANK)
        GOTO 999
      ENDIF
C
C ****  Get ALL screen commands
C
      CALL EZ_GET_ARRAY('PXSCREEN','NAME',
     &     TOTAL_SCREENS,IVAL(1),SCRENAME,SCRETYPE,SCREREM,IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG
     &    (' PU_GET_SCREEN_INDEX: Problem accessing PXSCREEN in ')
        CALL INTMSG(' RCP bank '//CURRENT_BANK)
        GOTO 999
      ENDIF
C
C ****  Search for the requested screen
C
      COMMAND = SCREEN_COMMAND(1:LEN(SCREEN_COMMAND))
      IER = 2
      NSCREEN = TOTAL_SCREENS
      DO I = 1, NSCREEN
        IF ( COMMAND .EQ. SCRENAME(I) ) THEN
          ISCREEN = I
          IER = 0
          GOTO 800                   ! Found screen command
        ENDIF
      ENDDO

  800 IF ( IER .NE. 0 ) THEN
        CALL ERRMSG(' SCREEN NOT FOUND',
     &    'PU_GET_SCREEN_NUMBER',' Could not find screen '//COMMAND,
     &    'W')
      ENDIF
C
  999 RETURN
      END
