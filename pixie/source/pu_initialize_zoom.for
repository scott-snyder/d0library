      SUBROUTINE PU_INITIALIZE_ZOOM(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   1) Get from the user two points from the display to determine the
C-   ZOOM area.
C-   2) Find which screen zoom is operating on.
C-   3) Calculate the new WINDOWxxxx values.
C-
C-   Use point PU_MODIFY_ZOOM to modify the appropriate screen
C-   in the PXSCREEN array.
C-
C-   Inputs  : COMMAND  [C*]    Command associated with ZOOM
C-
C-   Outputs : COMMAND  [C*]    Command associated with ZOOM
C-
C-   Created  18-OCT-1990   Lupe Howell, Harrison B. Prosper
C-   Updated  15-NOV-1990   Harrison B. Prosper
C-      Call routine PU_GET_CORNERS
C-   Updated   5-DEC-1990   Harrison B. Prosper
C-      Call routine PU_GET_VIEWPORT
C-   Updated  14-JAN-1991   Harrison B. Prosper
C-      Changed calling sequence in pu_modify_zoom
C-   Updated   7-MAY-1991   Harrison B. Prosper
C-      Changed calling sequence in pu_modify_zoom; tidy up
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
C
      INTEGER MAXPORT
      PARAMETER( MAXPORT  = 20 )
C
      REAL    XCOORD1, YCOORD1, XCOORD2, YCOORD2
      REAL    XMIN,XMAX,YMIN,YMAX
      REAL    VPORTXMIN(MAXPORT),VPORTXMAX(MAXPORT)
      REAL    VPORTYMIN(MAXPORT),VPORTYMAX(MAXPORT)
      INTEGER IDX(MAXPORT)
      CHARACTER*32 CURRENT_PACKAGE(MAXPORT),ACTION_COMMAND(MAXPORT)
C
      INTEGER I,J,II,JJ,KK,LL,IER,NUMBER_SCREENS
      LOGICAL ACTIVE,PU_SET_RCP_BANK
C----------------------------------------------------------------------
C
C ****  Get size of zoom area from user
C
      I = 0
      CALL OUTMSG('1')
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        I = I + 1
        CALL INTMSG
     &    (' Please use the MOUSE to input TWO POINTS defining')
        CALL INTMSG(
     &    ' the area to be ZOOMED')
C
        CALL PU_GET_CORNERS(XCOORD1,YCOORD1,XCOORD2,YCOORD2,IER)
C
        IF ( IER .EQ. 0 ) THEN
          ACTIVE = .FALSE.
        ELSE
          IF     ( I .EQ. 1 ) THEN
            CALL INTMSG(' Tee Hee, bad points, please try again!')
          ELSEIF ( I .EQ. 2 ) THEN
            CALL INTMSG(' What''s wrong with your eyes?')
          ELSE
            CALL INTMSG(' Goodbye BOZO!')
            GOTO 999
          ENDIF
        ENDIF
      ENDDO
C
C ****  Wait for a second or so
C
      CALL WAITIT(1.0)
C
      XMIN = MIN(XCOORD1,XCOORD2)
      XMAX = MAX(XCOORD1,XCOORD2)
      YMIN = MIN(YCOORD1,YCOORD2)
      YMAX = MAX(YCOORD1,YCOORD2)
C
C ****  Check command type
C
      IF ( INDEX(COMMAND,'%') .GT. 0 ) THEN
        CALL FLGSET('COMBINED_MODE',.TRUE.)
      ENDIF
C
C ****  Get viewports which contain box
C
      CALL PU_GET_VIEWPORT(COMMAND,XMIN,XMAX,YMIN,YMAX,
     &                     VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX,
     &                     CURRENT_PACKAGE,ACTION_COMMAND,IDX,
     &                     NUMBER_SCREENS)
      IF ( NUMBER_SCREENS .LE. 0 ) GOTO 999
C
C ****  Loop over viewports (screens)
C
      DO I = 1 , NUMBER_SCREENS
C
C ****  Pick correct RCP bank
C
        IF ( PU_SET_RCP_BANK(CURRENT_PACKAGE(I)) ) THEN
C
C ****  Modify window parameters
C
          CALL PU_MODIFY_ZOOM(IDX(I),XMIN,XMAX,YMIN,YMAX,
     &                        VPORTXMIN(I),VPORTXMAX(I),
     &                        VPORTYMIN(I),VPORTYMAX(I))
C
C ****  Reset RCP bank
C
          CALL PU_RESET_RCP_BANK
        ENDIF
      ENDDO
C
C ****  Set the ZOOM Flag
C
      CALL FLGSET('ZOOMING',.TRUE.)
  999 RETURN
      END
