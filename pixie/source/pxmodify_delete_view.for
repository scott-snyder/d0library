      SUBROUTINE PXMODIFY_DELETE_VIEW(RCPFILE,SCREEN,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Deletes the current view.
C-
C-   Inputs  : RCPFILE [C*]: RCP file name
C-             SCREEN  [C*]: Name of the screen to be deleted
C-             NPORT   [I ]: Number of ports
C-
C-   Outputs : NPORT   [I ]: If a viewport of a combiend view is deleted
C-                           The number of total porst is decreased
C-
C-   Created  31-MAY-1991   Lupe Howell
C-   Updated   9-AUG-1991   Lupe Howell  The number of parameter in the call to
C-                        PXMODIFY_DELETE_ACTIONS increased.
C-   Updated   1-NOV-1991   Lupe Howell  For now no combine view can be delete
C-   Updated  20-DEC-1991   Lupe Howell  Delete from PX_*_RCP ONLY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      INTEGER SCREEN_ELEMENTS
      PARAMETER( SCREEN_ELEMENTS = 21 )
C
      INTEGER VIEWPORT,COMBINED_ELEMENTS,I,J,K,ISCREEN,NSCREEN,IER
      INTEGER PARTY(MAXCPAR),ACTNUM,PTR,IMENU,ISUBM,IACTION
      REAL    PARVAL(MAXCPAR)
      CHARACTER*32 PACKAGE,ACTION(MAXCACT),PARNAMES(MAXCPAR)
      LOGICAL COMBINED
C----------------------------------------------------------------------
      CALL SWORDS(SCREEN,I,J,K)
C
C ****  Select port that is going to be modify if the
C ****  number of viewports is greather than 1
C
      IF ( NPORT .GT. 1 ) THEN
        CALL PU_SELECT_PORT(NPORT,VIEWPORT)
        COMBINED = .TRUE.
      ELSEIF ( SCREEN(J:J) .EQ. '%') THEN
        CALL INTMSG(' There is only 1 viewport in this view')
        CALL INTMSG(' You CAN NOT DELETE it !')
        GOTO 999
      ELSE
        COMBINED = .FALSE.
        VIEWPORT = NPORT
        CALL INTMSG(' There is only 1 viewport in this view')
        CALL INTMSG(' You CAN NOT DELETE it !')
        GOTO 999
      ENDIF

      IF ( COMBINED ) THEN
C
C ****  If more than one port remove the combined elements from
C ****  the combined array
C
        PTR = 1
C
C ****  Get the total number of elements in the picked
C ****  port.
C
        CALL PX_GET_ACTION
     &    (SCREEN,VIEWPORT,PACKAGE,ACTION,ACTNUM,PARNAMES,
     &    PARVAL,PARTY,COMBINED_ELEMENTS,PTR,IER)
C
C ****  Remove the number of elements in the combined view
C ****  in the PX_*_RCP file
C
        IF ( IER .EQ. 0 ) THEN
          CALL EZ_REMOVE_ELEMENT
     &       (SCREEN,'%PACKAGE',VIEWPORT,COMBINED_ELEMENTS,IER)
        ENDIF
      ENDIF
C
C ****  Redrawn the viewports after deletion
C
      CALL JCLEAR
      CALL JFRAME
      CALL PU_DRAW_SCREEN(SCREEN,RCPFILE,NPORT)

  999 RETURN
      END
