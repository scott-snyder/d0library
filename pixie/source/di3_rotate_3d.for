      SUBROUTINE DI3_ROTATE_3D(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize screen parameters to rotate
C-   the current view(s).
C-
C-   Inputs  : COMMAND  [C*]    Screen Command
C-   Outputs : COMMAND  [C*]    Screen Command
C-   Controls:
C-
C-   Created  13-MAY-1992   Lupe Howell and Harrison B. Prosper
C-       Based on old PX_ROTATE written by Nobu oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      CHARACTER*(*) COMMAND
      REAL    VPORTXMIN(MAXCVIEW),VPORTXMAX(MAXCVIEW)
      REAL    VPORTYMIN(MAXCVIEW),VPORTYMAX(MAXCVIEW)
      CHARACTER*32 PACKAGE(MAXCVIEW),ACTION_COMMAND(MAXCVIEW)
      CHARACTER*32 CURRENT_PACKAGE
      INTEGER IDX(MAXCVIEW),NPORT
C
      REAL    CAM(3),UPVEC(3),X(3),XWIN,YWIN,ZWIN,XV,YV
      INTEGER IER,IDDX,NUMBER_SCREENS,I,BUTID,ID
      CHARACTER*32 PACK,COMM
      LOGICAL PU_SET_RCP_BANK, VIEW3D,FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL PUTEXT_CREATE(BUTID)
        FIRST = .FALSE.
      ENDIF
C
C ****  Select viewport in which too draw the rotation cylinder
C
      CALL PU_SELECT_VIEWPORT(XWIN,YWIN,ZWIN,PACK,COMM,IDDX,IER)
      IF ( IER .EQ. -1 ) THEN
        CALL FLGSET('ROTATING',.FALSE.)
        FIRST = .TRUE.
        COMMAND = ' '
        GOTO 999
      ENDIF
C
C ****  Get rotation parameters if view is 3-D.
C
      IF ( PU_SET_RCP_BANK(PACK) ) THEN
C
        CALL PU_GET_SCREEN_PARAM(IDDX,'VIEW3D',VIEW3D,IER)
        IF ( .NOT. VIEW3D ) THEN
          CALL INTMSG(' Cannot ROTATE this view you bonehead!')
          GOTO 999
        ENDIF
C
C ****  Reset RCP bank
C
        CALL PU_RESET_RCP_BANK
      ELSE
        CALL INTMSG(' Cannot find package '//PACK)
        GOTO 999
      ENDIF
C-
C--- Create QUIT button
C-
      CALL PUTEXT_SET(BUTID,'AUTO',0)      ! Auto-position button
      CALL PUTEXT_SET(BUTID,'RED/E',0)
      CALL PUTEXT(BUTID,'QUIT',1)
C---
      CALL PUROTATE(CAM,UPVEC,X)
C-
C--- Find a QUIT button for exit
C-
      CALL PUTEXT_FIND(X,ID)
      IF ( ID .EQ. BUTID ) THEN
        CALL FLGSET('ROTATING',.FALSE.)
        CALL PUTEXT_DELETE(ID)
        FIRST = .TRUE.
        COMMAND = ' '
        GOTO 999
      ENDIF
C
C ****  Convert the window coordinates of selected point
C ****  to virtual coordinates
C
      CALL JCONWV( X(1), X(2), X(3), XV, YV )
C
C ****  Check command type
C
      IF ( INDEX(COMMAND,'%') .GT. 0 ) THEN
        CALL FLGSET('COMBINED_MODE',.TRUE.)
      ENDIF
C
C ****  Get viewports which contain selected point
C
      CALL PU_ACTIVE_PACKAGE(CURRENT_PACKAGE)
      CALL PU_GET_VIEWPORT(CURRENT_PACKAGE,COMMAND,XV,XV,YV,YV,
     &                     VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX,
     &                     PACKAGE,ACTION_COMMAND,IDX,
     &                     NUMBER_SCREENS)
      IF ( NUMBER_SCREENS .LE. 0 ) GOTO 999
C
C ****  Loop over viewports (screens)
C
      DO I = 1 , NUMBER_SCREENS
C
C ****  Pick correct RCP bank
C
        IF ( PU_SET_RCP_BANK(PACKAGE(I)) ) THEN
C
C ****  Modify window rotation parameters
C
          CALL PU_MODIFY_ROTATE(IDX(I),CAM,UPVEC)
C
C ****  Reset RCP bank
C
          CALL PU_RESET_RCP_BANK
        ENDIF
      ENDDO
C
C ****  Set the ROTATE Flag
C
      CALL FLGSET('ROTATING',.TRUE.)
  999 RETURN
      END
