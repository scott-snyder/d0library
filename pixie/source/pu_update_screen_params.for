      SUBROUTINE PU_UPDATE_SCREEN_PARAMS(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Updates all transformation and windowing
C-        screen parameters of the current viewport from DI-3000
C-        current settings.
C-   NOTE:  THIS VERSION IS FOR SINGLE VIEWPORTS ONLY.
C-
C-   Modified  3-SEP-1992   Nobuaki Oshima 
C-              CAM point calculation was changed. 
C-              It was CAM(J) = VWPNT(J) + 1000.*VNORM(J)
C-   Modified 22-JUL-1992   Nobuaki Oshima 
C-              Fix "CALL J3RGET/J4RGET" problem on DI3000.
C-   Created   1-JUL-1992   Michael A. Shupe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) COMMAND
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INTEGER JDX,IER,I,J
      REAL CAM(3),UPVEC(3),VNORM(3),VWPNT(3),WINDO(4)
      REAL VPORTXMIN(MAXCVIEW),VPORTXMAX(MAXCVIEW)
      REAL VPORTYMIN(MAXCVIEW),VPORTYMAX(MAXCVIEW)
      CHARACTER*32 PACKAGE(MAXCVIEW),ACTION_COMMAND(MAXCVIEW)
      CHARACTER*32 CURRENT_PACKAGE
      INTEGER IDX(MAXCVIEW)
      REAL XV,YV
      INTEGER NUMBER_SCREENS
      LOGICAL PU_SET_RCP_BANK
C----------------------------------------------------------------------
C
C ****  Check command type
C
      IF ( INDEX(COMMAND,'%') .GT. 0 ) THEN
        CALL FLGSET('COMBINED_MODE',.TRUE.)
      ENDIF
C
C **** Get view and window quantities from DI-3000
C
      CALL J3RGET(7,VWPNT(1),VWPNT(2),VWPNT(3))
      CALL J3RGET(8,VNORM(1),VNORM(2),VNORM(3))
      CALL J3RGET(9,UPVEC(1),UPVEC(2),UPVEC(3))
      CALL J4RGET(1,WINDO(1),WINDO(2),WINDO(3),WINDO(4))
C
C ****  Get viewports which contain ORIGIN.
C
      XV=0.
      YV=0.
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
C ****  Modify screen parameters
C
          JDX=IDX(I)
          CALL PU_SET_SCREEN_PARAM(JDX,'VIEWREFX',VWPNT(1),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'VIEWREFY',VWPNT(2),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'VIEWREFZ',VWPNT(3),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'UPVECX',UPVEC(1),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'UPVECY',UPVEC(2),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'UPVECZ',UPVEC(3),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'WINDOWXMIN',WINDO(1),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'WINDOWXMAX',WINDO(2),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'WINDOWYMIN',WINDO(3),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'WINDOWYMAX',WINDO(4),IER)
C
          DO J=1,3
            CAM(J) = VWPNT(J) - VNORM(J)
          ENDDO
          CALL PU_SET_SCREEN_PARAM(JDX,'CAMX',CAM(1),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'CAMY',CAM(2),IER)
          CALL PU_SET_SCREEN_PARAM(JDX,'CAMZ',CAM(3),IER)

          IF ( IER .NE. 0 ) THEN
            CALL INTMSG(' PU_UPDATE_SCREEN_PARAMS: Could NOT Set'//
     &        'screen parameters ')
          ENDIF
C
C ****  Reset RCP bank
C
          CALL PU_RESET_RCP_BANK
        ENDIF
      ENDDO
C
  999 RETURN
      END
