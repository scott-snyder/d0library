      SUBROUTINE PXMODIFY_VIEW(RCPFILE,SCREEN_NAME,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays all the screens available in the current
C-   RCP file allowing the user to pick the screen that is going to be modified.
C-   Then the modifycations done are saved.
C-
C-   Inputs  : RCPFILE    [C* ]: Name of the current RCP file
C-             SCREEN_NAME [C*]: Name of the screen to be modify
C-             NPORT       [I ]: Total number of viewports in the current
C-                               screen
C-
C-   Outputs : None
C-
C-   Created  13-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up 
C-   Updated  20-DEC-1991   Lupe Howell  Modify only PX_*_RCP files 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN_NAME
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
CC      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      CHARACTER*40 ARRAY
      CHARACTER*32 CURRENT_ACTION,PACKAGE(MAXCVIEW),ACTION(MAXCVIEW)
      CHARACTER*32 CURRENT_PACKAGE
C
      INTEGER TOTAL_SCREENS,OUTNUM,ISCREEN,NSCREEN,ICHAR,IER
      INTEGER I,II,J,JJ,K,KK,LL,LEN,IMENU,ISUBMENU,VIEWPORT
      INTEGER IDX(MAXCVIEW),UNIQUEPORT,TOTAL_PORT,MODIFY_VIEWS
C
      REAL    XMINPRT,XMAXPRT,YMINPRT,YMAXPRT,XV1,XV2,YV1,YV2,XX
      REAL    XMIN(MAXCVIEW),XMAX(MAXCVIEW),YMIN(MAXCVIEW)
      REAL    YMAX(MAXCVIEW)
C
      LOGICAL FOUND,ACTION_FOUND,EZERROR,ALL_VIEWS
C----------------------------------------------------------------------
C
C ****  Select port that is going to be modify if the 
C ****  number of viewports is greather than 1
C
      IF ( NPORT .GT. 1 ) THEN
        CALL PU_SELECT_PORT(NPORT,VIEWPORT)
      ELSE
        VIEWPORT = NPORT
      ENDIF
C
C ****  Modify the port selected
C
      CALL JVPORT( XCVPRT-XMVPRT, XCVPRT+XMVPRT,
     &               YCVPRT-YMVPRT, YCVPRT+YMVPRT )
      CALL JWINDO( -1., 1., -1., 1. )
      CALL INTMSG(' Defines the new viewport by clicking on two '//
     &              'opposite corners')
      CALL PULOCA( XMINPRT, YMINPRT, ICHAR, XV1, YV1 )
      CALL JOPEN
      CALL JCOLOR(0)
      CALL JMOVE( XV1, -1.)
      CALL JDRAW( XV1,  1.)
      CALL JMOVE( -1., YV1 )
      CALL JDRAW(  1., YV1 )
      CALL JCLOSE
      CALL PULOCA( XMAXPRT, YMAXPRT, ICHAR, XV2, YV2 )
      IF( XV2 .LT. XV1 ) THEN
        XX  = XV1
        XV1 = XV2
        XV2 = XX
      ENDIF
      IF ( YV2 .LT. YV1 ) THEN
        XX  = YV1
        YV1 = YV2
        YV2 = XX
      ENDIF
      IF( XV1 .EQ. XV2 .OR. XV1 .LT. -1. .OR. XV2 .GT. 1.) THEN
        CALL INTMSG(' Bad viewport definition in X, try again')
        GOTO 999
      ENDIF
      IF( YV1 .EQ. YV2 .OR. YV1 .LT. -1. .OR. YV2 .GT. 1.) THEN
        CALL INTMSG(' Bad viewport definition in Y, try again')
        GOTO 999
      ENDIF
C
C ****  Setting the new port values in the RCP banks
C
      I  = VIEWPORT
      JJ = VIEWPORT
      MODIFY_VIEWS = 1
      ALL_VIEWS = .FALSE.
      CALL SWORDS(SCREEN_NAME,K,J,LEN)
      LL = INDEX(RCPFILE,'PX_')
      KK = INDEX(RCPFILE,'_RCP')
      CURRENT_PACKAGE = RCPFILE(LL+3:KK-1)
C
C ****  If the view is combined determine how many ports it has 
C
      IF ( SCREEN_NAME(LEN:LEN) .EQ. '%' ) THEN
        CALL PU_GET_UNIQUE_PORTS(CURRENT_PACKAGE,SCREEN_NAME,
     &  XMIN,XMAX,YMIN,YMAX,PACKAGE,ACTION,IDX,UNIQUEPORT,TOTAL_PORT)
        ARRAY = SCREEN_NAME(1:LEN)
        IF( (NPORT .EQ. 1) .AND. (TOTAL_PORT .GT. 1) ) THEN
          MODIFY_VIEWS = TOTAL_PORT 
          ALL_VIEWS = .TRUE.
        ELSE
          IDX(1) = 1
        ENDIF
      ELSE
        ARRAY = 'PXSCREEN'
        CALL PU_GET_SCREEN_NUMBER(SCREEN_NAME,ISCREEN,NSCREEN,
     &          IER)
        CALL PU_GOTO_SCREEN(ISCREEN,IDX(1))
      ENDIF
      DO II = 1, MODIFY_VIEWS
        CALL EZ_SET_ELEMENT(ARRAY,'VPORTXMIN',IDX(II),I,XV1,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'VPORTXMAX',IDX(II),I,XV2,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'VPORTYMIN',IDX(II),I,YV1,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'VPORTYMAX',IDX(II),I,YV2,IER)
      ENDDO
C
C ****  Draw the modifed viewport of the chosen screen
C
      CALL JCLEAR
      CALL JFRAME
      CALL PU_DRAW_SCREEN(SCREEN_NAME,RCPFILE,NPORT)

  999 RETURN
      END
