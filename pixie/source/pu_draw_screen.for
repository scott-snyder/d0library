      SUBROUTINE PU_DRAW_SCREEN( SCREEN_NAME,CURRENT_RCPFILE,NPORT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws the viewport of the requested screen on the
C-   graphics window.
C-
C-   Inputs  : SCREEN_NAME[C* ]: Name of the screen to be interpret
C-             CURRENT_RCPFILE [C*]: Current RCP file being modify
C-
C-   Outputs : NPORT: Number of unique ports in the view requested
C-
C-   Created  10-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Cross was taken out and the
C-        name of the actions menus are displayed
C-   Updated  23-JUL-1991   Lupe Howell  Update to hadle combined displays 
C-   and varible font size 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) SCREEN_NAME
      CHARACTER*(*) CURRENT_RCPFILE
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
C
      INTEGER IDX,NSCREEN,IER,ISCREEN,I,J,K,KK,L,LEN,TOTAL_PORT
C
      REAL    XVP1,XVP2,YVP1,YVP2,STEP_LINE,RLINE
      REAL    XMINPRT(MAXCVIEW),XMAXPRT(MAXCVIEW),YMINPRT(MAXCVIEW)
      REAL    YMAXPRT(MAXCVIEW),OLDXMIN,OLDXMAX,OLDYMIN,OLDYMAX
      CHARACTER*32 PACKAGE(MAXCVIEW),CURRENT_SCREEN(MAXCVIEW)
      CHARACTER*32 ACTION(MAXCVIEW),CURRENT_PACKAGE
      CHARACTER*1 NUMBER
C
      SAVE XMINPRT,XMAXPRT,YMINPRT,YMAXPRT
C
      INTEGER VIEWPORTNUM,ICHAR
      REAL    XV1,YV1
      LOGICAL FOUND,PU_SET_RCP_BANK
C
      REAL    XSIZE,YSIZE
C
C----------------------------------------------------------------------
      CALL SWORDS(SCREEN_NAME,I,J,LEN)
      OLDXMIN = 0.0
      OLDXMAX = 0.0
      OLDYMIN = 0.0
      OLDYMAX = 0.0
C
C ****  Getting unique viewports
C
      I = INDEX(CURRENT_RCPFILE,'PX_')
      J = INDEX(CURRENT_RCPFILE,'_RCP')
      CURRENT_PACKAGE = CURRENT_RCPFILE(I+3:J-1)
      CALL PU_GET_UNIQUE_PORTS(CURRENT_PACKAGE,SCREEN_NAME,
     &  XMINPRT,XMAXPRT,YMINPRT,
     &  YMAXPRT,PACKAGE,ACTION,IDX,NPORT,TOTAL_PORT)
C
      DO I = 1, NPORT
C
C ****  Viewports should be drawn only if the port values are different
C
        IF ( (XMINPRT(I) .NE. OLDXMIN) .OR. (XMAXPRT(I) .NE. OLDXMAX)
     &   .OR.(YMINPRT(I) .NE. OLDYMIN) .OR. (YMAXPRT(I) .NE. OLDYMAX)
     &   )
     &     THEN
C
C ****  Calculating viewport values
C
          XVP1 = XMINPRT(I) * XMVPRT + XCVPRT
          XVP2 = XMAXPRT(I) * XMVPRT + XCVPRT
          YVP1 = YMINPRT(I) * YMVPRT + YCVPRT
          YVP2 = YMAXPRT(I) * YMVPRT + YCVPRT
C
C ****  Setting up the x and Y size of the action names using 
C ****  viewport values (0.5 %)
C
          XSIZE = 0.02 * (XVP2 - XVP1)
          YSIZE = 0.02 * (YVP2 - YVP1)
C
C ****  Setting graphics window parameters
C
          CALL JVPORT( XVP1, XVP2, YVP1, YVP2 )
          CALL JWINDO( 0., 1., 0., 1.)
          CALL JOPEN
          CALL PXCOLR('FOR')
          CALL JCOLOR(0)
          CALL JSIZE( 1., 1. )
          CALL JRECT( 0., 0., 1., 1. )
          CALL JRECT( .25, .25, .75, .75 )
C
C ****  Draw lines around second rectangle, and print the
C ****  name of each unique viewport
C
          CALL JMOVE( 0., 1. )
          CALL JDRAW( .25, .75 )
          CALL JMOVE( 1., 1. )
          CALL JDRAW( .75, .75 )
          CALL JMOVE(  1.,  0. )
          CALL JDRAW( .75, .25 )
          CALL JMOVE( 0., 0. )
          CALL JDRAW( .25, .25 )
          CALL JSIZE( XSIZE,YSIZE )
          CALL JJUST( 1, 1 )
C
C ****  Displys the name(s) of the actions in the view requested
C ****  If the view has only one viewport but it is a combined 
C ****  the names of the actions will be displayed after each other
C
          IF( ( NPORT .EQ. 1 ) .AND. ( TOTAL_PORT .GT. 1 ))THEN
            RLINE = 0.5/TOTAL_PORT
            STEP_LINE = .75 - RLINE
            DO KK = 1, TOTAL_PORT
              CALL JMOVE( 0.375,STEP_LINE )
              CALL sWORDs(ACTION(KK),J,K,L)
              CALL J3STRG(ACTION(KK)(J:K))
              STEP_LINE = STEP_LINE - RLINE
            ENDDO
C
C ****  If the view has only one action it will be displayed 
C ****  by itself
C
          ELSE
            CALL JMOVE( 0.375, 0.5 )
            CALL SWORDS(ACTION(I),J,K,L)
            CALL J3STRG(ACTION(I)(J:K))
          ENDIF
          CALL JCLOSE
          OLDXMIN = XMINPRT(I)
          OLDXMAX = XMAXPRT(I)
          OLDYMIN = YMINPRT(I)
          OLDYMAX = YMAXPRT(I)
        ENDIF
      ENDDO
  999 RETURN
C
      ENTRY PU_SELECT_PORT(NPORT,VIEWPORTNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Let the user select with the mouse the viewport
C-   is going to modify
C-
C-   Inputs  : NPORT       [I ]: Number of ports in the view
C-
C-   Outputs : VIEWPORTNUM [I ]: Number of the vieport selected
C-
C-   Created  29-MAY-1991   LUPE HOWELL
C-
C----------------------------------------------------------------------
      CALL JVPORT( XCVPRT-XMVPRT, XCVPRT+XMVPRT,
     &               YCVPRT-YMVPRT, YCVPRT+YMVPRT )
      CALL JWINDO( -1., 1., -1., 1. )
      CALL INTMSG(' Select the Viewport that you want to Modify')
      CALL PULOCA( XMINPRT, YMINPRT, ICHAR, XV1, YV1 )
C
C ****  Determine the viewport selected
C
      FOUND = .FALSE.
      I = 1
      DO WHILE ( ( .NOT. FOUND ) .AND. ( I .LE. NPORT ) )
        IF( ((XV1 .GE. XMINPRT(I)) .AND. (XV1 .LE. XMAXPRT(I))).AND.
     &      ((YV1 .GE. YMINPRT(I)) .AND. (YV1 .LE. YMAXPRT(I)))) THEN
          FOUND = .TRUE.
          VIEWPORTNUM = I
        ENDIF
        I = I + 1
      ENDDO
      END
