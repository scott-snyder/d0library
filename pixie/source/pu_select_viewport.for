      FUNCTION PU_SELECT_VIEWPORT(XOUT,YOUT,ZOUT,PACKAGE,COMMAND,IDX,
     &  IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put the echo position at the center of
C-               WINDOW coordinates. Wait for user input(cursor movement).
C-               Return the WINDOW position, current COMMAND and its
C-               INDEX.
C-
C-   Outputs   XOUT   [F] : Window coordinate of the selected point
C-             YOUT   [F] : Window coordinate of the selected point
C-             ZOUT   [F] : Window coordinate of the selected point
C-          PACKAGE   [C] : Package containing command
C-          COMMAND   [C] : Command associated with the selected viewport
C-              IDX   [I] : Index of Command
C-              IER   [I] : Error code (0 is okay)
C-   Controls:
C-
C-   Entry Points:      PU_PICKED()             TRUE if PICK successful
C-                      PU_CLEAR_PICK           Clear internal pick flag
C-                      PU_GET_PICKV(V)         Return viewport coordinates
C-                      PU_GET_PICKW(W)         Return window coordinates
C-                      PU_GET_PICKP(V)         Return PIXIE coordinates
C-                      PU_DRAW_PICK(COLOR,SIZE) Draw picked point
C-
C-   Created  13-DEC-1990   Nobuaki Oshima
C-   Updated  14-MAY-1991   Harrison B. Prosper
C-      Add entry points and make in function (for PU_PICKED)
C-   Updated  23-MAY-1991   Harrison B. Prosper
C-      Correct conversions
C-   Updated  18-JUN-1991   Nobuaki Oshima, Harrison B. Prosper
C-      Check y-coordinate for exit
C-      Add package argument
C-   Modified  6-JUL-1991   Nobuaki Oshima ( Use PICSEG for ILIST(1). )
C-   Modified  8-NOV-1991   Nobuaki Oshima ( Model Change for E&S. )
C-   Updated  14-MAY-1992   Lupe Howell, Harrison B. Prosper
C-            Removed Rlevel check for CLEAR_3D escape call.
C-   Modified  3-SEP-1992   Nobuaki Oshima
C-      Handle 'HARDWARE_ROTATE' mode for non_DI3000 devices.
C-   Modified  2-DEC-1992   Nobuaki Oshima
C-      Removed 'HARDWARE_ROTATE' mode part.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL PU_SELECT_VIEWPORT, PU_DRAW_PICK, PU_GET_PICKP
      LOGICAL PU_CLEAR_PICK, PU_PICKED, PU_GET_PICKV, PU_GET_PICKW
C
      REAL    XOUT, YOUT, ZOUT
      CHARACTER*(*) PACKAGE, COMMAND
      CHARACTER*(*) COLOR
      INTEGER BUTID,IDX,IER
      REAL    FRAC, YVPTMAX
C----------------------------------------------------------------------
      REAL    VCOOR(2),WCOOR(3)
      LOGICAL PICK_OK
      INTEGER ICH
      REAL    XV, YV, ZV, XV1, YV1,WX,WY,WZ
      REAL    BOXSIZE, X1,Y1,Z1,X2,Y2
      REAL    VXMIN,VXMAX,VYMIN,VYMAX
      REAL    WXMIN,WXMAX,WYMIN,WYMAX
      CHARACTER*3 COL
      SAVE PICK_OK, XV, YV, XV1, YV1, WX, WY, WZ
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
C----------------------------------------------------------------------
      IER = 0
      PICK_OK = .FALSE.
C
C ****  Set viewing port
C
      CALL JVPORT( XCVPRT-XMVPRT, XCVPRT+XMVPRT,
     &               YCVPRT-YMVPRT, YCVPRT+YMVPRT )
C
C ****  Map 2 x 2 square to the entire screen
C
      CALL JWINDO( -1., 1., -1., 1. )
      CALL JVUPNT(0.,0.,0.)
      CALL JNORML(0.,0.,-1.)
C
C ****  Activate cursor
C
      CALL JOPEN
      CALL PULOCA( 0., 0., ICH, XV, YV)   ! XV & YV are in WINDOW coordinate
      CALL JCLOSE
C-
C--- Convert from (-1.,1.) X (-1,1) space to virtual coordinates
C
      CALL JCONWV(XV, YV, 0., XV1, YV1)
C-
C--- Check y coordinate
C-
      YVPTMAX = YCVPRT + YMVPRT
      IF (YV1 .GT. YVPTMAX) THEN
        IER = -1
        GO TO 999
      ENDIF
C
C ****  Select viewport containing point.
C
      CALL PUSLVP( XV1, YV1,
     &    PACKAGE(1:LEN(PACKAGE)),COMMAND(1:LEN(COMMAND)),IDX,IER)
      IF ( IER .NE. 0 )   GOTO 999
C
C ****  Convert from virtual coordinates to current
C ****  window coordinates.
C
      CALL JCONVW(XV1, YV1, XOUT, YOUT, ZOUT)
C
C ****  Remember world coordinates
C
      WX = XOUT
      WY = YOUT
      WZ = ZOUT
C-
      PICK_OK = .TRUE.
      RETURN
C
      ENTRY PU_CLEAR_PICK
      PICK_OK = .FALSE.
      RETURN
C
      ENTRY PU_PICKED
      PU_PICKED = PICK_OK
      RETURN
C
      ENTRY PU_GET_PICKV(VCOOR)
      VCOOR(1) = XV1
      VCOOR(2) = YV1
      RETURN
C
      ENTRY PU_GET_PICKP(VCOOR)
      VCOOR(1) = XV
      VCOOR(2) = YV
      RETURN
C
      ENTRY PU_GET_PICKW(WCOOR)
C
      WCOOR(1) = WX
      WCOOR(2) = WY
      WCOOR(3) = WZ
      RETURN
C
C ****  Draw box around point.
C
      ENTRY PU_DRAW_PICK(COLOR,FRAC)
      CALL PU_GET_VIEWPORT_SIZE
     &      (VXMIN,VXMAX,VYMIN,VYMAX,WXMIN,WXMAX,WYMIN,WYMAX)
C
C ****  Define rectangle
C
      BOXSIZE = FRAC*(WXMAX-WXMIN)
      X1 = WX - BOXSIZE
      Y1 = WY + BOXSIZE
      Z1 = WZ
      X2 = WX + BOXSIZE
      Y2 = WY - BOXSIZE
C
      COL = COLOR(1:LEN(COLOR))
      IF ( COL(1:1) .EQ. ' ' ) THEN
        COL = 'FOR'
      ENDIF
C
      CALL JOPEN
      CALL PXCOLR(COL)
      CALL PURECT(X1,Y1,Z1,X2,Y2)
      CALL JCLOSE
C-
  999 RETURN
      END
