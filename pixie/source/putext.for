      SUBROUTINE PUTEXT(ID,LINE,NLINES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a bordered window within the current
C-   viewport with enough space for NLINES of text LINE(i). The
C-   width of the window is determined by LEN(LINE(1)). The window
C-   object must first be created with a call to PUTEXT_CREATE.
C-
C-   A) Use the entry point PUTEXT_CREATE(ID) to create a window object.
C-
C-   B) Use the entry point PUTEXT_SET(ID,OPTION,X) to change the
C-   attributes of the text window object.
C-
C-   C) Use the entry point PUTEXT_DELETE(ID) to delete a window object.
C-
C-   Inputs  : ID       [I]     Window ID
C-             LINE(*)  [C]     Character strings
C-             NLINES   [I]     Number of lines of text
C-
C-   Created  13-MAY-1991   Harrison B. Prosper
C-   Updated  22-MAY-1991   Harrison B. Prosper
C-   Updated  28-MAY-1991   Harrison B. Prosper
C-      Work in virtual viewport-relative coordinates
C-   Updated  01-JUL-1991   Nobuaki Oshima
C-      Reset Screen Params(VUPNT,UPVEC,NORML) after JVSAVE...
C-   Updated  21-OCT-1991   Nobuaki Oshima
C-      Add JVSAVE/JVLOAD and reset Screen Params(VUPNT,UPVEC,NORML) 
C-      after JVSAVE in ENTRY PUTEXT_SET.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID
      CHARACTER*(*) LINE(*)
      CHARACTER*(*) OPTION
      INTEGER NLINES
      REAL    A(*)
      INTEGER ISCR
C----------------------------------------------------------------------
C
C ****  PARAMETERS
C
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C
      INTEGER LEFT,TOP
      PARAMETER( LEFT = 1 )
      PARAMETER( TOP  = 3 )
C
C ****  Screen Object Buffers
C
      INTEGER SCREEN_ID
      REAL    S_X0_BASE(MAXSCREENS), S_Y0_BASE(MAXSCREENS)
      REAL    S_Y0_MAX(MAXSCREENS)
      LOGICAL S_FIRST(MAXSCREENS)
C
C ****  Window Object Buffers
C
      INTEGER NOBJECTS
      REAL    W_XSIZE(MAXOBJ), W_YSIZE(MAXOBJ), W_SPACE(MAXOBJ)
      REAL    W_X0(MAXOBJ),W_Y0(MAXOBJ), W_XWID(MAXOBJ), W_YWID(MAXOBJ)
      INTEGER W_SCREEN_ID(MAXOBJ),W_LINK(0:MAXOBJ),NEXT_ID,LAST_ID
      LOGICAL W_DEFAULT_POS(MAXOBJ)
      LOGICAL W_PRESENT(MAXOBJ)
      CHARACTER*3 W_TEXT_COLOR(MAXOBJ)
      CHARACTER*3 W_EDGE_COLOR(MAXOBJ)
      CHARACTER*3 W_FILL_COLOR(MAXOBJ)
C----------------------------------------------------------------------
      REAL    BUFFER(85)
      REAL    X0, Y0, XV1, YV1, XV2, YV2, XP(5),YP(5),ZP(5)
      REAL    X, Y, Z, X1,Y1,Z1, X2,Y2,Z2, DX1,DY1
      REAL    XSIZ, YSIZ, XSIZE, YSIZE, SPACE,DX,DY,XWID,YWID,YSTEP
      REAL    VXMIN, VXMAX, VYMIN, VYMAX
      REAL    XMIN,   XMAX,  YMIN,  YMAX
      REAL    X0_BASE, Y0_BASE, Y0_MAX,  DVX,DVY,DWX,DWY
      INTEGER I,L,NROWS, NCOLS,IER,IDS,II,JJ
      LOGICAL DEFAULT_POS, WITHIN, FIRST, CLEAR
      CHARACTER*3 TEXT_COLOR, EDGE_COLOR, FILL_COLOR
      CHARACTER*16 OPT
C----------------------------------------------------------------------
      SAVE W_XSIZE, W_YSIZE, W_SPACE, W_X0, W_Y0, W_XWID, W_YWID
      SAVE W_TEXT_COLOR, W_EDGE_COLOR, W_FILL_COLOR, W_DEFAULT_POS
      SAVE W_SCREEN_ID, W_PRESENT,S_X0_BASE,S_Y0_BASE,S_Y0_MAX,S_FIRST
      SAVE SCREEN_ID, W_LINK, DWX, DWY, DVX, DVY
      SAVE VXMIN, VXMAX, VYMIN, VYMAX
      DATA NOBJECTS/0/
C----------------------------------------------------------------------
C
C ****  Check window-object id
C
      IF ( (ID .LT. 1) .OR. (ID .GT. MAXOBJ) ) THEN
        CALL ERRMSG('BAD_ID','PUTEXT',
     &    'Invalid TEXT window ID','W')
        GOTO 999
      ENDIF
      IF ( .NOT. W_PRESENT(ID) ) THEN
        CALL ERRMSG('NOTFOUND','PUTEXT',
     &    'TEXT window object NOT found','W')
        GOTO 999
      ENDIF
C
C ****  Check widths of viewport in virtual and world coordinates
C
      IF ( (DVX.LE.0.0) .OR. (DVY.LE.0.0) ) THEN
        CALL ERRMSG('BAD_V_SIZE','PUTEXT',
     &    'Invalid viewport size','W')
        GOTO 999
      ENDIF
C
C ****  Load variables
C
      TEXT_COLOR = W_TEXT_COLOR(ID)
      EDGE_COLOR = W_EDGE_COLOR(ID)
      FILL_COLOR = W_FILL_COLOR(ID)
      DEFAULT_POS= W_DEFAULT_POS(ID)
      XSIZE      = W_XSIZE(ID)
      YSIZE      = W_YSIZE(ID)
      SPACE      = W_SPACE(ID)
      SCREEN_ID  = W_SCREEN_ID(ID)
C
C ****  Compute spacing in X and Y
C
      DX = SPACE*XSIZE
      DY = SPACE*YSIZE
C
C ****  Compute window width and height
C
      NCOLS= LEN(LINE(1))
      NROWS= NLINES
      XWID = DX + NCOLS*XSIZE + DX
      YWID = NROWS*(DY+YSIZE) + DY
C
C ****  Define location of upper left-hand point of window
C
      IF ( DEFAULT_POS ) THEN
        IF ( SCREEN_ID .GT. 0 ) THEN
C
C ****  Load screen object parameters for current screen
C
          FIRST   = S_FIRST  (SCREEN_ID)
          X0_BASE = S_X0_BASE(SCREEN_ID)
          Y0_BASE = S_Y0_BASE(SCREEN_ID)
          Y0_MAX  = S_Y0_MAX (SCREEN_ID)
C
          IF     ( FIRST ) THEN
            FIRST   = .FALSE.
            X0_BASE = 0.0
            Y0_BASE = 0.0
            Y0_MAX  = 0.0
          ELSEIF ( (X0_BASE+XWID) .GT. DVX ) THEN
            X0_BASE = 0.0
            Y0_BASE = Y0_MAX
          ENDIF
C
          X0 = X0_BASE + DX
          Y0 = Y0_BASE + DY + YWID
C
C ****  Update base-point for both X and Y
C
          X0_BASE = X0 + XWID
          IF ( Y0 .GT. Y0_MAX ) THEN
            Y0_MAX = Y0
          ENDIF
C
C ****  Update screen object parameters
C
          S_FIRST  (SCREEN_ID) = FIRST
          S_X0_BASE(SCREEN_ID) = X0_BASE
          S_Y0_BASE(SCREEN_ID) = Y0_BASE
          S_Y0_MAX (SCREEN_ID) = Y0_MAX
        ELSE
          CALL ERRMSG('BAD_S_ID','PUTEXT',
     &      'Invalid Screen ID','W')
          GOTO 999
        ENDIF
C
      ELSE
C
C ****  Use stored position
C
        X0 = W_X0(ID)
        Y0 = W_Y0(ID)
      ENDIF
C
C ****  Update window object parameters
C
      W_X0(ID) = X0
      W_Y0(ID) = Y0
      W_XWID(ID) = XWID
      W_YWID(ID) = YWID
      W_DEFAULT_POS(ID) = .FALSE.
C
C ****  Compute coordinates of diagonal points
C
      X1 = X0
      Y1 = Y0
      X2 = X0 + XWID
      Y2 = Y0 - YWID
C
C ****  Compute starting position of text
C
      DX1 = DX
      DY1 = DY
      X   = X1 + DX1
      Y   = Y1 - DY1
C
      XSIZ = XSIZE
      YSIZ = YSIZE
      YSTEP= DY1 + YSIZ
C
C ****  Save current viewing parameters
C
      CALL JVSAVE(BUFFER)
C-
C--- RESET SCREEN PARAMETERS
      CALL JRIGHT( .TRUE. )
      CALL JVUPNT(0., 0., 0.)
      CALL JUPVEC(0., 1., 0.)
      CALL JNORML(0., 0.,-1.)
C
C ****  Map to virtual (view-port relative) coordinates
C
      CALL JWINDO(0.0,DVX,0.0,DVY)
C
C ****  Open segment
C
      CALL JOPEN
C
C ****  Fill window area
C
      IF ( FILL_COLOR(1:1) .NE. ' ' ) THEN
        CALL PXCOLFILL(FILL_COLOR)
        CALL JRECT(X1,Y1,X2,Y2)
      ENDIF
C
C ****  Draw edge around window
C
      CALL PXCOLR(EDGE_COLOR)
      CALL JRECT(X1,Y1,X2,Y2)
C
C ****  Setup text attributes
C
      CALL PXCOLR(TEXT_COLOR)
      CALL JSIZE(XSIZ, YSIZ)
      CALL JJUST(LEFT, TOP)
C
C ****  Draw text
C
      DO I = 1, NROWS
        CALL JMOVE(X,Y)
        CALL J2STRG(LINE(I))
        Y = Y - YSTEP
      ENDDO
      CALL JCLOSE
C
C ****  Restore viewing parameters
C
      CALL JVLOAD(BUFFER)
      RETURN
C
C ****  Clear screen-parameters
C
      ENTRY PUTEXT_CLEAR(ISCR)
      IF ( (ISCR.GE.0) .AND. (ISCR.LE.MAXSCREENS) ) THEN
        S_FIRST(ISCR) = .TRUE.
      ENDIF
      RETURN
C
C ****  Define current screen
C
      ENTRY PUTEXT_SCREEN(ISCR)
      SCREEN_ID = ISCR
C
C ****  Get Size of current screen in virtual coordinates
C
      CALL J4RGET(2,VXMIN,VXMAX,VYMIN,VYMAX)
C
C ****  Compute width
C
      DVX = VXMAX-VXMIN
      DVY = VYMAX-VYMIN
      IF ( (DVX.LE.0.0) .OR. (DVY.LE.0.0) ) THEN
        GOTO 999
      ENDIF
      RETURN
C
C ****  ENTRY POINT to alter attributes of text window
C
      ENTRY PUTEXT_SET(ID,OPTION,A)
      IF ( (ID .LT. 1) .OR. (ID .GT. MAXOBJ) ) THEN
        CALL ERRMSG('BAD_ID','PUTEXT',
     &    'Invalid TEXT window ID','W')
        GOTO 999
      ENDIF
      IF ( .NOT. W_PRESENT(ID) ) THEN
        CALL ERRMSG('NOTFOUND','PUTEXT',
     &    'TEXT window object NOT found','W')
        GOTO 999
      ENDIF
C
      OPT = OPTION(1:LEN(OPTION))
C
      CALL UPCASE(OPT,OPT)
      I = INDEX(OPT,'/') + 1
      IF     ((OPT(1:1) .EQ. 'D') .OR.
     &        (OPT(1:1) .EQ. ' '   )) THEN
C
        W_XSIZE(ID)      = 0.016
        W_YSIZE(ID)      = 0.018
        W_SPACE(ID)      = 0.500
        W_TEXT_COLOR(ID) = 'FOR'
        W_EDGE_COLOR(ID) = 'FOR'
        W_FILL_COLOR(ID) = '   '
C
      ELSEIF ( OPT(I:I) .EQ. 'T' ) THEN
        W_TEXT_COLOR(ID) = OPT(1:3)
      ELSEIF ( OPT(I:I) .EQ. 'F' ) THEN
        W_FILL_COLOR(ID) = OPT(1:3)
      ELSEIF ( OPT(I:I) .EQ. 'E' ) THEN
        W_EDGE_COLOR(ID) = OPT(1:3)
C
      ELSEIF ( OPT(1:1) .EQ. 'A' ) THEN
        W_DEFAULT_POS(ID) = .TRUE.
      ELSEIF ( OPT(1:1) .EQ. 'P' ) THEN
C
C ****  Convert to virtual (viewport-relative) coordinates
C
        IF ( (DVX.LE.0.0) .OR. (DVY.LE.0.0) ) THEN
          GOTO 999
        ENDIF
C
C ****  Save current viewing parameters
C
        CALL JVSAVE(BUFFER)
C-
C--- RESET SCREEN PARAMETERS
        CALL JRIGHT( .TRUE. )
        CALL JVUPNT(0., 0., 0.)
        CALL JUPVEC(0., 1., 0.)
        CALL JNORML(0., 0.,-1.)
C-
        CALL JCONWV(A(1),A(2),A(3),X0,Y0)
        W_X0(ID) = X0-VXMIN
        W_Y0(ID) = Y0-VYMIN
C
C ****  Restore viewing parameters
C
        CALL JVLOAD(BUFFER)
C
        W_DEFAULT_POS(ID) = .FALSE.
C
      ELSEIF ( OPT(1:1) .EQ. 'X' ) THEN
        W_XSIZE(ID) = A(1)
      ELSEIF ( OPT(1:1) .EQ. 'Y' ) THEN
        W_YSIZE(ID) = A(1)
      ELSEIF ( OPT(1:1) .EQ. 'S' ) THEN
        W_SPACE(ID) = A(1)
      ENDIF
C
      RETURN
C
C ****  Create instance of window
C
      ENTRY PUTEXT_CREATE(ID)
      IF ( NOBJECTS .GE. MAXOBJ ) THEN
        CALL ERRMSG('MAXWIN','PUTEXT',
     &    'Maximum number of windows exceeded','W')
        GOTO 999
      ENDIF
C
C ****  Find next available ID
C
      IF ( NOBJECTS .LE. 0 ) THEN
        NOBJECTS  = 1
        ID        = 1
        W_LINK(0) = ID
        W_LINK(1) = 0
      ELSE
        NOBJECTS = NOBJECTS + 1
        ID       = 0
        LAST_ID  = 0
        JJ = 0
        DO WHILE ( JJ .LT. NOBJECTS )
          JJ = JJ + 1
          NEXT_ID = W_LINK(LAST_ID)               ! Find NEXT ID
          IF ( ((LAST_ID-NEXT_ID) .GT. 1) .OR.    ! Gap
     &         ( NEXT_ID .EQ. 0 ) ) THEN          ! Or end-of-list
            ID = LAST_ID + 1              ! Increment ID
            W_LINK(LAST_ID) = ID          ! Add to list
            W_LINK(ID)      = NEXT_ID
            JJ = NOBJECTS                 ! Exit
          ENDIF
          LAST_ID = NEXT_ID
        ENDDO
      ENDIF
C
      IF ( ID .GT. 0 ) THEN
        W_PRESENT(ID) = .TRUE.
C
C ****  Default values
C
        W_XSIZE(ID)      = 0.016
        W_YSIZE(ID)      = 0.018
        W_SPACE(ID)      = 0.750
        W_TEXT_COLOR(ID) = 'FOR'
        W_EDGE_COLOR(ID) = 'FOR'
        W_FILL_COLOR(ID) = '   '
        W_DEFAULT_POS(ID)= .TRUE.
        W_SCREEN_ID(ID)  = SCREEN_ID
      ELSE
        CALL STAMSG('  Problem creating text window',.FALSE.)
      ENDIF
      RETURN
C
C ****  Delete instance of window
C
      ENTRY PUTEXT_DELETE(ID)
      IF ( NOBJECTS .LE. 0 ) THEN
        CALL ERRMSG('NOWIN','PUTEXT',
     &    'No windows exist to be deleted','W')
        GOTO 999
      ENDIF
      IF ( (ID .LT. 1) .OR. (ID .GT. MAXOBJ) ) THEN
        CALL ERRMSG('BAD_ID','PUTEXT',
     &    'Invalid TEXT window ID','W')
        GOTO 999
      ENDIF
      IF ( .NOT. W_PRESENT(ID) ) THEN
        CALL ERRMSG('NOTFOUND','PUTEXT',
     &    'TEXT window object NOT found','W')
        GOTO 999
      ENDIF
C
      W_PRESENT(ID) = .FALSE.
C
C ****  Adjust linked list
C
      LAST_ID = 0
      JJ = 0
      DO WHILE ( JJ .LT. NOBJECTS )
        JJ = JJ + 1
        NEXT_ID = W_LINK(LAST_ID)       ! Find NEXT ID
        IF ( ID .EQ. NEXT_ID ) THEN     ! Check if same as ID
          NEXT_ID = W_LINK(ID)          ! Find next ID from ID
          W_LINK(LAST_ID) = NEXT_ID
          W_LINK(ID) = 0
          JJ = NOBJECTS                 ! Exit
        ENDIF
        LAST_ID = NEXT_ID
      ENDDO
C
      IF ( NOBJECTS .GT. 0 ) THEN
        NOBJECTS = NOBJECTS - 1
      ENDIF
      ID = 0
      RETURN
C
C ****  Find window containing given point
C
      ENTRY PUTEXT_FIND(A,ID)
C
C ****  Check viewport dimensions
C
      IF ( (DVX.LE.0.0) .OR. (DVY.LE.0.0) ) THEN
        GOTO 999
      ENDIF
C
C ****  Convert to virtual (viewport-relative) coordinates
C
      CALL JCONWV(A(1),A(2),A(3),X0,Y0)
      X0 = X0-VXMIN
      Y0 = Y0-VYMIN
C
C ****  Loop over all windows bound to current screen
C
      ID = 0
      IF ( SCREEN_ID .GT. 0 ) THEN
        II = 0
        DO JJ = 1, NOBJECTS
          II = W_LINK(II)
          IF ( SCREEN_ID .EQ. W_SCREEN_ID(II) ) THEN
            XMIN = W_X0(II)
            XMAX = XMIN + W_XWID(II)
            YMAX = W_Y0(II)
            YMIN = YMAX - W_YWID(II)
C
            IF ( XMIN .LE. X0 ) THEN
              IF ( XMAX .GE. X0 ) THEN
                IF ( YMIN .LE. Y0 ) THEN
                  IF ( YMAX .GE. Y0 ) THEN
                    ID = II
                    GOTO 999
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
  999 RETURN
      END
